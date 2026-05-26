! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module vector_3D_test_m
  use julienne_m, only : &
     operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,operator(.withinPercentage.) &
    ,passing_test &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,usher
  use formal_m, only : &
     vector_3D_t &
    ,vector_3D_initializer_i &
    ,divergence_3D_t &
    ,divergence_3D_initializer_i

  implicit none

  type, extends(test_t) :: vector_3D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  integer, parameter :: space_dimension = 3
  double precision, parameter :: tolerance = 1D-2

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The vector_3D_t derived type'
  end function

  function results() result(test_results)
   type(vector_3D_test_t) vector_3D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = vector_3D_test%run([ &
     test_description_t('computing the divergence of a vector field', usher(check_divergence)) &
   ])
  end function

  pure function biquadratic(x,y,z) result(vector_field)
    double precision, intent(in) :: x(:), y(:), z(:)
    double precision vector_field(size(x),size(y),size(z),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(x,y,z,vector_field)
      vector_field(i,j,k,:) = [ &
         1 - 2*x(i) + 3*x(i)**2 - x(i)*y(j)/5 + 3*y(j)**2 - 2*y(j) &
        ,1 - 2*x(i) + 3*x(i)**2 - x(i)*y(j)/5 + 3*y(j)**2 - 2*y(j) &
        ,z(k) &
      ]
    end do
  end function

  pure function biquadratic_divergence(x,y,z) result(divergence)
    double precision, intent(in) :: x(:), y(:), z(:)
    double precision divergence(size(x),size(y),size(z))
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(x,y,z,divergence)
      divergence(i,j,k) = &
          (-2 + 6*x(i) - y(j)/5) & ! du/dx
        + (-2 + 6*y(j) - x(i)/5) & ! dv/dy
        + (1D0)                    ! dw/dz
    end do
  end function

  pure function cubic(x,y,z) result(vector_field)
    double precision, intent(in) :: x(:), y(:), z(:)
    double precision vector_field(size(x),size(y),size(z),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(x,y,z,vector_field)
      vector_field(i,j,k,:) = [ &
         1 - 2*x(i) + 3*x(i)**3 - x(i)*y(j)/5 + 3*y(j)**3 - 2*y(j) &
        ,1 - 2*x(i) + 3*x(i)**3 - x(i)*y(j)/5 + 3*y(j)**3 - 2*y(j) &
        ,-z(k) &
      ]
    end do
  end function

  pure function cubic_divergence(x,y,z) result(divergence)
    double precision, intent(in) :: x(:), y(:), z(:)
    double precision divergence(size(x),size(y),size(z))
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(x,y,z,divergence)
      divergence(i,j,k) = &
          (-2 + 9*x(i)**2 - y(j)/5) & ! du/dx
        + (-2 + 9*y(j)**2 - x(i)/5) & ! dv/dx
        + (-1D0)                      ! dw/dz
    end do
  end function

  function check_divergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_3D_initializer_i), pointer :: vector_3D_initializer
    procedure(divergence_3D_initializer_i), pointer :: expected_divergence_initializer
    integer order

    test_diagnosis = passing_test()

    do order = 2, 4, 2
      select case(order)
      case(2)
        vector_3D_initializer => biquadratic
        expected_divergence_initializer => biquadratic_divergence
      case(4)
        vector_3D_initializer => cubic
        expected_divergence_initializer => cubic_divergence
      case default
        error stop "check_divergence(vector_3D_test_m): unsupported order"
      end select
      associate(vector_3D => vector_3D_t(vector_3D_initializer, order=order, cells=[40,20,30], x_min=[0D0,-.5D0,0D0], x_max=[2D0,0D0,3D0]))
        associate(div_vector => .div. vector_3D)
          associate(expected_divergence => divergence_3D_t(expected_divergence_initializer, mold=vector_3D))
            test_diagnosis = test_diagnosis .also. &
              (.all. (div_vector%values() .approximates. expected_divergence%values() .within. tolerance)) &
              // string_t(" for order ") // string_t(order)
          end associate
        end associate
      end associate
    end do
  end function

end module vector_3D_test_m