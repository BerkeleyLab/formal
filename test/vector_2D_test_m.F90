! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module vector_2D_test_m
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
     scalar_2D_t &
    ,scalar_2D_initializer_i &
    ,vector_2D_t &
    ,vector_2D_initializer_i &
    ,divergence_2D_t &
    ,divergence_2D_initializer_i &
    ,x_dir &
    ,y_dir

  implicit none

  type, extends(test_t) :: vector_2D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  integer, parameter :: space_dimension = 2
  real, parameter :: tolerance = 1E-2
  real, parameter :: u_const(*) = [1E0,2E0], v_const(*) = [3E0,4E0], u_dot_v_exact = dot_product(u_const, v_const)

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The vector_2D_t derived type'
  end function

  function results() result(test_results)
   type(vector_2D_test_t) vector_2D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = vector_2D_test%run([ &
      test_description_t('computing the divergence of a vector field', usher(check_divergence)) &
     ,test_description_t('computing the dot product of two vector fields', usher(check_dot_product)) &
     ,test_description_t('computing the product of a vector field and a scalar field', usher(check_vector_scalar_product)) &
   ])
  end function

  pure function biquadratic(x,y) result(z)
    real, intent(in) :: x(:), y(:)
    real z(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(x,y,z)
      z(i,j,:) = [ &
         1 - 2*x(i) + 3*x(i)**2 - x(i)*y(j)/5 + 3*y(j)**2 - 2*y(j) &
        ,1 - 2*x(i) + 3*x(i)**2 - x(i)*y(j)/5 + 3*y(j)**2 - 2*y(j) &
      ]
    end do
  end function

  pure function biquadratic_divergence(x,y) result(divergence)
    real, intent(in) :: x(:), y(:)
    real divergence(size(x),size(y))
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(divergence,x,y)
      divergence(i,j) = (-2 + 6*x(i) - y(j)/5) + (-2 + 6*y(j) - x(i)/5)
    end do
  end function

  pure function cubic(x,y) result(z)
    real, intent(in) :: x(:), y(:)
    real z(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(x,y,z)
      z(i,j,:) = [ &
         1 - 2*x(i) + 3*x(i)**3 - x(i)*y(j)/5 + 3*y(j)**3 - 2*y(j) &
        ,1 - 2*x(i) + 3*x(i)**3 - x(i)*y(j)/5 + 3*y(j)**3 - 2*y(j) &
      ]
    end do
  end function

  pure function cubic_divergence(x,y) result(divergence)
    real, intent(in) :: x(:), y(:)
    real divergence(size(x),size(y))
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(divergence,x,y)
      divergence(i,j) = (-2 + 9*x(i)**2 - y(j)/5) + (-2 + 9*y(j)**2 - x(i)/5)
    end do
  end function

  function check_divergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_2D_initializer_i), pointer :: vector_2D_initializer
    procedure(divergence_2D_initializer_i), pointer :: expected_divergence_initializer
    integer order

    test_diagnosis = passing_test()

    do order = 2, 4, 2
      select case(order)
      case(2)
        vector_2D_initializer => biquadratic
        expected_divergence_initializer => biquadratic_divergence
      case(4)
        vector_2D_initializer => cubic
        expected_divergence_initializer => cubic_divergence
      case default
        error stop "check_divergence(vector_2D_test_m): unsupported order"
      end select
      associate(vector_2D => vector_2D_t(vector_2D_initializer, order=order, cells=[40,30], x_min=[0E0,0E0], x_max=[2E0,1E0]))
        associate(div_vector => .div. vector_2D)
          associate(expected_divergence => divergence_2D_t(expected_divergence_initializer, mold=vector_2D))
            test_diagnosis = test_diagnosis .also. &
              (.all. (div_vector%values() .approximates. expected_divergence%values() .within. tolerance)) &
              // string_t(" for order ") // string_t(order)
          end associate
        end associate
      end associate
    end do
  end function

  pure function u_field(x,y) result(u)
    real, intent(in) :: x(:), y(:)
    real u(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      u(i,j,:) = u_const
    end do
  end function

  pure function v_field(x,y) result(v)
    real, intent(in) :: x(:), y(:)
    real v(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      v(i,j,:) = v_const
    end do
  end function

  function check_dot_product() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_2D_initializer_i), pointer :: u_init, v_init
    integer order

    test_diagnosis = passing_test()

    u_init => u_field
    v_init => v_field

    do order = 2, 4, 2
      associate( &
         u => vector_2D_t(u_init, cells=[10,10], x_min=[0E0,0E0], x_max=[5E0,5E0], order=order) &
        ,v => vector_2D_t(v_init, cells=[10,10], x_min=[0E0,0E0], x_max=[5E0,5E0], order=order) &
      )
        associate(u_dot_v => u .dot. v)
          test_diagnosis = test_diagnosis .also. (.all. (u_dot_v%values() .approximates. u_dot_v_exact .within. 1E-6))
        end associate
      end associate
    end do
  end function

  pure function scalar_field(x,y) result(s)
    real, intent(in) :: x(:), y(:)
    real s(size(x),size(y))
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      s(i,j) = x(i)
    end do
  end function

  pure function vector_field(x,y) result(v)
    real, intent(in) :: x(:), y(:)
    real v(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      v(i,j,:) = y(j)
    end do
  end function

  pure function scalar_vector_product(x,y) result(u)
    real, intent(in) :: x(:), y(:)
    real u(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      u(i,j,:) = x(i)*y(j)
    end do
  end function

  function check_vector_scalar_product() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_2D_initializer_i), pointer :: s_init
    procedure(vector_2D_initializer_i), pointer :: v_init, vs_init
    integer order

    test_diagnosis = passing_test()

    s_init => scalar_field
    v_init => vector_field
    vs_init => scalar_vector_product

    do order = 2, 4, 2
      associate( &
         s => scalar_2D_t(s_init, cells=[10,10], x_min=[0E0,0E0], x_max=[5E0,5E0], order=order) &
        ,v => vector_2D_t(v_init, cells=[10,10], x_min=[0E0,0E0], x_max=[5E0,5E0], order=order) &
        ,vs_expected => vector_2D_t(vs_init, cells=[10,10], x_min=[0E0,0E0], x_max=[5E0,5E0], order=order) &
      )
        associate(vs => v * s)
          test_diagnosis = test_diagnosis .also. (.all. (vs%values(x_dir) .approximates. vs_expected%values(x_dir) .within. 1E-6))
          test_diagnosis = test_diagnosis .also. (.all. (vs%values(y_dir) .approximates. vs_expected%values(y_dir) .within. 1E-6))
        end associate
      end associate
    end do
  end function

end module vector_2D_test_m