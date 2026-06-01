! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module scalar_3D_test_m
  use julienne_m, only : &
     operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,passing_test &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,usher
  use formal_m, only : scalar_3D_t, vector_3D_t, scalar_3D_initializer_i, vector_3D_initializer_i

  implicit none

  type, extends(test_t) :: scalar_3D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tolerance = 1D-2
  integer, parameter :: space_dimension = 3

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The scalar_3D_t derived type'
  end function

  function results() result(test_results)
   type(scalar_3D_test_t) scalar_3D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = scalar_3D_test%run([ &
     test_description_t('computing the gradient of a scalar field', usher(check_gradient)) &
   ])
  end function

  pure function triquadratic(x,y,z) result(f)
    double precision, intent(in) :: x(:), y(:), z(:)
    double precision f(size(x),size(y),size(z))
    do concurrent(integer :: j=1:size(y), k=1:size(z)) default(none) shared(x,y,z,f)
      f(:,j,k) = 1 - 2*x + 3*x**2 - x*y(j)/5 + 3*y(j)**2 - 2*y(j) - 2*z(k)
    end do
  end function

  pure function triquadratic_gradient(x,y,z) result(gradient)
    double precision, intent(in) :: x(:), y(:), z(:)
    double precision gradient(size(x),size(y),size(z),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(gradient,x,y,z)
      gradient(i,j,k,:) = [-2 + 6*x(i) - y(j)/5, -x(i)/5 + 6*y(j) - 2, -2D0]
    end do
  end function

  function check_gradient() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_3D_initializer_i), pointer :: scalar_3D_initializer
    procedure(vector_3D_initializer_i), pointer :: expected_gradient_initializer
    integer order

    scalar_3D_initializer => triquadratic
    expected_gradient_initializer => triquadratic_gradient
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(scalar_3D => scalar_3D_t(scalar_3D_initializer, order=order, cells=[30,20,10], x_min=[0D0,0D0,0D0], x_max=[1D0,1D0,1D0]))
        associate(grad_scalar => .grad. scalar_3D, expected_gradient => vector_3D_t(expected_gradient_initializer, mold=scalar_3D))
          test_diagnosis = test_diagnosis .also. &
            .all. (grad_scalar%values() .approximates. expected_gradient%values() .within. tolerance) &
            // string_t(" for order ") // string_t(order)
        end associate
      end associate
    end do
  end function

end module scalar_3D_test_m