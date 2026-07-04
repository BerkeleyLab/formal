! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module scalar_2D_test_m
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
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i

  implicit none

  type, extends(test_t) :: scalar_2D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tolerance = 1D-8
  integer, parameter :: space_dimension = 2

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The scalar_2D_t derived type'
  end function

  function results() result(test_results)
   type(scalar_2D_test_t) scalar_2D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = scalar_2D_test%run([ & 
     test_description_t('computing the gradient of a scalar field', usher(check_gradient)) &
   ])
  end function

  pure function stagnation_point_potential(x,y) result(z)
    double precision, intent(in) :: x(:), y(:)
    double precision z(size(x),size(y))
    do concurrent(integer :: j=1:size(y)) default(none) shared(x,y,z)
       z(:,j) = (x**2 - y(j)**2)/2
    end do
  end function

  pure function stagnation_point_velocity(x,y) result(gradient)
    double precision, intent(in) :: x(:), y(:)
    double precision gradient(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(gradient,x,y)
       gradient(i,j,:) = [x(i), - y(j)]
    end do
  end function

  function check_gradient() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_2D_initializer_i), pointer :: scalar_2D_initializer
    procedure(vector_2D_initializer_i), pointer :: expected_gradient_initializer
    integer order

    scalar_2D_initializer => stagnation_point_potential
    expected_gradient_initializer => stagnation_point_velocity
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(scalar_2D => scalar_2D_t(scalar_2D_initializer, order=order, cells=[30,20], x_min=[-10D0,-10D0], x_max=[20D0,10D0]))
        associate( &
          grad_scalar => .grad. scalar_2D &
         ,expected_gradient => vector_2D_t(expected_gradient_initializer, mold=scalar_2D) &
        )
          test_diagnosis = test_diagnosis .also. &
            .all. (grad_scalar%co_located_components() .approximates. expected_gradient%co_located_components() .within. tolerance) &
            // string_t(" for order ") // string_t(order)
        end associate
      end associate
    end do
  end function

end module scalar_2D_test_m