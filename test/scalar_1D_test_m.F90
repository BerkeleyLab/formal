! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module scalar_1D_test_m
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
    ,usher &
    ,csv  
  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i, d_dx, d2_dx2

  implicit none

  type, extends(test_t) :: scalar_1D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tolerance = 1D-11

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The scalar_1D_t derived type'
  end function

  function results() result(test_results)
   type(scalar_1D_test_t) scalar_1D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = scalar_1D_test%run([ & 
      test_description_t('raising a 1D scalar field to a power', usher(check_exponentiation)) &
     ,test_description_t('dividing a 1D scalar field by a constant', usher(check_divison_operator)) &
     ,test_description_t('computing a 1D scalar field derivative at cell centers extended', usher(check_derivative)) &
     ,test_description_t('computing a 1D scalar field second derivative at cell centers extended', usher(check_2nd_derivative)) &
   ])
  end function

  pure function line(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = x
  end function

  function check_exponentiation() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => null()
    integer order

    scalar_1D_initializer => line
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(scalar_1D => scalar_1D_t(scalar_1D_initializer, order=order, cells=10, x_min=0D0, x_max=10D0) )
        associate(cube => scalar_1D**3 )
          test_diagnosis = test_diagnosis .also. .all. &
            (cube%values() .approximates. scalar_1D%values()**3 .within. tolerance) &
            // string_t(" for order ") // string_t(order)
        end associate
      end associate
    end do

  end function

  function check_divison_operator() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => null()
    integer order

    scalar_1D_initializer => line
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(scalar_1D => scalar_1D_t(scalar_1D_initializer, order=order, cells=10, x_min=0D0, x_max=10D0) )
        associate( half => scalar_1D/2 )
          test_diagnosis = test_diagnosis .also. .all. (half%values() .approximates. scalar_1D%values()/2 .within. tolerance) &
                           // string_t(" for order ") // string_t(order)
        end associate
      end associate
    end do

  end function

  pure function parabola(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = 7*x**2 + 3*x + 5D0
  end function

  pure function d_parabola_dx(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = 14*x + 3D0
  end function

  pure function d2_parabola_dx2(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    allocate(y(size(x)))
    y = 14D0
  end function

  function check_derivative() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => null()
    integer order

    scalar_1D_initializer => parabola
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(scalar_1D => scalar_1D_t(scalar_1D_initializer, order=order, cells=10, x_min=0D0, x_max=10D0) )
        associate( d_scalar_1D_dx => d_dx(scalar_1D))
          test_diagnosis = test_diagnosis .also. .all. &
            (d_scalar_1D_dx%values() .approximates. d_parabola_dx(scalar_1D%grid()) .within. tolerance) &
            // string_t(" for order ") // string_t(order)
        end associate
      end associate
    end do

  end function

  function check_2nd_derivative() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => null()
    integer order

    scalar_1D_initializer => parabola
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(scalar_1D => scalar_1D_t(scalar_1D_initializer, order=order, cells=10, x_min=0D0, x_max=10D0) )
        associate(d2_scalar_1D_dx2 => d2_dx2(scalar_1D))
          test_diagnosis = test_diagnosis .also. .all. &
            (d2_scalar_1D_dx2%values() .approximates. d2_parabola_dx2(scalar_1D%grid()) .within. tolerance) &
            // string_t(" for order ") // string_t(order)
        end associate
      end associate
    end do

  end function

end module scalar_1D_test_m
