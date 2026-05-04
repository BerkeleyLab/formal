! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module interpolator_1D_test_m
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
  use formal_m, only : &
     centers_to_faces_1D_t, scalar_1D_t, scalar_1D_initializer_i &
    ,faces_to_centers_1D_t, vector_1D_t, vector_1D_initializer_i

  implicit none

  type, extends(test_t) :: interpolator_1D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tolerance = 1D-11

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'A 1D mimetic interpolator'
  end function

  function results() result(test_results)
   type(interpolator_1D_test_t) interpolator_1D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = interpolator_1D_test%run([ & 
      test_description_t('estimating center values from face values with 2nd- & 4th-order interpolators', usher(check_centers_to_faces)) &
     ,test_description_t('estimating face values from center values with 2nd- & 4th-order interpolators', usher(check_faces_to_centers)) &
   ])
  end function

  pure function line(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = x
  end function

  pure function cubic(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = 7*x**3 + 4*x**2 + x + 2
  end function

  function check_centers_to_faces() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => null()
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => line
    double precision, parameter :: x_min = 0D0, x_max = 20D0
    integer order, cells

    test_diagnosis = passing_test()

    do order = 2, 4, 2

      select case(order)
      case(2)
        cells = 10
        scalar_1D_initializer => line
      case(4)
        cells = 20
        scalar_1D_initializer => cubic
      case default
        error stop "check_centers_to_faces (interpolator_1D_test_m) unsupported order of accuracy"
      end select

      associate( &
         scalar_1D => scalar_1D_t(scalar_1D_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max) &
        ,vector_1D => vector_1D_t(vector_1D_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max) &
        ,interpolator => centers_to_faces_1D_t(order=order, cells=cells, dx=(x_max - x_min)/cells) &
      )
        associate(scalar_at_faces => interpolator%face_values(scalar_1D%values()))
          test_diagnosis = test_diagnosis .also. .all. (scalar_at_faces .approximates. scalar_1D_initializer(vector_1D%grid()) .within. tolerance) &
            // string_t(" for order ") // string_t(order)
        end associate
      end associate

    end do
  end function

  function check_faces_to_centers() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => null()
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => line
    double precision, parameter :: x_min = 0D0, x_max = 20D0
    integer order, cells

    test_diagnosis = passing_test()

    do order = 2, 4, 2

      select case(order)
      case(2)
        cells = 10
        vector_1D_initializer => line
      case(4)
        cells = 20
        vector_1D_initializer => cubic
      case default
        error stop "check_faces_to_centers (interpolator_1D_test_m) unsupported order of accuracy"
      end select

      associate( &
         vector_1D => vector_1D_t(vector_1D_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max) &
        ,scalar_1D => scalar_1D_t(scalar_1D_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max) &
        ,interpolator => faces_to_centers_1D_t(order=order, cells=cells, dx=(x_max - x_min)/cells) &
      )
        associate(vector_at_centers_extended => interpolator%center_values(vector_1D%values()))
          associate( centers_extended => scalar_1D%grid() )
            test_diagnosis = test_diagnosis .also. &
              .all. (vector_at_centers_extended .approximates. vector_1D_initializer(centers_extended) .within. tolerance) &
              // string_t(" for order ") // string_t(order)
          end associate
        end associate
      end associate

    end do
  end function

end module interpolator_1D_test_m
