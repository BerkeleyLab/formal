! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module vector_2D_test_m
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
  use formal_m, only : &
     vector_2D_t &
    ,vector_2D_initializer_i &
    ,divergence_2D_t &
    ,divergence_2D_initializer_i

  implicit none

  type, extends(test_t) :: vector_2D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tolerance = 5D-2
  integer, parameter :: space_dimension = 2

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
   ])
  end function

  pure function v(x,y) result(z)
    double precision, intent(in) :: x(:), y(:)
    double precision z(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(x,y,z)
      z(i,j,:) = [0D0,0D0]
    end do
  end function

  pure function div_v(x,y) result(divergence)
    double precision, intent(in) :: x(:), y(:)
    double precision divergence(size(x),size(y))
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(divergence,x,y)
      divergence(i,j) = 0D0
    end do
  end function

  function check_divergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_2D_initializer_i), pointer :: vector_2D_initializer
    procedure(divergence_2D_initializer_i), pointer :: expected_divergence_initializer
    integer order

    vector_2D_initializer => v
    expected_divergence_initializer => div_v
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(vector_2D => vector_2D_t(vector_2D_initializer, order=order, cells=[30,20], x_min=[-1D0,1D0], x_max=[9D0,4D0]))
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

end module vector_2D_test_m