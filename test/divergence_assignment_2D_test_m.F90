! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module divergence_assignment_2D_test_m
  use julienne_m, only : &
     operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.equalsExpected.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,passing_test &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,usher
  use formal_m, only : scalar_2D_t, vector_2D_t, gradient_2D_t, divergence_2D_t, scalar_2D_initializer_i
  implicit none

  integer, parameter :: x_cells_1 = 9, y_cells_1 = 10
  integer, parameter :: x_cells_2 = 11, y_cells_2 = 12
  real, parameter :: tolerance = 1E-6

  type, extends(test_t) :: divergence_assignment_2D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The scalar_2D_t divergence assignment'
  end function

  function results() result(test_results)
    type(divergence_assignment_2D_test_t) test_case
    type(test_result_t), allocatable :: test_results(:)

    test_results = test_case%run([ &
       test_description_t('copying a nonsquare divergence field into a scalar, preserving boundaries, repeated assignment, and .grad.', usher(check_divergence_assignment)) &
    ])
  end function

  pure function pattern_a(x, y) result(values)
    real, intent(in) :: x(:), y(:)
    real values(size(x), size(y))
    do concurrent(integer :: i = 1:size(x), j = 1:size(y)) default(none) shared(x, y, values)
      values(i,j) = 1000E0 + 10E0*x(i) + y(j)
    end do
  end function

  pure function pattern_b(x, y) result(values)
    real, intent(in) :: x(:), y(:)
    real values(size(x), size(y))
    do concurrent(integer :: i = 1:size(x), j = 1:size(y)) default(none) shared(x, y, values)
      values(i,j) = -500E0 + x(i) - 3E0*y(j)
    end do
  end function

  function check_divergence_assignment() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = passing_test()
    test_diagnosis = test_diagnosis .also. check_case(4, [x_cells_1, y_cells_1], [-2E0,-1E0], [1E0,3E0])
    test_diagnosis = test_diagnosis .also. check_case(2, [x_cells_2, y_cells_2], [-1E0,-2E0], [2E0,4E0])
  end function

  function check_case(order, cells, x_min, x_max) result(test_diagnosis)
    integer, intent(in) :: order, cells(2)
    real, intent(in) :: x_min(2), x_max(2)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_2D_initializer_i), pointer :: initializer_a, initializer_b
    type(divergence_2D_t) rhs_a, rhs_b
    type(scalar_2D_t) lhs, expected_a, expected_b
    type(gradient_2D_t) grad_lhs, grad_expected
    real, allocatable :: lhs_values(:,:), rhs_values(:,:)
    real, allocatable :: expected_values(:,:)

    test_diagnosis = passing_test()
    initializer_a => pattern_a
    initializer_b => pattern_b

    rhs_a = divergence_2D_t(initializer_a, order=order, cells=cells, x_min=x_min, x_max=x_max)
    rhs_b = divergence_2D_t(initializer_b, order=order, cells=cells, x_min=x_min, x_max=x_max)
    allocate(expected_values(cells(1)+2, cells(2)+2))
    expected_values = 0E0
    expected_values(2:size(expected_values,1)-1, 2:size(expected_values,2)-1) = rhs_a%values()
    expected_a = scalar_2D_t(expected_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max)
    expected_values = 0E0
    expected_values(2:size(expected_values,1)-1, 2:size(expected_values,2)-1) = rhs_b%values()
    expected_b = scalar_2D_t(expected_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max)

    lhs = rhs_a
    lhs_values = lhs%values()
    rhs_values = rhs_a%values()
    test_diagnosis = test_diagnosis .also. (check_shape(lhs_values)) .also. (check_contents(lhs_values, rhs_values)) .also. (check_gradient(lhs, expected_a))

    lhs = rhs_b
    lhs_values = lhs%values()
    rhs_values = rhs_b%values()
    test_diagnosis = test_diagnosis .also. (check_shape(lhs_values)) .also. (check_contents(lhs_values, rhs_values)) .also. (check_gradient(lhs, expected_b))

    lhs = rhs_a
    lhs_values = lhs%values()
    rhs_values = rhs_a%values()
    test_diagnosis = test_diagnosis .also. (check_shape(lhs_values)) .also. (check_contents(lhs_values, rhs_values)) .also. (check_gradient(lhs, expected_a))

  contains

    pure function expected_initializer(x, y) result(values)
      real, intent(in) :: x(:), y(:)
      real values(size(x), size(y))
      values = expected_values
    end function

    function check_shape(values) result(diagnosis)
      real, intent(in) :: values(:,:)
      type(test_diagnosis_t) diagnosis
      diagnosis = passing_test()
      diagnosis = diagnosis .also. (.all. (shape(values) .equalsExpected. [cells(1) + 2, cells(2) + 2]))
    end function

    function check_contents(values, rhs_values) result(diagnosis)
      real, intent(in) :: values(:,:), rhs_values(:,:)
      type(test_diagnosis_t) diagnosis
      diagnosis = passing_test()
      diagnosis = diagnosis .also. (boundary_zero(values)) .also. (interior_copy(values, rhs_values))
    end function

    function boundary_zero(values) result(diagnosis)
      real, intent(in) :: values(:,:)
      type(test_diagnosis_t) diagnosis
      integer :: nx, ny
      nx = size(values,1)
      ny = size(values,2)
      diagnosis = passing_test()
      diagnosis = diagnosis .also. (maxval(abs(values(1,:))) .approximates. 0E0 .within. tolerance)
      diagnosis = diagnosis .also. (maxval(abs(values(nx,:))) .approximates. 0E0 .within. tolerance)
      diagnosis = diagnosis .also. (maxval(abs(values(:,1))) .approximates. 0E0 .within. tolerance)
      diagnosis = diagnosis .also. (maxval(abs(values(:,ny))) .approximates. 0E0 .within. tolerance)
    end function

    function interior_copy(values, rhs_values) result(diagnosis)
      real, intent(in) :: values(:,:), rhs_values(:,:)
      type(test_diagnosis_t) diagnosis
      diagnosis = passing_test()
      diagnosis = diagnosis .also. (.all. (values(2:size(values,1)-1, 2:size(values,2)-1) .approximates. rhs_values .within. tolerance))
    end function

    function check_gradient(scalar_field, expected_scalar) result(diagnosis)
      type(scalar_2D_t), intent(in) :: scalar_field
      type(scalar_2D_t), intent(in) :: expected_scalar
      type(test_diagnosis_t) diagnosis
      type(gradient_2D_t) grad_expected
      diagnosis = passing_test()
      grad_lhs = .grad. scalar_field
      grad_expected = .grad. expected_scalar
      diagnosis = diagnosis .also. (.all. (shape(grad_lhs%to_centers_extended()) .equalsExpected. shape(grad_expected%to_centers_extended())))
      diagnosis = diagnosis .also. (.all. (grad_lhs%to_centers_extended() .approximates. grad_expected%to_centers_extended() .within. tolerance))
    end function

  end function check_case

end module divergence_assignment_2D_test_m
