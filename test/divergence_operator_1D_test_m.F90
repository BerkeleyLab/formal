! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
  !! include Julienne preprocessor  macros

module divergence_operator_1D_test_m
  use julienne_m, only : &
     operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,passing_test &
    ,string_t &
    ,test_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,usher
  use formal_m, only : vector_1D_t, vector_1D_initializer_i, scalar_1D_t, scalar_1D_initializer_i
#ifdef __GFORTRAN__
  use formal_m, only : divergence_1D_t
#endif
  implicit none

  type, extends(test_t) :: divergence_operator_1D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tight_tolerance = 5D-14, loose_tolerance = 1D-08, rough_tolerance = 1D-02, crude_tolerance = 2D-02

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'A 1D mimetic divergence operator'
  end function

  function results() result(test_results)
    type(divergence_operator_1D_test_t) divergence_operator_1D_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = divergence_operator_1D_test%run([ & 
       test_description_t( &
          'computing 2nd-order .div.(.grad. (x**2)/2) within ' // string_t(tight_tolerance) &
         ,usher(check_2nd_order_div_grad_parabola)) &
      ,test_description_t( &
          'computing 4th-order .div.(.grad. (x**2)/2) within ' // string_t(tight_tolerance) &
         ,usher(check_4th_order_div_grad_parabola)) &
      ,test_description_t( &
          'computing convergence rate of 2 for 2nd-order .div. [sin(x) + cos(x)] within ' // string_t(rough_tolerance) &
         ,usher(check_2nd_order_div_sinusoid_convergence)) &
      ,test_description_t( &
          'computing convergence rate of 4 for 4th-order .div. [sin(x) + cos(x)] within ' // string_t(crude_tolerance) &
         ,usher(check_4th_order_div_sinusoid_convergence)) &
    ])
  end function

  pure function parabola(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = (x**2)/2
  end function

  ! PURPOSE: Tests that the 2nd-order discrete divergence of the gradient operator correctly computes
  !          div(grad(f)) for a parabolic function (x^2/2), which should yield a constant value of
  !          1.0 everywhere, and reports a passing or failing test diagnosis based on whether the
  !          computed values approximate the expected result within a tight tolerance.
  ! KEYWORDS: divergence, gradient, div-grad, laplacian-equivalence, finite-difference,
  !           defined operation, unit-test, scalar_1D, parabola, 2nd-order, structured-grid, staggered-grid,
  !           test-diagnosis, differential-operator, verification, compound-operator
  ! CONTEXT: This function is part of the operator test suite in the formal library, which provides
  !          defined operations (.grad., .div., .laplacian., etc.) for staggered-grid
  !          scalar fields. It tests the compound expression .div. (.grad. f), which should be
  !          mathematically equivalent to the Laplacian but is computed by composing the gradient and
  !          divergence operators separately rather than using the dedicated .laplacian. operator. The
  !          scalar field is initialized as a parabola (x^2/2) on a 16-cell 1D domain [0, 5] at
  !          2nd-order accuracy, and the expected div(grad) value is the constant 1.0. This
  !          complements the direct Laplacian tests by verifying that the discrete gradient and
  !          divergence operators compose correctly. A tight_tolerance is used, reflecting that the
  !          parabola is within the polynomial exactness range of the 2nd-order stencils. The
  !          conditional compilation directives handle differences between gfortran and other compilers
  !          regarding associate-block support for user-defined operator results. The test result is
  !          accumulated using the .also. and .approximates. defined operations and the
  !          passing_test()/test_diagnosis_t testing infrastructure.
  function check_2nd_order_div_grad_parabola() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => parabola
    double precision, parameter :: expected_divergence = 1D0
#ifdef __GFORTRAN__
    type(divergence_1D_t) div_grad_scalar
    div_grad_scalar = .div. (.grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=5D0))
#else
    associate(div_grad_scalar => .div. (.grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=5D0)))
#endif
 
      test_diagnosis = passing_test()
      test_diagnosis = test_diagnosis .also. (.all. (div_grad_scalar%values() .approximates. expected_divergence .within. tight_tolerance)) &
                     // " (2nd-order .div. (.grad. (x**2)/2))"

#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  ! PURPOSE: Tests that the 4th-order discrete divergence of the gradient operator correctly computes
  !          div(grad(f)) for a parabolic function (x^2/2), which should yield a constant value of
  !          1.0 everywhere, and reports a passing or failing test diagnosis based on whether the
  !          computed values approximate the expected result within a tight tolerance.
  ! KEYWORDS: divergence, gradient, div-grad, laplacian-equivalence, finite-difference,
  !           defined operation, unit-test, scalar_1D, parabola, 4th-order, structured-grid, staggered-grid,
  !           test-diagnosis, differential-operator, verification, compound-operator,
  !           higher-order-accuracy
  ! CONTEXT: This function is part of the operator test suite in the formal library, which provides
  !          defined operations (.grad., .div., .laplacian., etc.) for staggered-grid
  !          scalar fields. It tests the compound expression .div. (.grad. f), which should be
  !          mathematically equivalent to the Laplacian but is computed by composing the gradient and
  !          divergence operators separately rather than using the dedicated .laplacian. operator. The
  !          scalar field is initialized as a parabola (x^2/2) on a 16-cell 1D domain [0, 9] at
  !          4th-order accuracy, and the expected div(grad) value is the constant 1.0. Compared to the
  !          2nd-order variant, this test uses a wider domain [0, 9] vs [0, 5], which tests the
  !          higher-order stencil on a coarser effective resolution. A tight_tolerance is still used,
  !          as the parabola is well within the polynomial exactness range of the 4th-order stencils.
  !          The conditional compilation directives handle differences between gfortran and other
  !          compilers regarding associate-block support for user-defined operator results. The test
  !          result is accumulated using the .also. and .approximates. defined operations and the
  !          passing_test()/test_diagnosis_t testing infrastructure.
  function check_4th_order_div_grad_parabola() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => parabola
    double precision, parameter :: expected_divergence = 1D0
#ifdef __GFORTRAN__
    type(divergence_1D_t) div_grad_scalar
    div_grad_scalar = .div. (.grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=9D0))
#else
    associate(div_grad_scalar => .div. (.grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=9D0)))
#endif

      test_diagnosis = passing_test()
      test_diagnosis = test_diagnosis .also. (.all. (div_grad_scalar%values() .approximates. expected_divergence .within. tight_tolerance)) &
                     // " (4th-order .div. (.grad. (x**2)/2))"

#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  pure function sinusoid(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = sin(x) + cos(x)
  end function

  ! PURPOSE: Tests that the 2nd-order discrete divergence operator converges at the expected rate by
  !          comparing coarse-grid (100 cells) and fine-grid (101 cells) solutions of the divergence
  !          of the vector field [sin(x) + cos(x)] against the analytical divergence cos(x) - sin(x).
  !          It verifies that both grids approximate the expected divergence within a rough tolerance
  !          and that the observed convergence rate matches 2nd-order accuracy.
  ! KEYWORDS: divergence, finite-difference, convergence-rate, 2nd-order, defined operation,
  !           unit-test, vector_1D, sinusoid, structured-grid, staggered-grid, test-diagnosis, differential-operator,
  !           verification, grid-refinement, order-of-accuracy
  ! CONTEXT: This function is part of the divergence operator test suite in the formal library, which
  !          provides defined operations (.div., .grad., .laplacian., etc.) for
  !          staggered-grid scalar and vector fields. It constructs two vector_1D_t objects
  !          initialized with a sinusoidal function on the domain [0, 2*pi] at 2nd-order accuracy
  !          with coarse (100) and fine (101) cell counts, applies the .div. operator to both, and
  !          compares the results against the analytical divergence cos(x) - sin(x). This test mirrors
  !          the structure of the gradient convergence tests but exercises the divergence operator on
  !          a vector field rather than the gradient operator on a scalar field. The observed
  !          convergence rate is computed via log(coarse_error/fine_error)/log(fine_cells/coarse_cells)
  !          and checked against the desired 2nd order. A rough_tolerance is used for all checks. The
  !          conditional compilation directives handle differences between gfortran and other compilers
  !          regarding associate-block support for user-defined operator results. The test result is
  !          accumulated using the .also. and .approximates. defined operations and the
  !          passing_test()/test_diagnosis_t testing infrastructure.
  function check_2nd_order_div_sinusoid_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => sinusoid
    double precision, parameter :: pi = 3.141592653589793D0
    integer, parameter :: order_desired = 2, coarse_cells=100, fine_cells=coarse_cells+1
#ifdef __GFORTRAN__
    type(divergence_1D_t) div_coarse, div_fine
    div_coarse = .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi)
    div_fine   = .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi)
#else
    associate( &
       div_coarse => .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi) &
      ,div_fine   => .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi) &
    )
#endif
      associate( &
         x_coarse => div_coarse%grid() &
        ,x_fine   => div_fine%grid())
        associate( &
           grad_coarse => cos(x_coarse) - sin(x_coarse) &
          ,grad_fine   => cos(x_fine)   - sin(x_fine) &
          ,div_coarse_values => div_coarse%values() &
          ,div_fine_values   => div_fine%values() &
        )
          test_diagnosis = passing_test()
          test_diagnosis = test_diagnosis .also. (.all. (div_coarse_values .approximates. grad_coarse .within. rough_tolerance)) &
            // " (coarse-grid 2nd-order .div. [sin(x) + cos(x)])"
          test_diagnosis = test_diagnosis .also. (.all. (div_fine_values .approximates. grad_fine .within. rough_tolerance)) &
            // " (fine-grid 2nd-order .div. [sin(x) + cos(x)])"
          associate( &
             error_coarse_max => maxval(abs(div_coarse_values - grad_coarse)) &
            ,error_fine_max   => maxval(abs(div_fine_values   - grad_fine)) &
          )
            associate(order_actual => log(error_coarse_max/error_fine_max)/log(dble(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. dble(order_desired) .within. rough_tolerance) &
                // " (convergence rate for 2nd-order .div. [sin(x) + cos(x)])"
            end associate
          end associate
        end associate
      end associate
#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  ! PURPOSE: Tests that the 4th-order discrete divergence operator converges at the expected rate by
  !          comparing coarse-grid (500 cells) and fine-grid (501 cells) solutions of the divergence
  !          of the vector field [sin(x) + cos(x)] against the analytical divergence cos(x) - sin(x).
  !          It verifies that both grids approximate the expected divergence within a loose tolerance
  !          and that the observed convergence rate matches 4th-order accuracy.
  ! KEYWORDS: divergence, finite-difference, convergence-rate, 4th-order, defined operation,
  !           unit-test, vector_1D, sinusoid, structured-grid, staggered-grid, test-diagnosis, differential-operator,
  !           verification, grid-refinement, order-of-accuracy, higher-order-accuracy
  ! CONTEXT: This function is part of the divergence operator test suite in the formal library, which
  !          provides defined operations (.div., .grad., .laplacian., etc.) for
  !          staggered-grid scalar and vector fields. It constructs two vector_1D_t objects
  !          initialized with a sinusoidal function on the domain [0, 2*pi] at 4th-order accuracy
  !          with coarse (500) and fine (501) cell counts, applies the .div. operator to both, and
  !          compares the results against the analytical divergence cos(x) - sin(x). Compared to the
  !          2nd-order divergence convergence test, this test uses significantly more cells (500/501
  !          vs 100/101) to ensure stable convergence rate estimation at higher order. The point-wise
  !          accuracy checks use loose_tolerance rather than rough_tolerance, reflecting the tighter
  !          errors achievable with the 4th-order stencil on finer grids, while the convergence rate
  !          check uses crude_tolerance. The observed convergence rate is computed via
  !          log(coarse_error/fine_error)/log(fine_cells/coarse_cells) and checked against the desired
  !          4th order. The conditional compilation directives handle differences between gfortran and
  !          other compilers regarding associate-block support for user-defined operator results. The
  !          test result is accumulated using the .also. and .approximates. defined operations and
  !          the passing_test()/test_diagnosis_t testing infrastructure.
  function check_4th_order_div_sinusoid_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => sinusoid
    double precision, parameter :: pi = 3.141592653589793D0
    integer, parameter :: order_desired = 4, coarse_cells=500, fine_cells=coarse_cells+1
#ifdef __GFORTRAN__
    type(divergence_1D_t) div_coarse, div_fine
    div_coarse = .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi)
    div_fine   = .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi)
#else
    associate( &
       div_coarse => .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi) &
      ,div_fine   => .div. vector_1D_t(vector_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi) &
    )
#endif
      associate( &
         x_coarse => div_coarse%grid() &
        ,x_fine   => div_fine%grid()  &
      )
        associate( &
           div_coarse_expected => cos(x_coarse) - sin(x_coarse) &
          ,div_fine_expected   => cos(x_fine) - sin(x_fine) &
          ,div_coarse_values => div_coarse%values() &
          ,div_fine_values   => div_fine%values() &
        )

          test_diagnosis = passing_test()
          test_diagnosis = test_diagnosis .also. (.all. (div_coarse_values .approximates. div_coarse_expected .within. loose_tolerance)) &
            // " (coarse-grid 4th-order .div. [sin(x) + cos(x)])"
          test_diagnosis = test_diagnosis .also. (.all. (div_fine_values .approximates. div_fine_expected .within. loose_tolerance)) &
            // " (fine-grid 4th-order .div. [sin(x) + cos(x)])"

          associate( &
             error_coarse_max => maxval(abs(div_coarse_values - div_coarse_expected)) &
            ,error_fine_max => maxval(abs(div_fine_values - div_fine_expected)) &
          )
            associate(order_actual => log(error_coarse_max/error_fine_max)/log(dble(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. dble(order_desired) .within. crude_tolerance) &
                // " (convergence rate for 4th-order .div. [sin(x) + cos(x)])"
            end associate
          end associate
        end associate
      end associate
#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

end module divergence_operator_1D_test_m
