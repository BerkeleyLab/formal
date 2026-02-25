! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
  !! include Julienne preprocessor  macros

module gradient_operator_1D_test_m
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
  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i
#ifdef __GFORTRAN__
  use formal_m, only : vector_1D_t, vector_1D_initializer_i, gradient_1D_t
#endif

  type, extends(test_t) :: gradient_operator_1D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tight_tolerance = 1D-14, loose_tolerance = 1D-12, rough_tolerance = 5D-02

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'A 1D mimetic gradient operator'
  end function

  function results() result(test_results)
    type(gradient_operator_1D_test_t) gradient_operator_1D_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = gradient_operator_1D_test%run([ & 
       test_description_t('computing 2nd- & 4th-order .grad. (5) within ' &
         // string_t(tight_tolerance), usher(check_grad_const)) &
      ,test_description_t('computing 2nd- & 4th-order .grad. (14*x + 3) within ' &
         // string_t(loose_tolerance), usher(check_grad_line)) &
      ,test_description_t('computing 2nd- & 4th-order .grad. (7*x**2 + 3*x + 5) within ' &
         // string_t(loose_tolerance), usher(check_grad_parabola)) &
      ,test_description_t('computing convergence rate of 2 for 2nd-order .grad. [sin(x) + cos(x)] within ' &
         // string_t(rough_tolerance), usher(check_2nd_order_grad_convergence)) &
      ,test_description_t('computing convergence rate of 4 for 4th-order .grad. [sin(x) + cos(x)] within ' &
         // string_t(rough_tolerance), usher(check_4th_order_grad_convergence)) &
    ])
  end function

  pure function const(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    integer i
    y = [(5D0, i=1,size(x))]
  end function

  ! PURPOSE: Tests that the discrete gradient operator correctly computes the gradient of a constant
  !          function, which should yield zero everywhere, at both 2nd-order and 4th-order accuracy
  !          on 16-cell 1D domains, and reports a passing or failing test diagnosis based on whether
  !          the computed values approximate zero within a loose tolerance.
  ! KEYWORDS: gradient, finite-difference, defined operation, unit-test, scalar_1D, constant-function,
  !           2nd-order, 4th-order, structured-grid, staggered-grid, test-diagnosis, differential-operator, verification
  ! CONTEXT: This function is part of the gradient operator test suite in the formal library, which
  !          provides defined operations (.grad., .laplacian., etc.) for staggered-grid
  !          scalar fields. It constructs a scalar_1D_t object initialized with a constant function and
  !          applies the .grad. operator twice: first at 2nd-order on the domain [0, 4], then at
  !          4th-order on the domain [0, 8]. In both cases the analytically expected gradient is zero,
  !          so this test verifies that the finite-difference stencils do not introduce spurious
  !          non-zero gradients for a trivial input. A loose_tolerance is used for both checks. The
  !          conditional compilation directives handle differences between gfortran and other compilers
  !          regarding associate-block support for user-defined operator results. The test result is
  !          accumulated across both order checks using the .also. and .approximates. defined
  !          operations and the passing_test()/test_diagnosis_t testing infrastructure.
  function check_grad_const() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: grad_expected = 0.
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => const

#ifdef __GFORTRAN__
    type(gradient_1D_t) grad

    grad = .grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=4D0)
#else
    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=4D0))
#endif

      test_diagnosis = passing_test()
      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (2nd-order .grad.(5))"

#ifndef __GFORTRAN__
    end associate
#endif

#ifdef __GFORTRAN__
    grad = .grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=8D0)
#else
    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=8D0))
#endif

      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (4th-order .grad.(5))"

#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  pure function line(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = 14*x + 3
  end function

  ! PURPOSE: Tests that the discrete gradient operator correctly computes the gradient of a linear
  !          function (14*x + 3), which should yield a constant value of 14 everywhere, at both
  !          2nd-order and 4th-order accuracy on 16-cell 1D domains, and reports a passing or failing
  !          test diagnosis based on whether the computed values approximate the expected constant
  !          gradient within a loose tolerance.
  ! KEYWORDS: gradient, finite-difference, defined operation, unit-test, scalar_1D, linear-function,
  !           2nd-order, 4th-order, structured-grid, staggered-grid, test-diagnosis, differential-operator, verification
  ! CONTEXT: This function is part of the gradient operator test suite in the formal library, which
  !          provides defined operations (.grad., .laplacian., etc.) for staggered-grid
  !          scalar fields. It constructs a scalar_1D_t object initialized with a linear function and
  !          applies the .grad. operator twice: first at 2nd-order on the domain [0, 4], then at
  !          4th-order on the domain [0, 8]. In both cases the analytically expected gradient is the
  !          constant 14, so this test verifies that finite-difference stencils of different orders
  !          exactly reproduce the gradient of a linear function, which is within the polynomial
  !          exactness range of both stencil orders. This complements the constant-function gradient
  !          test by exercising a non-trivial but still analytically simple input. A loose_tolerance
  !          is used for both checks. The conditional compilation directives handle differences between
  !          gfortran and other compilers regarding associate-block support for user-defined operator
  !          results. The test result is accumulated across both order checks using the .also. and
  !          .approximates. defined operations and the passing_test()/test_diagnosis_t testing
  !          infrastructure.
  function check_grad_line() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    double precision, parameter :: grad_expected = 14D0
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => line
#ifdef __GFORTRAN__
    type(gradient_1D_t) grad

    grad = .grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=4D0)
#else
    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=4D0))
#endif

      test_diagnosis = passing_test()
      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (2nd-order .grad.(14*x + 3))"

#ifndef __GFORTRAN__
    end associate
#endif

#ifdef __GFORTRAN__
    grad = .grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=8D0)
#else
    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=8D0))
#endif

      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (4th-order .grad.(14*x + 3))"

#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  pure function parabola(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = 7*x**2 + 3*x + 5
  end function

  ! PURPOSE: Tests that the discrete gradient operator correctly computes the gradient of a parabolic
  !          function (7*x^2 + 3*x + 5), which should yield the spatially-varying result 14*x + 3
  !          at each grid point, at both 2nd-order and 4th-order accuracy on 16-cell 1D domains, and
  !          reports a passing or failing test diagnosis based on whether the computed values
  !          approximate the expected gradient within a loose tolerance.
  ! KEYWORDS: gradient, finite-difference, defined operation, unit-test, scalar_1D, parabola,
  !           2nd-order, 4th-order, structured-grid, staggered-grid, test-diagnosis, differential-operator,
  !           verification, spatially-varying
  ! CONTEXT: This function is part of the gradient operator test suite in the formal library, which
  !          provides defined operations (.grad., .laplacian., etc.) for staggered-grid
  !          scalar fields. It constructs a scalar_1D_t object initialized with a parabolic function
  !          and applies the .grad. operator twice: first at 2nd-order on the domain [0, 4], then at
  !          4th-order on the domain [0, 8]. Unlike the constant and linear gradient tests, the
  !          expected gradient here is spatially varying (14*x + 3), so the test retrieves the grid
  !          coordinates via grad%grid() and constructs the expected values at each grid point using
  !          nested associate blocks. The parabola is within the polynomial exactness range of both
  !          the 2nd-order and 4th-order stencils, so both should reproduce the analytical gradient.
  !          A loose_tolerance is used for both checks. The conditional compilation directives handle
  !          differences between gfortran and other compilers regarding associate-block support for
  !          user-defined operator results. The test result is accumulated across both order checks
  !          using the .also. and .approximates. defined operations and the passing_test()/
  !          test_diagnosis_t testing infrastructure.
  function check_grad_parabola() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => parabola
#ifdef __GFORTRAN__
    type(gradient_1D_t) grad

    grad =  .grad. scalar_1D_t(scalar_1D_initializer , order=2, cells=16, x_min=0D0, x_max=4D0)
#else
    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer , order=2, cells=16, x_min=0D0, x_max=4D0))
#endif

      test_diagnosis = passing_test()

      associate(x => grad%grid())
        associate(grad_expected => 14*x + 3)
          test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
            // " (2nd-order .grad.(7*x**2 + 3*x + 5))"
        end associate
      end associate

#ifndef __GFORTRAN__
    end associate
#endif

#ifdef __GFORTRAN__
    grad = .grad. scalar_1D_t(scalar_1D_initializer , order=4, cells=16, x_min=0D0, x_max=8D0)
#else
    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer , order=4, cells=16, x_min=0D0, x_max=8D0))
#endif

      associate(x => grad%grid())
        associate(grad_expected => 14*x + 3)
          test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
            // " (4th-order .grad.(7*x**2 + 3*x + 5))"
        end associate
      end associate

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

  ! PURPOSE: Tests that the 2nd-order discrete gradient operator converges at the expected rate by
  !          comparing coarse-grid (200 cells) and fine-grid (201 cells) solutions of the gradient
  !          of sin(x) + cos(x) against the analytical derivative cos(x) - sin(x). It verifies that
  !          both grids approximate the expected gradient within a rough tolerance and that the
  !          observed convergence rate matches 2nd-order accuracy.
  ! KEYWORDS: gradient, finite-difference, convergence-rate, 2nd-order, defined operation,
  !           unit-test, scalar_1D, sinusoid, structured-grid, staggered-grid, test-diagnosis, differential-operator,
  !           verification, grid-refinement, order-of-accuracy
  ! CONTEXT: This function is part of the gradient operator test suite in the formal library, which
  !          provides defined operations (.grad., .laplacian., etc.) for staggered-grid
  !          scalar fields. It constructs two scalar_1D_t objects initialized with a sinusoidal
  !          function on the domain [0, 2*pi] at 2nd-order accuracy with coarse (200) and fine (201)
  !          cell counts, applies the .grad. operator to both, and compares the results against the
  !          analytical derivative cos(x) - sin(x). Unlike the polynomial gradient tests (constant,
  !          linear, parabola) that verify exact reproduction within stencil polynomial exactness,
  !          this test uses a transcendental function to measure the actual convergence rate via
  !          log(coarse_error/fine_error)/log(fine_cells/coarse_cells) and checks that it matches the
  !          desired 2nd order. Unlike the Laplacian convergence test, this test does not separately
  !          check interior and boundary convergence rates. A rough_tolerance is used for all checks.
  !          The conditional compilation directives handle differences between gfortran and other
  !          compilers regarding associate-block support for user-defined operator results. The test
  !          result is accumulated using the .also. and .approximates. defined operations and the
  !          passing_test()/test_diagnosis_t testing infrastructure.
  function check_2nd_order_grad_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => sinusoid
    double precision, parameter :: pi = 3.141592653589793D0
    integer, parameter :: order_desired = 2, coarse_cells=200, fine_cells=coarse_cells+1
#ifdef __GFORTRAN__
    type(gradient_1D_t) grad_coarse, grad_fine

    grad_coarse = .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi)
    grad_fine   = .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi)
#else
    associate( &
       grad_coarse => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi) &
      ,grad_fine   => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi) &
    )
#endif
      associate( &
         x_coarse => grad_coarse%grid() &
        ,x_fine   => grad_fine%grid() &
      )
        associate( &
          grad_coarse_expected => cos(x_coarse) - sin(x_coarse) &
         ,grad_fine_expected   => cos(x_fine) - sin(x_fine) &
         ,grad_coarse_values   => grad_coarse%values() &
         ,grad_fine_values     => grad_fine%values() &
        )
          test_diagnosis = passing_test()
          test_diagnosis = test_diagnosis .also. (.all. (grad_coarse_values .approximates. grad_coarse_expected .within. rough_tolerance)) &
            // " (coarse-grid 2nd-order .grad. [sin(x) + cos(x)])"
          test_diagnosis = test_diagnosis .also. (.all. (grad_fine_values .approximates. grad_fine_expected .within. rough_tolerance)) &
            // " (fine-grid 4th-order .grad. [sin(x) + cos(x)])"
          associate( &
             error_coarse_max => maxval(abs(grad_coarse_values - grad_coarse_expected)) &
            ,error_fine_max   => maxval(abs(grad_fine_values - grad_fine_expected)) &
          )
            associate(order_actual => log(error_coarse_max/error_fine_max)/log(dble(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. dble(order_desired) .within. rough_tolerance)  &
                // " (2nd-order .grad. [sin(x) + cos(x)] order of accuracy)"
            end associate
          end associate
        end associate
      end associate
#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  ! PURPOSE: Tests that the 4th-order discrete gradient operator converges at the expected rate by
  !          comparing coarse-grid and fine-grid solutions of the gradient of sin(x) + cos(x) against
  !          the analytical derivative cos(x) - sin(x). It verifies that both grids approximate the
  !          expected gradient within a rough tolerance and that the observed convergence rate matches
  !          4th-order accuracy.
  ! KEYWORDS: gradient, finite-difference, convergence-rate, 4th-order, defined operation,
  !           unit-test, scalar_1D, sinusoid, structured-grid, staggered-grid, test-diagnosis, differential-operator,
  !           verification, grid-refinement, order-of-accuracy, higher-order-accuracy
  ! CONTEXT: This function is part of the gradient operator test suite in the formal library, which
  !          provides defined operations (.grad., .laplacian., etc.) for staggered-grid
  !          scalar fields. It constructs two scalar_1D_t objects initialized with a sinusoidal
  !          function on the domain [0, 2*pi] at 4th-order accuracy, applies the .grad. operator to
  !          both, and compares the results against the analytical derivative cos(x) - sin(x). The
  !          coarse and fine cell counts differ between compilers: gfortran uses 300/301 while other
  !          compilers use 400/401, reflecting compiler-specific numerical behavior at higher order.
  !          Like the 2nd-order gradient convergence test, this test computes the observed convergence
  !          rate via log(coarse_error/fine_error)/log(fine_cells/coarse_cells) and checks that it
  !          matches the desired 4th order. It does not separately check interior and boundary
  !          convergence rates as the Laplacian convergence test does. A rough_tolerance is used for
  !          all checks. The conditional compilation directives handle both the differing cell counts
  !          and the differences between gfortran and other compilers regarding associate-block support
  !          for user-defined operator results. The test result is accumulated using the .also. and
  !          .approximates. defined operations and the passing_test()/test_diagnosis_t testing
  !          infrastructure.
  function check_4th_order_grad_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => sinusoid
    double precision, parameter :: pi = 3.141592653589793D0
#ifdef __GFORTRAN__
    integer, parameter :: order_desired = 4, coarse_cells=300, fine_cells=coarse_cells+1
    type(gradient_1D_t) grad_coarse, grad_fine

    grad_coarse = .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi)
    grad_fine   = .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi)
#else
    integer, parameter :: order_desired = 4, coarse_cells=400, fine_cells=coarse_cells+1
    associate( &
       grad_coarse => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi) &
      ,grad_fine   => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi) &
    )
#endif
      associate( &
         x_coarse => grad_coarse%grid() &
        ,x_fine   => grad_fine%grid() &
      )
        associate( &
           grad_coarse_expected => cos(x_coarse) - sin(x_coarse) &
          ,grad_fine_expected   => cos(x_fine) - sin(x_fine) &
          ,grad_coarse_values   => grad_coarse%values() &
          ,grad_fine_values     => grad_fine%values() &
        )
          test_diagnosis = passing_test()
          test_diagnosis = test_diagnosis .also. (.all. (grad_coarse_values .approximates. grad_coarse_expected .within. rough_tolerance)) &
            // " (4th-order d(sinusoid)/dx point-wise errors)"
          test_diagnosis = test_diagnosis .also. (.all. (grad_fine_values .approximates. grad_fine_expected .within. rough_tolerance)) &
            // " (4th-order d(sinusoid)/dx point-wise)"
          associate( &
             error_coarse_max => maxval(abs(grad_coarse_values - grad_coarse_expected)) &
            ,error_fine_max   => maxval(abs(grad_fine_values - grad_fine_expected)) &
          )
            associate(order_actual => log(error_coarse_max/error_fine_max)/log(dble(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. dble(order_desired) .within. rough_tolerance) &
                // " (4th-order d(sinusoid)/dx order of accuracy)"
            end associate
          end associate
        end associate
      end associate
#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

end module
