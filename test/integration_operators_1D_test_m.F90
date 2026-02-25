! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"
  !! include Julienne preprocessor  macros

module integration_operators_1D_test_m
  use julienne_m, only : &
     operator(//) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.isAtMost.) &
    ,operator(.withinPercentage.) &
    ,passing_test &
    ,string_t &
    ,test_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,usher
  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i, vector_1D_t, vector_1D_initializer_i
  implicit none

  type, extends(test_t) :: integration_operators_1D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  character(len=*), parameter, dimension(*) :: ordinal = ["   ", "2nd", "   ", "4th"]
  double precision, parameter :: residual_tolerance = 1D-15

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The set of 2nd- and 4th-order 1D mimetic integration operators'
  end function

  function results() result(test_results)
    type(integration_operators_1D_test_t) integration_operators_1D_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = integration_operators_1D_test%run([ & 
       test_description_t( &
          'computing the volume integral .SSS. (v .dot. .grad. f)*dV' &
         ,usher(check_volume_integral_of_v_dot_grad_f)) &
      ,test_description_t( &
          'computing the volume integral .SSS. (f * .div. v)*dV' &
         ,usher(check_volume_integral_of_f_div_v)) &
      ,test_description_t( &
          'computing the surface integral .SS. (f .x. (v .dot. dA))' &
         ,usher(check_surface_integral_of_vf)) &
      ,test_description_t( &
          'satisfying the extended Gauss Divergence Theorem within a residual tolerance of ' // string_t(residual_tolerance) &
         ,usher(check_gauss_divergence_theorem)) &
    ])
  end function

  pure function parabola(x) result(f)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = (x**2)/2
  end function

  pure function line(x) result(v)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: v(:)
    v = x
  end function

  pure function SSS_v_dot_grad_f(x) result(integral)
    double precision, intent(in) :: x
    double precision integral 
    integral = (x**3)/3
  end function

  pure function SSS_f_div_v(x) result(integral)
    double precision, intent(in) :: x
    double precision integral 
    integral = (x**3)/6
  end function

  ! PURPOSE: Tests that the volume integral of v dot grad(f) converges at the expected rate and
  !          produces sufficiently accurate results for both 2nd-order and 4th-order discretizations.
  !          It computes the integral .SSS. (v .dot. .grad. f) * dV on two grids (500 and 501 cells)
  !          for each order, compares the high-resolution result against a known analytical integral,
  !          and verifies that the observed convergence rate matches the expected order of accuracy.
  ! KEYWORDS: volume-integral, gradient, dot-product, finite-difference, convergence-rate,
  !           defined operation, unit-test, scalar_1D, vector_1D, parabola, linear-function,
  !           2nd-order, 4th-order, structured-grid, staggered-grid, test-diagnosis, verification, grid-refinement,
  !           order-of-accuracy, integral-operator
  ! CONTEXT: This function is part of the operator test suite in the formal library, which provides
  !          defined operations (.grad., .SSS., .dot., etc.) for staggered-grid scalar
  !          and vector fields. It exercises a compound expression combining the gradient operator,
  !          vector dot product, volume element, and volume integration operator in a single test. The
  !          scalar field f is initialized as a parabola and the vector field v as a linear function,
  !          yielding an analytically known volume integral via the antiderivative SSS_v_dot_grad_f
  !          evaluated at the domain boundaries [0, 1]. The test loops over 2nd-order and 4th-order
  !          discretizations, using order-specific expected convergence rates and solution tolerances
  !          stored in parameter arrays indexed by order. For each order, it constructs low-resolution
  !          (500 cells) and high-resolution (501 cells) scalar and vector fields, computes the volume
  !          integral on each, checks the high-resolution absolute error against a tight solution
  !          tolerance, and verifies the convergence rate via log(lo_res/hi_res)/log(cells_/cells)
  !          against the expected order within a percentage tolerance. The test result is accumulated
  !          using the .also., .isAtMost., .approximates., and .withinPercentage. defined operations
  !          and the passing_test()/test_diagnosis_t testing infrastructure.
  function check_volume_integral_of_v_dot_grad_f() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    double precision, parameter :: x_min = 0D0, x_max = 1D0
    integer, parameter :: cells = 500, cells_ = cells + 1
    double precision, parameter, dimension(*) :: expected  = [0, 2, 0, 1]
    double precision, parameter, dimension(*) :: order_tolerance = [0, 1, 0, 1]
    double precision, parameter, dimension(*) :: solution_tolerance = [0D0, 5D-7, 0D0, 5D-10]
    integer order

    scalar_1D_initializer => parabola
    vector_1D_initializer => line

    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate( &
         f  => scalar_1D_t(scalar_1D_initializer, order, cells , x_min, x_max) &
        ,f_ => scalar_1D_t(scalar_1D_initializer, order, cells_, x_min, x_max) &
        ,v  => vector_1D_t(vector_1D_initializer, order, cells , x_min, x_max) &
        ,v_ => vector_1D_t(vector_1D_initializer, order, cells_, x_min, x_max) &
        ,expected_integral => SSS_v_dot_grad_f(x_max) - SSS_v_dot_grad_f(x_min) &
      )
        associate( &
           dV  => f%dV() &
          ,dV_ => f_%dV() &
        )
          associate( &
             lo_res => abs((.SSS. (v  .dot. .grad. f ) * dV  - expected_integral)) &
            ,hi_res => abs((.SSS. (v_ .dot. .grad. f_) * dV_ - expected_integral)) &
          )
            test_diagnosis = test_diagnosis .also. &
              (hi_res .isAtMost. solution_tolerance(order)) &
              // " for " // ordinal(order) // "-order discretization of .SSS. (v .dot. .grad. f) * dV"
              associate(calculated_order => log(lo_res/hi_res)/log(dble(cells_)/cells))
                test_diagnosis = test_diagnosis .also. &
                  (calculated_order .approximates. expected(order) .withinPercentage. order_tolerance(order)) &
                  // " for convergence rate of " // ordinal(order) //  "-order discretization of .SSS. (v .dot. .grad. f) * dV"
              end associate
            end associate
          end associate
      end associate
    end do

  end function
  ! END CODE CHUNK

  ! PURPOSE: Tests that the volume integral of f times div(v) converges at the expected rate and
  !          produces sufficiently accurate results for both 2nd-order and 4th-order discretizations.
  !          It computes the integral .SSS. (f * .div. v) * dV on two grids (500 and 501 cells) for
  !          each order, compares the high-resolution result against a known analytical integral, and
  !          verifies that the observed convergence rate matches the expected order of accuracy.
  ! KEYWORDS: volume-integral, divergence, scalar-multiplication, finite-difference, convergence-rate,
  !           defined operation, unit-test, scalar_1D, vector_1D, parabola, linear-function,
  !           2nd-order, 4th-order, structured-grid, staggered-grid, test-diagnosis, verification, grid-refinement,
  !           order-of-accuracy, integral-operator
  ! CONTEXT: This function is part of the operator test suite in the formal library, which provides
  !          defined operations (.grad., .div., .SSS., etc.) for staggered-grid scalar
  !          and vector fields. It exercises a compound expression combining the divergence operator,
  !          scalar-field multiplication, volume element, and volume integration operator in a single
  !          test. The scalar field f is initialized as a parabola and the vector field v as a linear
  !          function, yielding an analytically known volume integral via the antiderivative
  !          SSS_f_div_v evaluated at the domain boundaries [0, 1]. This test complements the
  !          check_volume_integral_of_v_dot_grad_f test, as f*div(v) and v dot grad(f) are related
  !          through integration by parts. The test loops over 2nd-order and 4th-order
  !          discretizations, using order-specific expected convergence rates and solution tolerances
  !          stored in parameter arrays indexed by order. The 4th-order case uses a slightly larger
  !          percentage tolerance (2%) compared to the v_dot_grad_f test (1%), reflecting differences
  !          in how the divergence and gradient discretizations accumulate numerical error. For each
  !          order, it constructs low-resolution (500 cells) and high-resolution (501 cells) scalar
  !          and vector fields, computes the volume integral on each, checks the high-resolution
  !          absolute error against a tight solution tolerance, and verifies the convergence rate via
  !          log(lo_res/hi_res)/log(cells_/cells) against the expected order within a percentage
  !          tolerance. The test result is accumulated using the .also., .isAtMost., .approximates.,
  !          and .withinPercentage. defined operations and the passing_test()/test_diagnosis_t
  !          testing infrastructure.
  function check_volume_integral_of_f_div_v() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    double precision, parameter :: x_min = 0D0, x_max = 1D0
    integer, parameter :: cells = 500, cells_ = cells + 1
    double precision, parameter, dimension(*) :: expected  = [0, 2, 0, 1]
    double precision, parameter, dimension(*) :: order_tolerance = [0, 1, 0, 2]
    double precision, parameter, dimension(*) :: solution_tolerance = [0D0, 2D-7, 0D0, 4D-10]
    integer order

    scalar_1D_initializer => parabola
    vector_1D_initializer => line

    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate( &
         f  => scalar_1D_t(scalar_1D_initializer, order, cells , x_min, x_max) &
        ,f_ => scalar_1D_t(scalar_1D_initializer, order, cells_, x_min, x_max) &
        ,v  => vector_1D_t(vector_1D_initializer, order, cells , x_min, x_max) &
        ,v_ => vector_1D_t(vector_1D_initializer, order, cells_, x_min, x_max) &
        ,expected_integral => SSS_f_div_v(x_max) - SSS_f_div_v(x_min) &
      )
        associate( &
           dV  => f%dV() &
          ,dV_ => f_%dV() &
        )
          associate( &
             lo_res => abs( (.SSS. (f  * .div. v ) * dV ) - expected_integral) &
            ,hi_res => abs( (.SSS. (f_ * .div. v_) * dV_) - expected_integral) &
          )
            test_diagnosis = test_diagnosis .also. &
              (hi_res .isAtMost. solution_tolerance(order)) &
              // " for " // ordinal(order) // "-order discretization of .SSS. (f .div. v) * dV"
            associate(calculated_order => log(lo_res/hi_res)/log(dble(cells_)/cells))
              test_diagnosis = test_diagnosis .also. &
                (calculated_order .approximates. expected(order) .withinPercentage. order_tolerance(order)) &
                // " for convergence rate of " // ordinal(order) //  "-order discretization of .SSS. (f * .div. v) * dV"
            end associate
          end associate
        end associate
      end associate
    end do

  end function
  ! END CODE CHUNK

  ! PURPOSE: Tests that the surface integral of the product f times (v dot dA) converges at the
  !          expected rate and produces sufficiently accurate results for both 2nd-order and 4th-order
  !          discretizations. It computes the integral .SS. (f .x. (v .dot. dA)) on two grids for
  !          each order, compares the high-resolution result against a known analytical surface
  !          integral, and verifies that the observed convergence rate matches the expected order of
  !          accuracy.
  ! KEYWORDS: surface-integral, dot-product, scalar-multiplication, finite-difference, convergence-rate,
  !           defined operation, unit-test, scalar_1D, vector_1D, parabola, linear-function,
  !           2nd-order, 4th-order, structured-grid, staggered-grid, test-diagnosis, verification, grid-refinement,
  !           order-of-accuracy, integral-operator, surface-area-element
  ! CONTEXT: This function is part of the operator test suite in the formal library, which provides
  !          defined operations (.grad., .div., .SS., .dot., .x., etc.) for
  !          staggered-grid scalar and vector fields. It exercises a compound expression combining
  !          scalar-vector multiplication, the vector dot product with the surface area element dA,
  !          and the surface integration operator .SS. in a single test. The scalar field f is
  !          initialized as a parabola and the vector field v as a linear function, yielding an
  !          analytically known surface integral computed as parabola(x_max)*line(x_max) minus
  !          parabola(x_min)*line(x_min) on the domain [0, 1]. This test complements the volume
  !          integral tests by verifying surface flux computations, which are related through the
  !          divergence theorem. The test includes compiler-specific conditional compilation: the
  !          Intel compiler uses fewer cells (400/401 vs 500/501) and a looser percentage tolerance
  !          for the 4th-order convergence rate (5% vs 4%), reflecting compiler-specific numerical
  !          differences. The test loops over 2nd-order and 4th-order discretizations, using
  !          order-specific expected convergence rates and solution tolerances stored in parameter
  !          arrays indexed by order. For each order, it constructs low-resolution and high-resolution
  !          scalar and vector fields, computes the surface integral on each, checks the
  !          high-resolution absolute error against a tight solution tolerance, and verifies the
  !          convergence rate via log(lo_res/hi_res)/log(cells_/cells) against the expected order
  !          within a percentage tolerance. The test result is accumulated using the .also.,
  !          .isAtMost., .approximates., and .withinPercentage. defined operations and the
  !          passing_test()/test_diagnosis_t testing infrastructure.
  function check_surface_integral_of_vf() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    double precision, parameter :: x_min = 0D0, x_max = 1D0
#ifndef __INTEL_COMPILER
    integer, parameter :: cells = 500, cells_ = cells+1
    double precision, parameter, dimension(*) :: order_tolerance = [0, 1, 0, 4]
#else
    integer, parameter :: cells = 400, cells_ = cells+1
    double precision, parameter, dimension(*) :: order_tolerance = [0, 1, 0, 5]
#endif
    double precision, parameter, dimension(*) :: expected  = [0, 2, 0, 1]
    double precision, parameter, dimension(*) :: solution_tolerance = [0D0, 2D-6, 0D0, 2D-9]
    integer order

    scalar_1D_initializer => parabola
    vector_1D_initializer => line

    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate( &
         f  => scalar_1D_t(scalar_1D_initializer, order, cells , x_min, x_max) &
        ,f_ => scalar_1D_t(scalar_1D_initializer, order, cells_, x_min, x_max) &
        ,v  => vector_1D_t(vector_1D_initializer, order, cells , x_min, x_max) &
        ,v_ => vector_1D_t(vector_1D_initializer, order, cells_, x_min, x_max) &
        ,expected_integral => parabola([x_max])*line([x_max]) - parabola([x_min])*line([x_min]) &
      )
        associate( &
           dA  => v%dA() &
          ,dA_ => v_%dA() &
        )
          associate( &
             lo_res => abs(.SS. (f  .x. (v  .dot. dA )) - expected_integral(1)) &
            ,hi_res => abs(.SS. (f_ .x. (v_ .dot. dA_)) - expected_integral(1)) &
          )
            test_diagnosis = test_diagnosis .also. &
              (hi_res .isAtMost. solution_tolerance(order)) &
              // " for " // ordinal(order) // "-order discretization of .SS. (f .x. (v .dot. dA))"
            associate(calculated_order => log(lo_res/hi_res)/log(dble(cells_)/cells))
              test_diagnosis = test_diagnosis .also. &
                (calculated_order .approximates. expected(order) .withinPercentage. order_tolerance(order)) &
                // " for convergence rate of " // ordinal(order) //  "-order discretization of .SS. (f .x. (v .dot. dA)))"
            end associate
          end associate
        end associate
      end associate
    end do

  end function
  ! END CODE CHUNK

  pure function quartic(x) result(f)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = (x**4)/4
  end function

  pure function exponential(x) result(v)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: v(:)
    v = exp(x)
  end function

  ! PURPOSE: Tests that the extended Gauss divergence theorem holds discretely by verifying that the
  !          residual of the identity .SSS. (v dot grad f) dV + .SSS. (f div v) dV - .SS. (f v dot dA)
  !          is near zero for both 2nd-order and 4th-order discretizations on a 20-cell 1D domain.
  ! KEYWORDS: gauss-divergence-theorem, volume-integral, surface-integral, gradient, divergence,
  !           dot-product, finite-difference, defined operation, unit-test, scalar_1D, vector_1D,
  !           quartic, exponential, 2nd-order, 4th-order, structured-grid, staggered-grid, test-diagnosis,
  !           verification, integral-identity, conservation
  ! CONTEXT: This function is part of the operator test suite in the formal library, which provides
  !          defined operations (.grad., .div., .SSS., .SS., .dot., .x., etc.) for
  !          staggered-grid scalar and vector fields. It serves as a capstone verification test that
  !          ties together the volume integral of v dot grad(f), the volume integral of f times div(v),
  !          and the surface integral of f times (v dot dA) through the extended Gauss divergence
  !          theorem identity. Unlike the individual volume and surface integral convergence tests
  !          that compare each integral against an analytical result, this test checks that the three
  !          discrete integrals satisfy the theorem's algebraic relationship with a residual below
  !          residual_tolerance, regardless of how closely each individual integral matches its
  !          analytical value. The scalar field f is initialized as a quartic function and the vector
  !          field v as an exponential function, providing a non-trivial test case where neither field
  !          is within the polynomial exactness range of the stencils. A relatively coarse grid of 20
  !          cells is used, emphasizing that the discrete identity should hold even on under-resolved
  !          grids. The test loops over 2nd-order and 4th-order discretizations, computing the
  !          residual for each and checking its absolute value against residual_tolerance. The test
  !          result is accumulated using the .also. and .isAtMost. defined operations and the
  !          passing_test()/test_diagnosis_t testing infrastructure.
  function check_gauss_divergence_theorem() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    integer, parameter :: cells=20
    double precision, parameter :: x_min = 0D0, x_max = 1D0
    integer order

    scalar_1D_initializer => quartic
    vector_1D_initializer => exponential

    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate( &
         f  => scalar_1D_t(scalar_1D_initializer, order, cells , x_min, x_max) &
        ,v  => vector_1D_t(vector_1D_initializer, order, cells , x_min, x_max) &
      )
        associate( &
           dA => v%dA() &
          ,dV => f%dV() &
        )
          associate(residual => (.SSS. (v  .dot. .grad. f )*dV) + (.SSS. (f  * .div. v )*dV) - .SS. (f .x. (v .dot. dA)))
            test_diagnosis = test_diagnosis .also. (abs(residual) .isAtMost. residual_tolerance) &
              // " for " // ordinal(order) // "-order Extended Gauss Divergence Theorem residual"
          end associate
        end associate
      end associate
    end do

  end function
  ! END CODE CHUNK

end module integration_operators_1D_test_m
