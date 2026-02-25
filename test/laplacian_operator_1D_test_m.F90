! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module laplacian_operator_1D_test_m
  use julienne_m, only : &
     file_t &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.separatedBy.) &
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
  use formal_m, only : laplacian_1D_t
#endif
  implicit none

  type, extends(test_t) :: laplacian_operator_1D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  double precision, parameter :: tight_tolerance = 5D-14, loose_tolerance = 1D-09, crude_tolerance = 1D-02

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'A 1D mimetic laplacian operator'
  end function

  function results() result(test_results)
    type(laplacian_operator_1D_test_t) laplacian_operator_1D_test
    type(test_result_t), allocatable :: test_results(:)

    test_results = laplacian_operator_1D_test%run([ & 
       test_description_t( &
          'computing 2nd-order .laplacian. [(x**2)/2] within ' // string_t(tight_tolerance) &
         ,usher(check_2nd_order_laplacian_parabola)) &
      ,test_description_t( &
          'computing 4th-order .laplacian. [(x**4)/12] within ' // string_t(loose_tolerance) &
         ,usher(check_4th_order_laplacian_of_quartic)) &
      ,test_description_t( &
          'converging as dx^2 internally and dx near boundary for 2nd-order .laplacian. sin(x) within ' // string_t(crude_tolerance) &
         ,usher(check_2nd_order_laplacian_convergence)) &
      ,test_description_t( &
          'converging as dx^4 internally and dx^3 near boundary for 4th-order .laplacian. sin(x) within ' // string_t(crude_tolerance) &
         ,usher(check_4th_order_laplacian_convergence)) &
    ])
  end function

  pure function parabola(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = (x**2)/2
  end function

  ! PURPOSE: Tests that the 2nd-order discrete Laplacian operator correctly computes the Laplacian of
  !          a parabolic function (x^2/2), which should yield a constant value of 1.0 everywhere, and
  !          reports a passing or failing test diagnosis based on whether the computed values approximate
  !          the expected analytical result within a tight tolerance.
  ! KEYWORDS: laplacian, finite-difference, defined operation, unit-test, scalar_1D, parabola, 2nd-order,
  !           structured-grid, staggered-grid, test-diagnosis, differential-operator, verification
  ! CONTEXT: This function is part of the Laplacian operator test suite in the formal library, which provides
  !          defined operations (.laplacian., .gradient., etc.) for staggered-grid scalar fields.
  !          It constructs a scalar_1D_t object initialized with a parabola function on a 16-cell 1D domain [0, 5],
  !          applies the .laplacian. operator, and checks that all resulting values match the analytically expected
  !          constant Laplacian of 1.0. The conditional compilation directives handle differences between gfortran
  !          and other compilers regarding associate-block support for user-defined operator results. The test result
  !          is accumulated using the .also. and .approximates. defined operations and the
  !          passing_test()/test_diagnosis_t testing infrastructure.
  function check_2nd_order_laplacian_parabola() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => parabola
    double precision, parameter :: expected_laplacian = 1D0
#ifdef __GFORTRAN__
    type(laplacian_1D_t) laplacian_scalar
    laplacian_scalar = .laplacian. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=5D0)
#else
    associate(laplacian_scalar => .laplacian. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0D0, x_max=5D0))
#endif

      test_diagnosis = passing_test()
      test_diagnosis = test_diagnosis .also. (.all. (laplacian_scalar%values() .approximates. expected_laplacian .within. tight_tolerance)) &
        // " (2nd-order .laplacian. [(x**2)/2]"

#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  pure function quartic(x) result(y)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: y(:)
    y = (x**4)/12
  end function

  ! PURPOSE: Tests that the 4th-order discrete Laplacian operator correctly computes the Laplacian
  !          of a quartic function (x^4/24), which should yield x^2 at each grid point, and reports a passing or
  !          failing test diagnosis based on whether the computed values approximate the expected spatially-varying
  !          analytical result within a loose tolerance.
  ! KEYWORDS: laplacian, finite-difference, defined operation, unit-test, scalar_1D, quartic, 4th-order,
  !           structured-grid, staggered-grid, test-diagnosis, differential-operator, verification, higher-order-accuracy
  ! CONTEXT: This function is part of the Laplacian operator test suite in the formal library, which provides
  !          defined operations (.laplacian., .gradient., etc.) for staggered-grid scalar fields.
  !          It constructs a scalar_1D_t object initialized with a quartic function on a 16-cell 1D domain [0, 40],
  !          applies the .laplacian. operator at 4th-order accuracy, and checks that all resulting values match the
  !          analytically expected spatially-varying Laplacian of x^2. Unlike the 2nd-order parabola test, this test
  !          exercises a higher-order stencil and validates against a non-constant expected result by retrieving the
  !          grid coordinates via laplacian_quartic%grid(). A loose_tolerance is used instead of tight_tolerance,
  !          reflecting the greater numerical challenge of the higher-order polynomial on a coarse grid. The
  !          conditional compilation directives handle differences between gfortran and other compilers regarding
  !          associate-block support for user-defined operator results. The test result is accumulated using
  !          the .also. and .approximates. defined operations and the passing_test()/test_diagnosis_t testing infrastructure.
  function check_4th_order_laplacian_of_quartic() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => quartic

#ifndef __GFORTRAN__
    associate(laplacian_quartic => .laplacian. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=40D0))
#else
    type(laplacian_1D_t) laplacian_quartic
    laplacian_quartic = .laplacian. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0D0, x_max=40D0)
#endif
      associate(x => laplacian_quartic%grid())
        associate(expected_laplacian => x**2, actual_laplacian => laplacian_quartic%values())
          test_diagnosis = passing_test()
          test_diagnosis = test_diagnosis .also. (.all. (actual_laplacian .approximates. expected_laplacian .within. loose_tolerance)) &
            // " (4th-order .laplacian. [(x**4)/24]"
        end associate
      end associate
#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

  pure function f(x)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = sin(x)
  end function

  pure function d2f_dx2(x)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: d2f_dx2(:)
    d2f_dx2 = -sin(x)
  end function

  ! PURPOSE: Wrapper test that invokes the Laplacian convergence check for 2nd-order accuracy
  !          using a coarse grid of 400 cells and a fine grid of 401 cells, returning the
  !          resulting test diagnosis.
  ! KEYWORDS: laplacian, finite-difference, convergence-rate, 2nd-order, unit-test, wrapper,
  !           grid-refinement, verification, test-diagnosis
  ! CONTEXT: This function is a thin wrapper in the Laplacian operator test suite of the formal
  !          library. It delegates to check_laplacian_convergence, supplying 2nd-order accuracy
  !          and specific coarse/fine cell counts (400 and 401). The nearly identical cell counts
  !          yield a small refinement ratio, which is sufficient for estimating the convergence
  !          rate of the 2nd-order Laplacian stencil applied to sin(x) on [0, 2*pi]. By
  !          isolating the parameter choices in a dedicated function, the test suite can register
  !          this case as a standalone test while reusing the shared convergence-checking logic
  !          in check_laplacian_convergence.
  function check_2nd_order_laplacian_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = check_laplacian_convergence(order_desired=2, coarse_cells=400, fine_cells=401)
  end function
  ! END CODE CHUNK

  ! PURPOSE: Wrapper test that invokes the Laplacian convergence check for 4th-order accuracy
  !          using a coarse grid of 150 cells and a fine grid of 151 cells, returning the
  !          resulting test diagnosis.
  ! KEYWORDS: laplacian, finite-difference, convergence-rate, 4th-order, unit-test, wrapper,
  !           grid-refinement, verification, test-diagnosis, higher-order-accuracy
  ! CONTEXT: This function is a thin wrapper in the Laplacian operator test suite of the formal
  !          library. It delegates to check_laplacian_convergence, supplying 4th-order accuracy
  !          and specific coarse/fine cell counts (150 and 151). Compared to the 2nd-order
  !          convergence wrapper, this test uses fewer cells because the higher-order stencil
  !          achieves smaller errors on coarser grids, making convergence detectable with fewer
  !          degrees of freedom. By isolating the parameter choices in a dedicated function, the
  !          test suite can register this case as a standalone test while reusing the shared
  !          convergence-checking logic in check_laplacian_convergence.
  function check_4th_order_laplacian_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    test_diagnosis = check_laplacian_convergence(order_desired = 4, coarse_cells=150, fine_cells=151)
  end function
  ! END CODE CHUNK

  ! PURPOSE: Tests that the discrete Laplacian operator converges at the expected order of accuracy by
  !          comparing coarse-grid and fine-grid solutions of the Laplacian of sin(x) against the
  !          analytical second derivative. It verifies that both grids approximate the expected Laplacian
  !          within a crude tolerance, that the interior convergence rate matches the desired order, and
  !          that the boundary convergence rate matches one order lower than the desired order.
  ! KEYWORDS: laplacian, finite-difference, convergence-rate, defined operation, unit-test, scalar_1D,
  !           structured-grid, staggered-grid, test-diagnosis, differential-operator, verification, grid-refinement,
  !           boundary-accuracy, interior-accuracy, order-of-accuracy, sin-function
  ! CONTEXT: This function is part of the Laplacian operator test suite in the formal library, which
  !          provides defined operations (.laplacian., .gradient., etc.) for staggered-grid
  !          scalar fields. Unlike the parabola and quartic tests that verify correctness against known
  !          analytical results on a single grid, this test performs a grid-refinement convergence study
  !          using a trigonometric function f(x)=sin(x) on the domain [0, 2*pi]. It constructs two
  !          scalar_1D_t objects at the caller-specified order and cell counts (coarse and fine), applies
  !          the .laplacian. operator to both, and computes the maximum absolute error in both the
  !          interior and boundary regions separately. The interior region excludes boundary points up to
  !          a depth returned by reduced_order_boundary_depth(), reflecting that boundary stencils are
  !          one order less accurate than interior stencils. The observed convergence rate is computed as
  !          log(coarse_error/fine_error)/log(fine_cells/coarse_cells) and checked against the desired
  !          order for the interior and desired order minus one for the boundary. The conditional
  !          compilation directives handle differences between gfortran and other compilers regarding
  !          associate-block support for user-defined operator results. The test result is accumulated
  !          using the .also. and .approximates. defined operations and the passing_test()/test_diagnosis_t
  !          testing infrastructure.
  function check_laplacian_convergence(order_desired, coarse_cells, fine_cells) result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => f
    double precision, parameter :: pi = 3.141592653589793D0
    integer, intent(in) :: order_desired, coarse_cells, fine_cells

#ifndef __GFORTRAN__
    associate( &
       laplacian_coarse => .laplacian. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi) &
      ,laplacian_fine   => .laplacian. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi) &
    )
#else
       type(laplacian_1D_t) laplacian_coarse, laplacian_fine
       laplacian_coarse = .laplacian. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0D0, x_max=2*pi)
       laplacian_fine   = .laplacian. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0D0, x_max=2*pi)
#endif
      grids: &
      associate( &
         x_coarse => laplacian_coarse%grid() &
        ,x_fine   => laplacian_fine%grid())

        laplacian_values: &
        associate( &
           expected_coarse => d2f_dx2(x_coarse) &
          ,expected_fine   => d2f_dx2(x_fine) &
          ,actual_coarse => laplacian_coarse%values() &
          ,actual_fine   => laplacian_fine%values() &
          ,depth => laplacian_coarse%reduced_order_boundary_depth()  &
        )
          test_diagnosis = passing_test()
          test_diagnosis = test_diagnosis .also. &
            .all. (actual_coarse .approximates. expected_coarse .within. crude_tolerance) &
            // " (coarse-grid 2nd-order .laplacian. sin(x))"

          test_diagnosis = test_diagnosis .also. &
            (.all. (actual_fine .approximates. expected_fine .within. crude_tolerance)) &
            // " (fine-grid 2nd-order .laplacian. sin(x))"

          check_internal_convergence_rate: &
          associate( &
             coarse_error_max => maxval( abs( &
               actual_coarse(1+depth:size(actual_coarse)-depth) - expected_coarse(1+depth:size(expected_coarse)-depth) &
             )) &
            ,fine_error_max   => maxval( abs( &
               actual_fine(1+depth:size(actual_fine)-depth) - expected_fine(1+depth:size(expected_fine)-depth) &
          )  ))
            associate(order_actual => log(coarse_error_max/fine_error_max)/log(dble(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. dble(order_desired) .within. crude_tolerance) &
                // " (boundary convergence rate as dx^" // string_t(order_desired) // " for .laplacian. sin(x))"
            end associate
          end associate check_internal_convergence_rate

          check_boundary_convergence_rate: &
          associate( &
             coarse_error_max => maxval( abs( &
                [  actual_coarse(1:depth-1),   actual_coarse(size(actual_coarse)-depth+1:)] &
               -[expected_coarse(1:depth-1), expected_coarse(size(actual_coarse)-depth+1:)] &
             )) &
            ,fine_error_max   => maxval( abs( &
                [  actual_fine(1:depth-1),   actual_fine(size(actual_fine)-depth+1:)] &
               -[expected_fine(1:depth-1), expected_fine(size(actual_fine)-depth+1:)] &
          )  ))
            associate(order_actual => log(coarse_error_max/fine_error_max)/log(dble(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. dble(order_desired-1) .within. crude_tolerance) &
                // " (boundary convergence rate as dx^" // string_t(order_desired-1) // " for .laplacian. sin(x))"
            end associate
          end associate check_boundary_convergence_rate

        end associate laplacian_values
      end associate grids
#ifndef __GFORTRAN__
    end associate
#endif
  end function
  ! END CODE CHUNK

end module
