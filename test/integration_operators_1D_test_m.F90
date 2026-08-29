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
  real, parameter :: residual_tolerance = 1E-5

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
    real, intent(in) :: x(:)
    real, allocatable :: f(:)
    f = (x**2)/2
  end function

  pure function line(x) result(v)
    real, intent(in) :: x(:)
    real, allocatable :: v(:)
    v = x
  end function

  pure function SSS_v_dot_grad_f(x) result(integral)
    real, intent(in) :: x
    real integral 
    integral = (x**3)/3
  end function

  pure function SSS_f_div_v(x) result(integral)
    real, intent(in) :: x
    real integral 
    integral = (x**3)/6
  end function

  function check_volume_integral_of_v_dot_grad_f() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    real, parameter :: x_min = 0E0, x_max = 1E0
    integer, parameter :: cells = 32, cells_ = 2*cells
    real, parameter, dimension(*) :: expected  = [0, 2, 0, 1]
    real, parameter, dimension(*) :: order_tolerance = [0, 25, 0, 25]
    real, parameter, dimension(*) :: solution_tolerance = [0E0, 1E-3, 0E0, 1E-5]
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
            if (hi_res > 1.0E-6 .and. lo_res > 1.0E-6) then
              associate(calculated_order => log(lo_res/hi_res)/log(real(cells_)/cells))
                test_diagnosis = test_diagnosis .also. &
                  (calculated_order .approximates. expected(order) .withinPercentage. order_tolerance(order)) &
                  // " for convergence rate of " // ordinal(order) //  "-order discretization of .SSS. (v .dot. .grad. f) * dV"
              end associate
            end if
            end associate
          end associate
      end associate
    end do

  end function

  function check_volume_integral_of_f_div_v() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    real, parameter :: x_min = 0E0, x_max = 1E0
    integer, parameter :: cells = 32, cells_ = 2*cells
    real, parameter, dimension(*) :: expected  = [0, 2, 0, 1]
    real, parameter, dimension(*) :: order_tolerance = [0, 25, 0, 25]
    real, parameter, dimension(*) :: solution_tolerance = [0E0, 1E-3, 0E0, 1E-5]
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
            if (hi_res > 1.0E-6 .and. lo_res > 1.0E-6) then
              associate(calculated_order => log(lo_res/hi_res)/log(real(cells_)/cells))
                test_diagnosis = test_diagnosis .also. &
                  (calculated_order .approximates. expected(order) .withinPercentage. order_tolerance(order)) &
                  // " for convergence rate of " // ordinal(order) //  "-order discretization of .SSS. (f * .div. v) * dV"
              end associate
            end if
          end associate
        end associate
      end associate
    end do

  end function

  function check_surface_integral_of_vf() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    real, parameter :: x_min = 0E0, x_max = 1E0
    integer, parameter :: cells = 32, cells_ = 2*cells
    real, parameter, dimension(*) :: order_tolerance = [0, 25, 0, 25]
    real, parameter, dimension(*) :: expected  = [0, 2, 0, 1]
    real, parameter, dimension(*) :: solution_tolerance = [0E0, 1E-3, 0E0, 1E-5]
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
            if (hi_res > 1.0E-6 .and. lo_res > 1.0E-6) then
              associate(calculated_order => log(lo_res/hi_res)/log(real(cells_)/cells))
                test_diagnosis = test_diagnosis .also. &
                  (calculated_order .approximates. expected(order) .withinPercentage. order_tolerance(order)) &
                  // " for convergence rate of " // ordinal(order) //  "-order discretization of .SS. (f .x. (v .dot. dA)))"
              end associate
            end if
          end associate
        end associate
      end associate
    end do

  end function

  pure function quartic(x) result(f)
    real, intent(in) :: x(:)
    real, allocatable :: f(:)
    f = (x**4)/4
  end function

  pure function exponential(x) result(v)
    real, intent(in) :: x(:)
    real, allocatable :: v(:)
    v = exp(x)
  end function

  function check_gauss_divergence_theorem() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer
    integer, parameter :: cells=20
    real, parameter :: x_min = 0E0, x_max = 1E0
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

end module integration_operators_1D_test_m
