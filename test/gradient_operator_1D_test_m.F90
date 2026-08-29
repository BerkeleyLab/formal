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

  type, extends(test_t) :: gradient_operator_1D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  real, parameter :: tight_tolerance = 1E-4, loose_tolerance = 1E-4, rough_tolerance = 5E-02

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
    real, intent(in) :: x(:)
    real, allocatable :: y(:)
    integer i
    y = [(5E0, i=1,size(x))]
  end function


  function check_grad_const() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: grad_expected = 0.
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => const

    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0E0, x_max=4E0))
      test_diagnosis = passing_test()
      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (2nd-order .grad.(5))"
    end associate

    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0E0, x_max=8E0))
      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (4th-order .grad.(5))"
    end associate
  end function

  pure function line(x) result(y)
    real, intent(in) :: x(:)
    real, allocatable :: y(:)
    y = 14*x + 3
  end function

  function check_grad_line() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: grad_expected = 14E0
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => line

    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=2, cells=16, x_min=0E0, x_max=4E0))
      test_diagnosis = passing_test()
      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (2nd-order .grad.(14*x + 3))"
    end associate

    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer, order=4, cells=16, x_min=0E0, x_max=8E0))
      test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
        // " (4th-order .grad.(14*x + 3))"
    end associate
  end function

  pure function parabola(x) result(y)
    real, intent(in) :: x(:)
    real, allocatable :: y(:)
    y = 7*x**2 + 3*x + 5
  end function

  function check_grad_parabola() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => parabola

    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer , order=2, cells=16, x_min=0E0, x_max=4E0))
      test_diagnosis = passing_test()
      associate(x => grad%grid())
        associate(grad_expected => 14*x + 3)
          test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
            // " (2nd-order .grad.(7*x**2 + 3*x + 5))"
        end associate
      end associate
    end associate

    associate(grad => .grad. scalar_1D_t(scalar_1D_initializer , order=4, cells=16, x_min=0E0, x_max=8E0))
      associate(x => grad%grid())
        associate(grad_expected => 14*x + 3)
          test_diagnosis = test_diagnosis .also. (.all. (grad%values() .approximates. grad_expected .within. loose_tolerance)) &
            // " (4th-order .grad.(7*x**2 + 3*x + 5))"
        end associate
      end associate
    end associate
  end function

  pure function sinusoid(x) result(y)
    real, intent(in) :: x(:)
    real, allocatable :: y(:)
    y = sin(x) + cos(x)
  end function


  function check_2nd_order_grad_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => sinusoid
    real, parameter :: pi = 3.141592653589793E0
    integer, parameter :: order_desired = 2, coarse_cells=64, fine_cells=2*coarse_cells
    associate( &
       grad_coarse => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0E0, x_max=2*pi) &
      ,grad_fine   => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0E0, x_max=2*pi) &
    )
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
            associate(order_actual => log(error_coarse_max/error_fine_max)/log(real(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. real(order_desired) .within. rough_tolerance)  &
                // " (2nd-order .grad. [sin(x) + cos(x)] order of accuracy)"
            end associate
          end associate
        end associate
      end associate
    end associate
  end function

  function check_4th_order_grad_convergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => sinusoid
    real, parameter :: pi = 3.141592653589793E0
    integer, parameter :: order_desired = 4, coarse_cells=32, fine_cells=2*coarse_cells
    associate( &
       grad_coarse => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=coarse_cells, x_min=0E0, x_max=2*pi) &
      ,grad_fine   => .grad. scalar_1D_t(scalar_1D_initializer , order=order_desired, cells=fine_cells  , x_min=0E0, x_max=2*pi) &
    )
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
            associate(order_actual => log(error_coarse_max/error_fine_max)/log(real(fine_cells)/coarse_cells))
              test_diagnosis = test_diagnosis .also. (order_actual .approximates. real(order_desired) .within. 2.1) &
                // " (4th-order d(sinusoid)/dx order of accuracy)"
            end associate
          end associate
        end associate
      end associate
    end associate
  end function

end module
