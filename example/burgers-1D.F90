! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

module initial_condition_m
  implicit none

contains

  pure function initial_condition(x)
    !! Initial solution to Burgers equation
    double precision, intent(in) :: x(:)
    double precision, allocatable :: initial_condition(:)
    initial_condition = 10*sin(x)
    ! To change this function, please edit only the right-hand-side (RHS) expression,
    ! keeping the rest in place for proper display of the function at runtime.
  end function

end module

program burgers_1D
  !! This program demonstrates the use of Formal to solve the partial differential equation of
  !! Burgers (1948) in conservative form using the 2nd- or 4th-order mimetic discretizations of
  !! Corbino & Castillo (2020) and Dumett & Castillo (2022).
  !!
  !! * Burgers, J.M.      (1948) https://doi.org/10.1016/S0065-2156(08)70100-5
  !! * Corbino & Castillo (2020) https://doi.org/10.1016/j.cam.2019.06.042.
  !! * Dumett & Castillo  (2022) https://doi.org/10.13140/RG.2.2.26630.14400
  use initial_condition_m, only : initial_condition
  use julienne_m, only :  command_line_t, csv, call_julienne_assert_, operator(.equalsExpected.), string_t
  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i, d_dx, d2_dx2
  use iso_fortran_env, only : output_unit
  implicit none

  character(len=:), allocatable :: order_string
  type(command_line_t) command_line
  integer order

  if (command_line%argument_present([character(len=len("--help")) :: ("--help"), "-h"])) then
    stop                  new_line('') // new_line('') &
      // 'Usage:'                      // new_line('') &
      // '  fpm run \'                 // new_line('') &
      // '    --example burgers-1D \'  // new_line('') &
      // '    --compiler flang \'      // new_line('') &
      // '    --flag "-O3" \'          // new_line('') & 
      // '    [--help|-h] | [--order <integer>]' // new_line('') // new_line('') &
      // 'where square brackets indicate optional arguments and angular brackets indicate user input values.' // new_line('')
  end if

  print '(a)', new_line('')
  print '(a)',"#   Initial condition"
  print '(a)',"#   ================="

  write(output_unit,'(a)', advance="no") 
  call execute_command_line("grep 'initial_condition =' example/burgers-1D.F90 | grep -v execute_command", wait=.true.)

  order_string = command_line%flag_value("--order")

  if (len(order_string)==0) then 
    order = 4
  else
    read(order_string,"(i1)") order 
  end if


  block
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
    double precision, parameter :: pi = acos(-1D0), nu=1D0, t_final=0.6D0
    double precision, allocatable :: u_surface(:,:), time(:)
    double precision dt
    type(scalar_1D_t) u
    integer step, n

    scalar_1D_initializer => initial_condition
    u = scalar_1D_t(scalar_1D_initializer, order, x_min=0D0, x_max=2*pi, cells=199)
    dt = diffusion_stability_limit(nu, u%dx(), order)

    associate(steps => ceiling(t_final/dt))
      associate(initial_condition => u%values())
        allocate(u_surface(size(initial_condition), steps + 1))
        u_surface(:,1) = initial_condition
      end associate

      runge_kutta: &
      do step = 1, steps
        select case(order)
        case(2)
          associate(u_half => u + d_dt(u,nu)*dt/2) ! first substep
            u = u + d_dt(u_half,nu)*dt ! second substep
            u_surface(:,step) = u%values()
          end associate
        case(4)
          associate(k1 => d_dt(u          , nu))
          associate(k2 => d_dt(u + dt*k1/2, nu))
          associate(k3 => d_dt(u + dt*k1/2, nu))
          associate(k4 => d_dt(u + dt*k2  , nu))
            u = u + (k1 + 2*k2 + 2*k3 + k4)*dt/6
            u_surface(:,step) = u%values()
          end associate
          end associate
          end associate
          end associate
        end select
      end do runge_kutta

      block  
        character(len=64) scratch_pad
        character(len=:), allocatable :: file_name
        character(len=*), parameter :: path = "example/scripts/"
        integer file_unit

        write(scratch_pad,'(a,i1,a)') "burgers-order-", order, ".dat"
        file_name = trim(scratch_pad)
        open(newunit=file_unit, file = path // file_name, status="unknown")
        write(file_unit,'(a)'     ) "#  1D Burgers equation solver results"
        write(file_unit,'(a)'     ) "#  =================================="
        write(file_unit,'(a,i2)'  ) "#  spatial order of accuracy = ", order
        write(file_unit,'(a,g0)'  ) "#  nu = " , nu
        write(file_unit,'(a,g0)'  ) "#  dt = " , dt
        write(file_unit,'(a,i4,a)') "#  steps = ", steps

        associate(x => u%grid())
          do n = 1, size(x)
            write(file_unit,"(*(G13.6,:,'  '))") x(n), u_surface(n,:) ! write space-separated values
          end do
        end associate

        close(file_unit)

        write(*,*)
        write(*,'(a)') "To animate the results, set your present working directory to formal/example/scripts."
        write(*,'(a)') "Then execute the following command:"
        associate(dt_ => string_t(dt), frames => string_t(steps+1))
          write(*,'(a)') new_line('') &
             // 'gnuplot -e "results_file=' // "'" // file_name // "'" // '"' &
             //        ' -e "animation_file=' // "'" // "animated-burgers.gif" // "'" // '"' &
             //        ' -e "dt=' // dt_%string() // '"' &
             //        ' -e "frames=' //   frames%string() // '"' &
             //        " animate-burgers.gnuplot" // new_line('')
        end associate

      end block
    end associate
  end block

contains

  pure function d_dt(u, nu) result(du_dt)
    type(scalar_1D_t), intent(in) :: u
    double precision, intent(in) :: nu
    type(scalar_1D_t) du_dt
    du_dt = nu*d2_dx2(u) - d_dx((u**2)/2)
  end function

  pure function diffusion_stability_limit(diffusivity,delta_x,order_of_accuracy)  result(stable_time_step)
    double precision, intent(in) :: diffusivity, delta_x
    integer, intent(in) :: order_of_accuracy
    double precision stable_time_step
    double precision, parameter, dimension(*) :: stability_limit=[2.,2.,2.5,2.79] ! third value needs to be checked
    double precision, parameter :: safety_factor = 0.9
    ! See Moin, P. (2010) Fundamentals of Engineering Numerical Analysis, 2nd ed., pp. 111-116.
    stable_time_step = safety_factor*stability_limit(order_of_accuracy)*(delta_x**2)/(4*diffusivity)
  end function

end program
