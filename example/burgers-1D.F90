! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

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
  !! Advance the 1D Burgers partial differential equation over time.
  use initial_condition_m, only : initial_condition
  use julienne_m, only :  command_line_t
  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i, d_dx, d2_dx2
  implicit none

#ifdef __GFORTRAN__
  procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
#endif
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

  print *, new_line('')
  print *,"   Initial condition"
  print *,"   ================="

  call execute_command_line("grep 'initial_condition =' example/burgers-1D.F90 | grep -v execute_command", wait=.true.)

  order_string = command_line%flag_value("--order")

  if (len(order_string)==0) then 
    order = 2
  else
    read(order_string,"(i1)") order 
  end if

  print *, "order = ", order

  block
#ifndef __GFORTRAN__
    procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
#endif
    double precision, parameter :: dt = 1D0, pi = acos(-1D0)
    type(scalar_1D_t) u

    scalar_1D_initializer => initial_condition
    u = scalar_1D_t(scalar_1D_initializer, order, x_min=0D0, x_max=2*pi, cells=20)
    !dt = u%runge_kutta_2nd_step(nu ,grid_resolution)

    !do while (t<=t_final) ! 2nd-order Runge-Kutta:
      associate(u_half => u + d_dt(u)*dt/2) ! first substep
        u = u + d_dt(u_half)*dt ! second substep
      end associate
     !t = t + dt
    !end do

  end block

#ifdef __GFORTRAN__
    stop
#endif

contains

  pure function d_dt(u) result(du_dt)
    type(scalar_1D_t), intent(in) :: u
    type(scalar_1D_t) du_dt
    double precision, parameter :: nu=1D0
    du_dt = nu*d2_dx2(u) - d_dx((u**2)/2)
  end function

end program
