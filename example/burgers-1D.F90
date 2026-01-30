! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module initial_condition_m
  implicit none

contains

  pure function initial_condition(x)
    !! Initial solution to Burgers equation
    double precision, intent(in) :: x(:)
    double precision, allocatable :: initial_condition(:)
    double precision, parameter :: pi = acos(-1D0)
    initial_condition = sin(2*pi*x)
    ! To change this function, please edit only the right-hand-side (RHS) expression,
    ! keeping the rest in place for proper display of the function at runtime.
  end function

end module

program burgers_1D
  !! Advance the 1D Burgers partial differential equation over time.
  use initial_condition_m, only : initial_condition
  use julienne_m, only :  command_line_t
  use formal_m, only : vector_1D_t, vector_1D_initializer_i
  implicit none

  procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => initial_condition
  character(len=:), allocatable :: order_string
  type(command_line_t) command_line
  type(vector_1D_t) :: u
  integer order

  if (command_line%argument_present([character(len=len("--help")) :: ("--help"), "-h"])) then
    stop                  new_line('') // new_line('') &
      // 'Usage:'                      // new_line('') &
      // '  fpm run \'                 // new_line('') &
      // '  --example burgers-1D \'    // new_line('') &
      // '  --compiler flang-new \'    // new_line('') &
      // '  --flag "-O3" \'            // new_line('') & 
      // '  -- [--help|-h] | [--order <integer>]' // new_line('') // new_line('') &
      // 'where square brackets indicate optional arguments and angular brackets indicate user input values.' // new_line('')
  end if


  print *, new_line('')
  print *,"   Initial condition"
  print *,"   ================="

  call execute_command_line("grep 'initial_condition =' example/burgers-1D.F90 | grep -v execute_command", wait=.true.)

  order_string = command_line%flag_value("--order")

  if (len(order_string)==0) then 
    order = 4 
  else
    read(order_string,"(i)") order 
  end if

  print *, "order = ", order

  u = vector_1D_t(vector_1D_initializer, order, x_min=0D0, x_max=1D0, cells=50)
  associate(div_uu_2 => .div. (u*u/2)) ! result is at cell centers; To Do: interpolate to cell faces
  end associate

#ifdef __GFORTRAN__
    stop
#endif

end program
