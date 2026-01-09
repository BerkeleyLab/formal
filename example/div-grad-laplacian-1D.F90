module functions_m
  implicit none

contains

  pure function f(x)
    !! Function for differentiation
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = (x**3)/6 + (x**2)/2 + 1
    ! To change functions, edit only the right-hand-side (RHS) expression. 
    ! Please keep the rest place for proper display of the function at runtime.
  end function

  double precision elemental function df_dx(x)
    !! 1st-derivative function
    double precision, intent(in) :: x
    df_dx = (x**2)/2 + x
    ! To change derivative functions, edit only the RHS.
    ! Please keep the rest place for proper display of the function at runtime.
    ! Also, ensure the new RHS expresses the first derivative of f above.
  end function

  double precision elemental function d2f_dx2(x)
    !! 2nd-derivative function
    double precision, intent(in) :: x
    d2f_dx2 = x + 1
    ! To edit the above function, edit only the right-hand side expression,
    ! Please keep the rest place for proper display of the function at runtime.
    ! Also, ensure the new RHS expresses the second derivative of f above.
  end function

end module functions_m

program div_grad_laplacian_1D
  !! Compute the 2nd- and 4th-order mimetic approximations to the gradient,
  !! divergence, and Laplacian of the above function f(x) on a 1D uniform,
  !! staggered grid.
  use functions_m, only : f, df_dx, d2f_dx2
  use julienne_m, only :  file_t, string_t, operator(.separatedBy.), command_line_t
  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i
#ifdef __GFORTRAN__
  use formal_m, only : vector_1D_t, laplacian_1D_t, gradient_1D_t
#endif
  implicit none

  procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => f
  character(len=:), allocatable :: order
  type(command_line_t) command_line
  integer row

  if (command_line%argument_present([character(len=len("--help")) :: ("--help"), "-h"])) then
    stop                             new_line('') // new_line('') &
      // 'Usage:'                                 // new_line('') &
      // '  fpm run \'                            // new_line('') &
      // '  --example div-grad-laplacian-1D \'    // new_line('') &
      // '  --compiler flang-new \'               // new_line('') &
      // '  --flag "-O3" \'                       // new_line('') & 
      // '  -- [--help|-h] | [--order <integer>]' // new_line('') // new_line('') &
      // 'where square brackets indicate optional arguments and angular brackets indicate user input values.' // new_line('')
  end if

  order = command_line%flag_value("--order")

  print *,new_line('')
  print *,"   Functions"
  print *,"   ========================"
  call execute_command_line("grep 'f =' example/div-grad-laplacian-1D.F90 | grep -v execute_command", wait=.true.)
  call execute_command_line("grep 'df_dx =' example/div-grad-laplacian-1D.F90 | grep -v execute_command", wait=.true.)
  call execute_command_line("grep 'd2f_dx2 =' example/div-grad-laplacian-1D.F90 | grep -v execute_command", wait=.true.)

  if (len(order)==0 .or. order=="2") then 
    print *,new_line('')
    print *,"   2nd-order approximations"
    print *,"   ========================"
    call output(order=2)
  end if

  if (len(order)==0 .or. order=="4") then
    print *,new_line('')
    print *,"   4th-order approximations"
    print *,"   ========================"
    call output(order=4)
  end if


#ifdef __GFORTRAN__
    stop
#endif

contains

#ifndef __GFORTRAN__

  subroutine output(order)
    integer, intent(in) :: order
  
    associate(   s           => scalar_1D_t(scalar_1D_initializer, order=order, cells=20, x_min=0D0, x_max=20D0))
      associate( grad_s      => .grad. s &
                ,laplacian_s => .laplacian. s)
        associate( s_grid           => s%grid()      &
                  ,grad_s_grid      => grad_s%grid() &
                  ,laplacian_s_grid => laplacian_s%grid())
          associate( s_table           => tabulate( &
                        string_t([character(len=22)::"x", "f(x) expected"         , "f(x) actual"         ]) &
                       ,s_grid, f(s_grid), s%values() &
                     ) &
                    ,grad_s_table      => tabulate( &
                       string_t([character(len=22)::"x", ".grad. f expected"     , ".grad. f actual"     ])  &
                      ,grad_s_grid, df_dx(grad_s_grid), grad_s%values() &
                     ) &
                    ,laplacian_s_table => tabulate( &
                       string_t([character(len=22)::"x", ".laplacian. f expected", ".laplacian. f actual"])  &
                      ,laplacian_s_grid, d2f_dx2(laplacian_s_grid), laplacian_s%values()) &
                     )
             call s_table%write_lines()
             call grad_s_table%write_lines()
             call laplacian_s_table%write_lines()
          end associate
        end associate
      end associate
    end associate
  end subroutine

#else

  subroutine output(order)
    integer, intent(in) :: order
  
    type(scalar_1D_t) s
    type(gradient_1D_t) grad_s
    type(laplacian_1D_t) laplacian_s
    type(file_t) s_table, grad_s_table, laplacian_s_table
    double precision, allocatable,dimension(:) :: s_grid, grad_s_grid, laplacian_s_grid

    s = scalar_1D_t(scalar_1D_initializer, order=order, cells=20, x_min=0D0, x_max=20D0)
    grad_s = .grad. s
    laplacian_s = .laplacian. s

    s_grid = s%grid()
    grad_s_grid = grad_s%grid()
    laplacian_s_grid = laplacian_s%grid()

    s_table           = tabulate( &
       string_t([character(len=22)::"x", "f(x) expected"         , "f(x) actual"         ]) &
      ,s_grid, f(s_grid), s%values() &
    )
    grad_s_table      = tabulate( &
       string_t([character(len=22)::"x", ".grad. f expected"     , ".grad. f actual"     ]) &
      ,grad_s_grid, df_dx(grad_s_grid), grad_s%values() &
    )
    laplacian_s_table = tabulate( &
       string_t([character(len=22)::"x", ".laplacian. f expected", ".laplacian. f actual"]) &
      ,laplacian_s_grid, d2f_dx2(laplacian_s_grid), laplacian_s%values() &
    )
    call s_table%write_lines()
    call grad_s_table%write_lines()
    call laplacian_s_table%write_lines()
  end subroutine

#endif

  pure function tabulate(headings, abscissa, expected, actual) result(file)
    double precision, intent(in), dimension(:) :: abscissa, expected, actual
    type(string_t), intent(in) :: headings(:)
    type(file_t) file
    integer line

    file = file_t([ &
       string_t("") &
      ,headings .separatedBy. "  " &
      ,string_t("------------------------------------------------------------------") &
      ,[( string_t(abscissa(line)) // "          " // string_t(expected(line)) // "          " // string_t(actual(line)), line = 1, size(abscissa))] &
    ])
  end function

end program
