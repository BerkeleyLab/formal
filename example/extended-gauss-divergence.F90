! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

! PURPOSE: User-defined module functions for use in initializing scalar and vector fields on
!          a one-dimensional (1D) staggered grid. Scalar functions will be sampled at cell
!          centers and domian boundaries (domain interval end points in 1D).  Vector functions
!          will be sampled at cell faces (subinterval end points).
! KEYWORDS: module function, scalar, vector
! CONTEXT: 
module integrand_operands_m
  implicit none
contains

  pure function scalar(x) result(f)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = (x**2)/2 ! <-- scalar function
  end function

  pure function vector(x) result(v)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: v(:)
    v = x        ! <-- vector function
  end function

end module
! END CODE CHUNK

program extended_gauss_divergence
  !! Print each term in the following residual formed from the extended Gauss-divergence
  !! theorem using one-dimensional (1D) 4th- (default) and 2nd-order mimetic discretizations:
  !! `residual = .SSS. (v .dot. .grad. f) * dV +.SSS. (f * .div. v) * dV - .SS. (f .x. (v .dot. dA))`
  !! where `.SSS.` and `.SS.` are the 1D equivalents of a volume integral over the whole
  !! domain and a surface integral over a domain boundary of unit area, respectively.

  ! PURPOSE: Import derived types (*_t) and abstract interfaces (*_i) from modules (*_m) that
  !          export the public entities provided by the Formal mimetic abstraction library and
  !          the Julienne correctness-checking framework.  Also import scalar and vector functions
  !          from a user-defined module: integrand_operands_m.
  ! KEYWORDS: derived type, use association, abstract interface
  ! CONTEXT: demonstration of satisfaction of an extended form of the Gauss Divergence Theorem
  
  use julienne_m, only : command_line_t
  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i, vector_1D_t, vector_1D_initializer_i
  use integrand_operands_m, only : scalar, vector
  ! END CODE CHUNK

  implicit none
  ! PURPOSE: Define procedure pointers for 1D scalar- and vector-field initialization functions
  ! KEYWORDS: initial condition, 1D, scalar, vector
  ! CONTEXT: pass as the first argument in a scalar_1D_t or vector_1D_t constructor function invocation
  procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => scalar
  procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => vector
  ! END CODE CHUNK

  type numerical_arguments_t
    !! Define default initializations that can be overridden with the command-line arguments
    !! detailed by the usage information below
    integer :: cells_=200, order_=4
    double precision :: x_min_=0D0, x_max_=1D0
  end type

  type text_flags_t
    logical div_, grad_, vf_
  end type

  type(command_line_t) command_line
  double precision SSS_v_dot_grad_f_dV, SSS_f_div_v_dV, SS_f_v_dot_dA

  if (command_line%argument_present([character(len=len("--help")) :: ("--help"), "-h"])) then
    stop                              new_line('') // new_line('') &
      // 'Usage:'                                  // new_line('') &
      // '  fpm run \'                             // new_line('') &
      // '  --example extended-gauss-divergence \' // new_line('') &
      // '  --compiler flang-new \'                // new_line('') &
      // '  --flag "-O3" \'                        // new_line('') &
      // '  -- [--help|-h] | [[--cells <integer>] [--order <integer>] [--xmin <double precision>] [--xmax <double precision>] [--div|d] [--grad|g] [--vf|f]]' &
      //                              new_line('') // new_line('') &
      // 'where pipes (|) separate square-bracketed optional arguments and angular brackets indicate user input values.' // new_line('')
  end if

  call execute_command_line("grep '<-- scalar' example/extended-gauss-divergence.F90 | grep -v execute_command", wait=.true.)
  call execute_command_line("grep '<-- vector' example/extended-gauss-divergence.F90 | grep -v execute_command", wait=.true.)

#ifdef __GFORTRAN__
  command_line_arguments: &
  block
    type(numerical_arguments_t) args
    args  = get_numerical_arguments()
#else
  command_line_arguments: &
  associate(args => get_numerical_arguments())
#endif
    text_flags: &
    associate(flags => text_flags_t( &
       div_  = command_line%argument_present( [ character(len=len("--div" )) :: "--div" , "-d" ] ) &
      ,grad_ = command_line%argument_present( [ character(len=len("--grad")) :: "--grad", "-g" ] ) &
      ,vf_   = command_line%argument_present( [ character(len=len("--vf"  )) :: "--vf"  , "-f" ] ) &
    ))
      print_all: &
      associate(all_terms => merge(.true., .false., all([flags%div_, flags%grad_, flags%vf_]) .or. .not. any([flags%div_, flags%grad_, flags%vf_])))
        ! PURPOSE: Construct 1D scalar- and vector-field objects with a specified order of accuracy,
        !          number of grid cells, and domain boundaries
        ! KEYWORDS: scalar field, vector field, one-dimensional (1D)
        ! CONTEXT: construct fields for use as operands in vector-calculus expressions
        integrand_factors: &
        associate( &
           f => scalar_1D_t(scalar_1D_initializer, args%order_, args%cells_, args%x_min_, args%x_max_) &
          ,v => vector_1D_t(vector_1D_initializer, args%order_, args%cells_, args%x_min_, args%x_max_) &
        )
        ! END CODE CHUNK
          differential_volume: &
          associate(dV => f%dV())

            if (flags%grad_ .or. all_terms) then
              ! PURPOSE: Evaluate a volume integral over the problem domain with an integrand formed
              !          from the dot product of a vector field v with the gradient of a scalar f
              ! KEYWORDS: volume integral, dot product, gradient
              ! CONTEXT: Use to verfiy the extended Gauss divergence theorem.
              SSS_v_dot_grad_f_dV = .SSS. (v .dot. .grad. f) * dV
              ! END CODE CHUNK
              print '(a,g0)', ".SSS. (v .dot. .grad. f) * dV =  ", SSS_v_dot_grad_f_dV
            end if

            if (flags%div_ .or. all_terms) then
              ! PURPOSE: Evaluate a volume integral over the problem domain with an integrand formed
              !          from the product of a scalar field f with the divergence of a vector field v
              ! KEYWORDS: volume integral, divergence
              ! CONTEXT: Use to verfiy the extended Gauss divergence theorem.
              SSS_f_div_v_dV      = .SSS. (f * .div. v) * dV
              ! END CODE CHUNK
              print '(a,g0)', ".SSS. (     f * .div. v) * dV =  ", SSS_f_div_v_dV
            end if

          end associate differential_volume

          differential_area: &
          associate(dA => v%dA())
            if (flags%vf_ .or. all_terms) then
              ! PURPOSE: Evaluate a boundary surface integral representing the flux of a vector field formed
              !         from the product of a scalar field f and a vector field v
              ! KEYWORDS: surface integral, flux
              ! CONTEXT: Use to verfiy the extended Gauss divergence theorem.
              SS_f_v_dot_dA     =  .SS. (f .x. (v .dot. dA))
              ! END CODE CHUNK
              print '(a,g0)', "   -.SS. (f .x. (v .dot. dA)) = ", -SS_f_v_dot_dA
            end if

            if (all_terms) then
              print '(a)'   , "----------------------------------------------------"
              ! PURPOSE: Verify satisfaction of the Extended Gauss Divergence Theorem by computing a residual
              !          formed from the two volume integrals and one surface integral in the theorem.
              ! KEYWORDS: Extended Gauss Divergence Theorem, residual
              ! CONTEXT: A small resudual verifies the satisfaction of the Extended Guass Divergence Theorem
              print '(26x,a,g0,a)',"sum = ", SSS_v_dot_grad_f_dV  +  SSS_f_div_v_dV - SS_f_v_dot_dA, " (residual)"
              ! END CODE CHUNK
            end if

          end associate differential_area
        end associate integrand_factors
      end associate print_all
    end associate text_flags
#ifndef __GFORTRAN__
  end associate command_line_arguments
#else
  end block command_line_arguments
#endif

contains

  function get_numerical_arguments() result(numerical_arguments)
    type(numerical_arguments_t) numerical_arguments

#ifdef __GFORTRAN__
    character(len=:), allocatable :: cells_string, order_string, x_min_string, x_max_string
    cells_string = command_line%flag_value("--cells")
    order_string = command_line%flag_value("--order")
    x_min_string = command_line%flag_value("--x_min")
    x_max_string = command_line%flag_value("--x_max")
#else
    associate( &
       cells_string => command_line%flag_value("--cells") &
      ,order_string => command_line%flag_value("--order") &
      ,x_min_string => command_line%flag_value("--x_min") &
      ,x_max_string => command_line%flag_value("--x_max") &
    )
#endif
      if (len(cells_string)/=0) read(cells_string,*) numerical_arguments%cells_
      if (len(order_string)/=0) read(order_string,*) numerical_arguments%order_
      if (len(x_min_string)/=0) read(x_min_string,*) numerical_arguments%x_min_
      if (len(x_max_string)/=0) read(x_max_string,*) numerical_arguments%x_max_
#ifndef __GFORTRAN__
    end associate
#endif

  end function

end program
