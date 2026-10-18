! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program validate_advection_inputs
    use formal_advection_workspace, only: fa_config_t, fa_validate_cfg, fa_validate_working_set
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf
    implicit none
    type(fa_config_t) :: cfg
    character(len=64) :: mode, argument
    integer :: nx, ny, ios

    ! This host-only harness never allocates fields or calls the solver.
    call get_command_argument(1, mode)
    select case (trim(mode))
    case ("working-set")
        if (command_argument_count() /= 3) error stop "expected working-set nx ny"
        call get_command_argument(2, argument)
        read(argument, *, iostat=ios) nx
        if (ios /= 0) error stop "invalid nx"
        call get_command_argument(3, argument)
        read(argument, *, iostat=ios) ny
        if (ios /= 0) error stop "invalid ny"
        call fa_validate_working_set(nx, ny)
    case ("timestep")
        if (command_argument_count() /= 2) error stop "expected timestep dt"
        call get_command_argument(2, argument)
        select case (trim(argument))
        case ("nan")
            cfg%dt = ieee_value(cfg%dt, ieee_quiet_nan)
        case ("inf", "+inf")
            cfg%dt = ieee_value(cfg%dt, ieee_positive_inf)
        case ("-inf")
            cfg%dt = ieee_value(cfg%dt, ieee_negative_inf)
        case default
            read(argument, *, iostat=ios) cfg%dt
            if (ios /= 0) error stop "invalid timestep"
        end select
        call fa_validate_cfg(cfg, .true.)
    case ("defaults")
        if (command_argument_count() /= 1) error stop "expected defaults"
        call fa_validate_cfg(cfg, .false.)
        call fa_validate_working_set(cfg%nx, cfg%ny)
    case default
        error stop "expected working-set, timestep or defaults"
    end select
    print '(a)', "input validation passed"
end program validate_advection_inputs
