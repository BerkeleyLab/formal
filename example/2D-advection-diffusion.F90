! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module fields_m
  implicit none

  integer, parameter :: space_dimension = 2

contains

  pure function scalar_field(x,y) result(gaussian)
    real, intent(in) :: x(:), y(:)
    real gaussian(size(x),size(y))
    real, parameter :: pi = acos(-1E0)
    real, parameter :: x0 = -pi/2, y0 = -pi/2, sigma = pi/8
    do concurrent(integer :: j=1:size(y)) default(none) shared(x,y,gaussian)
      associate(r => sqrt((x-x0)**2 + (y(j)-y0)**2))
        gaussian(:,j) = exp(-(r**2)/(2*sigma**2))
      end associate
    end do
  end function

  pure function taylor_green_velocity(x,y) result(velocity)
    real, intent(in) :: x(:), y(:)
    real velocity(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      velocity(i,j,:) = [10*sin(x(i))*cos(y(j)), -10*cos(x(i))*sin(y(j))]
    end do
  end function

end module

program advection_diffusion_2D
  !! Solve the advection-diffusion equation for a passive scalar moving through a
  !! static velocity field define by 2D Taylor-Green vortices.
  use formal_advection_workspace, only : &
     fa_advance_rk4 &
    ,fa_fields_t &
    ,fa_init_from_arrays &
    ,fa_read_raw_input &
    ,fa_validate_cfg &
    ,fa_validate_working_set &
    ,fa_reconstruct_scalar &
    ,fa_write_final_raw
  use fields_m, only : scalar_field, taylor_green_velocity
  use gpu_benchmark_m, only : gpu_benchmark_begin, gpu_benchmark_end, gpu_benchmark_stats_t
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i, x_dir, y_dir
  use iso_fortran_env, only : int64, real32, real64, output_unit
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
  use julienne_m, only : command_line_t
  implicit none

  integer, parameter :: default_order = 4
  integer, parameter :: solver_workspace = 1, solver_operators = 2
  real, parameter :: default_dt = 1E-4
  real, parameter :: pi = acos(-1E0)
  real, parameter :: x_min(2) = [-pi,-pi], x_max(2) = [pi,pi]
  type numerical_arguments_t
    integer :: order = default_order
    integer :: steps = 500
    real :: dt = default_dt
    integer :: cells(2) = [51,51]
    integer :: solver = 0
    logical :: benchmark = .false.
    logical :: no_csv = .false.
    logical :: dt_supplied = .false.
    logical :: cells_supplied = .false.
    logical :: steps_supplied = .false.
    character(len=:), allocatable :: input_path
    character(len=:), allocatable :: dump_final_raw_path
  end type
  type(command_line_t) command_line
  type(numerical_arguments_t) args
  type(gpu_benchmark_stats_t) warmup_gpu_stats, solve_gpu_stats
  type(fa_fields_t) workspace_fields
  procedure(scalar_2D_initializer_i), pointer :: scalar_2D_initializer
  procedure(vector_2D_initializer_i), pointer :: velocity_2D_initializer
  type(scalar_2D_t) s, s_initial, s_final, warmup_rhs
  type(vector_2D_t) v
  real(real32), allocatable :: workspace_initial(:,:)
  integer(int64) :: clock_rate, setup_start, setup_end, initial_io_start, initial_io_end
  integer(int64) :: solve_start, solve_end, final_io_start, final_io_end
  real(real64) :: setup_seconds, initial_io_seconds, solve_seconds, final_io_seconds
  real(real64) :: initial_checksum, final_checksum, l_inf_change, relative_l2_change, warmup_checksum, warmup_jit_seconds
  real(real64) :: initial_norm_squared, difference_norm_squared
  logical :: initial_all_finite, final_all_finite
  logical :: gpu_profile_requested
  character(len=:), allocatable :: dump_input_path
  integer :: step

  if (command_line%argument_present([character(len=len("--help")) :: ("--help"), "-h"])) then
    stop new_line('') // new_line('') &
      // 'Usage:' // new_line('') &
      // '  fpm run \' // new_line('') &
      // '    --example 2D-advection-diffusion \' // new_line('') &
      // '    --compiler flang \' // new_line('') &
      // '    --flag "-O3" \' // new_line('') &
      // '    -- [--solver workspace|operators] [--cells <n[,m]>] [--steps <integer>] [--dt <real>] [--order <integer>] [--benchmark] [--no-csv] [--dump-input <path>] [--dump-final-raw <path>] [--input <path>] [--gpu-profile]' // new_line('') &
      // '  or use the no-argument legacy regression case (51x51, 500 steps, dt = 1e-4).' // new_line('') // new_line('') &
      // 'When --dt is omitted, the default time step scales as 1e-4 * min(51/Nx, 51/Ny)^2.' // new_line('') // new_line('') &
      // 'Use --benchmark for the 257x257, 30000-step preset; explicit --cells, --steps, and --dt override the preset.' // new_line('') // new_line('') &
      // 'The workspace solver is the default for order 4; order 2 defaults to operators and rejects workspace explicitly.' // new_line('') // new_line('') &
      // 'When --dump-input is set, the initial state is written before warmup/timing in the native FADV2D1 raw format.' // new_line('') // new_line('') &
      // 'When --dump-final-raw is set, the final scalar field is written in the same raw format for binary comparison.' // new_line('') // new_line('') &
      // 'When --input is set, raw input mode is intended for workspace validation with --no-csv.' // new_line('') // new_line('') &
      // 'When --gpu-profile is enabled, set LFORTRAN_GPU_PROFILE=1 to collect Metal telemetry.' // new_line('') // new_line('') &
      // 'Examples:' // new_line('') &
      // '  fpm run --example 2D-advection-diffusion -- --cells 129,129 --steps 2000 --dt 2e-5' // new_line('') &
      // '  fpm run --example 2D-advection-diffusion -- --cells 51,51 --steps 500 --dt 1e-4 --gpu-profile' // new_line('')
  end if

  args = get_numerical_arguments()
  dump_input_path = command_line%flag_value("--dump-input")
  if (.not. allocated(dump_input_path)) dump_input_path = ""
  if (.not. allocated(args%dump_final_raw_path)) args%dump_final_raw_path = ""
  gpu_profile_requested = command_line%argument_present(["--gpu-profile"])
  if (args%order == 2 .and. ((allocated(dump_input_path) .and. len(dump_input_path) /= 0) .or. &
                             (allocated(args%dump_final_raw_path) .and. len(args%dump_final_raw_path) /= 0))) then
    error stop "raw dumps require --order 4"
  end if

  scalar_2D_initializer => scalar_field
  velocity_2D_initializer => taylor_green_velocity

  call system_clock(count_rate=clock_rate)
  if (clock_rate <= 0_int64) error stop "system_clock returned non-positive rate"

  call system_clock(count=setup_start)
  if (allocated(args%input_path)) then
    if (.not. args%no_csv) error stop "--input currently requires --no-csv"
    call fa_read_raw_input(args%input_path, workspace_fields, args%cells(1), args%cells(2), args%steps, args%dt)
    if (args%order /= default_order) error stop "--input requires order 4"
  else
    s = scalar_2D_t( &
       scalar_2D_initializer &
      ,order = args%order &
      ,cells = args%cells &
      ,x_min = x_min &
      ,x_max = x_max &
    )
    v = vector_2D_t(velocity_2D_initializer, mold=s)
    s_initial = s
    call fa_init_from_arrays(workspace_fields, &
       real(s_initial%values(), kind=real32), &
       real(v%values(x_dir), kind=real32), &
       real(v%values(y_dir), kind=real32))
  end if
  workspace_initial = workspace_fields%base
  call system_clock(count=setup_end)

  initial_all_finite = all(ieee_is_finite(real(workspace_initial, kind=real64)))
  if (.not. initial_all_finite) error stop "initial scalar field contains non-finite values"
  if (.not. allocated(args%input_path)) then
    call system_clock(count=initial_io_start)
    if (allocated(dump_input_path) .and. len(dump_input_path) /= 0) call write_native_input(dump_input_path, s_initial, v, args)
    if (.not. args%no_csv) then
      associate(scalar_file => s_initial%to_file("scalar"))
        call scalar_file%write_lines("example/scripts/scalar-initial.csv")
      end associate
      associate(velocity_file => v%to_file("velocity"))
        call velocity_file%write_lines("example/scripts/velocity.csv")
      end associate
    end if
    call system_clock(count=initial_io_end)
  end if

  warmup_jit_seconds = 0E0
  if (gpu_profile_requested) then
    call gpu_benchmark_begin(warmup_gpu_stats)
    if (.not. warmup_gpu_stats%gpu_timing_available) error stop "gpu profiling requested but unavailable"
  end if
  if (args%solver == solver_workspace) then
    call fa_advance_rk4(workspace_fields, args%cells(1), args%cells(2), 1, args%dt, pi, 0.5E0)
  else
    warmup_rhs = s
    call advance_one_step(warmup_rhs, v, args%dt)
  end if
  if (args%solver == solver_workspace) then
    warmup_checksum = sum(real(workspace_fields%base, kind=real64))
  else
    warmup_checksum = sum(real(warmup_rhs%values(), kind=real64))
  end if
  if (.not. ieee_is_finite(warmup_checksum)) error stop "warmup step contains non-finite values"
  if (gpu_profile_requested) then
    call gpu_benchmark_end(warmup_gpu_stats)
    warmup_jit_seconds = warmup_gpu_stats%jit_seconds
  end if
  workspace_fields%base = workspace_initial

  if (args%solver == solver_workspace) then
    if (gpu_profile_requested) then
      call gpu_benchmark_begin(solve_gpu_stats)
      if (.not. solve_gpu_stats%gpu_timing_available) error stop "gpu profiling requested but unavailable"
    end if
    call system_clock(count=solve_start)
    call fa_advance_rk4(workspace_fields, args%cells(1), args%cells(2), args%steps, args%dt, pi, 0.5E0)
    if (gpu_profile_requested) call gpu_benchmark_end(solve_gpu_stats)
    call system_clock(count=solve_end)
  else
    s = s_initial
    if (gpu_profile_requested) then
      call gpu_benchmark_begin(solve_gpu_stats)
      if (.not. solve_gpu_stats%gpu_timing_available) error stop "gpu profiling requested but unavailable"
    end if
    call system_clock(count=solve_start)
    do step = 1, args%steps
      call advance_one_step(s, v, args%dt)
    end do
    if (gpu_profile_requested) call gpu_benchmark_end(solve_gpu_stats)
    call system_clock(count=solve_end)
    workspace_fields%base = real(s%values(), kind=real32)
    s_final = s
  end if

  final_all_finite = all(ieee_is_finite(real(workspace_fields%base, kind=real64)))
  if (.not. final_all_finite) error stop "final scalar field contains non-finite values"
  initial_checksum = sum(real(workspace_initial, kind=real64))
  final_checksum = sum(real(workspace_fields%base, kind=real64))
  l_inf_change = maxval(abs(real(workspace_fields%base, kind=real64) - real(workspace_initial, kind=real64)))
  initial_norm_squared = sum(real(workspace_initial, kind=real64)**2)
  difference_norm_squared = sum((real(workspace_fields%base, kind=real64) - real(workspace_initial, kind=real64))**2)
  if (initial_norm_squared > 0E0) then
    relative_l2_change = sqrt(difference_norm_squared / initial_norm_squared)
  else
    if (difference_norm_squared /= 0E0) error stop "zero scalar field changed"
    relative_l2_change = 0E0
  end if

  if (.not. allocated(args%input_path)) then
    call system_clock(count=final_io_start)
    if (.not. args%no_csv) then
      if (args%solver == solver_workspace) then
        s_final = fa_reconstruct_scalar(workspace_fields%base, args%order, args%cells, x_min, x_max)
      else
        s_final = s
      end if
      associate(scalar_file => s_final%to_file("scalar"))
        call scalar_file%write_lines("example/scripts/scalar-final.csv")
      end associate
    end if
    call system_clock(count=final_io_end)
  end if

  if (allocated(args%dump_final_raw_path) .and. len(args%dump_final_raw_path) /= 0) then
    call fa_write_final_raw(args%dump_final_raw_path, workspace_fields, args%cells(1), args%cells(2), args%steps, args%dt)
  end if

  setup_seconds = real(setup_end - setup_start, kind=real64) / real(clock_rate, kind=real64)
  if (.not. allocated(args%input_path)) then
    initial_io_seconds = real(initial_io_end - initial_io_start, kind=real64) / real(clock_rate, kind=real64)
    final_io_seconds = real(final_io_end - final_io_start, kind=real64) / real(clock_rate, kind=real64)
  else
    initial_io_seconds = 0E0
    final_io_seconds = 0E0
  end if
  solve_seconds = real(solve_end - solve_start, kind=real64) / real(clock_rate, kind=real64)

  write(output_unit,'(a)') "configuration:"
  write(output_unit,'(a,i0)') "  order = ", args%order
  write(output_unit,'(a,i0,1x,i0)') "  cells = ", args%cells(1), args%cells(2)
  write(output_unit,'(a,i0)') "  steps = ", args%steps
  write(output_unit,'(a,es16.8)') "  dt = ", args%dt
  write(output_unit,'(a,1x,a)') "solver =", merge("workspace","operators", args%solver == solver_workspace)
  write(output_unit,'(a,1x,g0)') "setup_seconds =", setup_seconds
  write(output_unit,'(a,1x,g0)') "initial_io_seconds =", initial_io_seconds
  write(output_unit,'(a,1x,g0)') "solve_seconds =", solve_seconds
  write(output_unit,'(a,1x,g0)') "final_io_seconds =", final_io_seconds
  write(output_unit,'(a,1x,l1)') "initial_all_finite =", all(ieee_is_finite(real(workspace_initial, kind=real64)))
  write(output_unit,'(a,1x,l1)') "final_all_finite =", final_all_finite
  write(output_unit,'(a,1x,g0)') "initial_checksum =", initial_checksum
  write(output_unit,'(a,1x,g0)') "final_checksum =", final_checksum
  write(output_unit,'(a,1x,g0)') "field_linf_change =", l_inf_change
  write(output_unit,'(a,1x,g0)') "field_relative_l2_change =", relative_l2_change
  if (allocated(dump_input_path) .and. len(dump_input_path) /= 0) write(output_unit,'(a,1x,a)') "dump_input_path =", dump_input_path
  if (allocated(args%dump_final_raw_path) .and. len(args%dump_final_raw_path) /= 0) write(output_unit,'(a,1x,a)') "dump_final_raw_path =", args%dump_final_raw_path
  if (gpu_profile_requested) then
    write(output_unit,'(a,1x,g0)') "warmup_jit_seconds =", warmup_jit_seconds
    write(output_unit,'(a,1x,l1)') "gpu_timing_available =", solve_gpu_stats%gpu_timing_available
    write(output_unit,'(a,1x,i0)') "gpu_completed_command_buffers =", solve_gpu_stats%completed_command_buffers
    write(output_unit,'(a,1x,i0)') "gpu_commits =", solve_gpu_stats%commits
    write(output_unit,'(a,1x,i0)') "gpu_dispatches =", solve_gpu_stats%dispatches
    write(output_unit,'(a,1x,i0)') "gpu_host_syncs =", solve_gpu_stats%host_syncs
    write(output_unit,'(a,1x,i0)') "gpu_host_waits =", solve_gpu_stats%host_waits
    write(output_unit,'(a,1x,i0)') "gpu_memcpy_h2d_bytes =", solve_gpu_stats%memcpy_h2d_bytes
    write(output_unit,'(a,1x,i0)') "gpu_memcpy_d2h_bytes =", solve_gpu_stats%memcpy_d2h_bytes
    write(output_unit,'(a,1x,i0)') "gpu_buffer_allocations =", solve_gpu_stats%buffer_allocations
    write(output_unit,'(a,1x,i0)') "gpu_buffer_alloc_bytes =", solve_gpu_stats%buffer_alloc_bytes
    write(output_unit,'(a,1x,g0)') "gpu_elapsed_seconds =", solve_gpu_stats%gpu_elapsed_seconds
    write(output_unit,'(a,1x,g0)') "gpu_busy_seconds =", solve_gpu_stats%gpu_busy_seconds
    write(output_unit,'(a,1x,g0)') "gpu_jit_seconds =", solve_gpu_stats%jit_seconds
  end if

contains

  pure function d_dt(s, v) result(ds_dt)
    type(scalar_2D_t), intent(in) :: s
    type(vector_2D_t), intent(in) :: v
    type(scalar_2D_t) ds_dt
    real, parameter :: D = 0.5E0
    ds_dt = .div. (D * .grad. s) - .div. (v * s)
  end function

  subroutine advance_one_step(state, velocity, dt)
    type(scalar_2D_t), intent(inout) :: state
    type(vector_2D_t), intent(in) :: velocity
    real, intent(in) :: dt
    associate(k1 => d_dt(state, velocity))
      associate(k2 => d_dt(state + (dt/2)*k1, velocity))
        associate(k3 => d_dt(state + (dt/2)*k2, velocity))
          associate(k4 => d_dt(state + dt*k3, velocity))
            state = state + (dt/6)*(k1 + 2*k2 + 2*k3 + k4)
          end associate
        end associate
      end associate
    end associate
  end subroutine

  subroutine reconstruct_final_scalar(values, final_scalar)
    real(real32), intent(in) :: values(:,:)
    type(scalar_2D_t), intent(out) :: final_scalar
    final_scalar = fa_reconstruct_scalar(values, args%order, args%cells, x_min, x_max)
  end subroutine

  function get_numerical_arguments() result(numerical_arguments)
    type(numerical_arguments_t) numerical_arguments
    character(len=:), allocatable :: cells_string, steps_string, dt_string, order_string
    character(len=:), allocatable :: solver_string, input_string, dump_final_raw_string

    numerical_arguments%solver = 0
    numerical_arguments%benchmark = command_line%argument_present(["--benchmark"])
    numerical_arguments%no_csv = command_line%argument_present(["--no-csv"])
    if (numerical_arguments%benchmark) then
      numerical_arguments%cells = [257,257]
      numerical_arguments%steps = 30000
    end if

    cells_string = command_line%flag_value("--cells")
    steps_string = command_line%flag_value("--steps")
    dt_string = command_line%flag_value("--dt")
    order_string = command_line%flag_value("--order")
    solver_string = command_line%flag_value("--solver")
    input_string = command_line%flag_value("--input")
    dump_final_raw_string = command_line%flag_value("--dump-final-raw")

    if (len(cells_string) /= 0) then
      call parse_int_pair(cells_string, numerical_arguments%cells)
      numerical_arguments%cells_supplied = .true.
    end if
    if (len(steps_string) /= 0) then
      call parse_positive_integer(steps_string, numerical_arguments%steps, "--steps")
      numerical_arguments%steps_supplied = .true.
    end if
    if (len(dt_string) /= 0) then
      call parse_positive_real(dt_string, numerical_arguments%dt, "--dt")
      numerical_arguments%dt_supplied = .true.
    end if
    if (len(order_string) /= 0) call parse_order(order_string, numerical_arguments%order)
    if (len(solver_string) /= 0) numerical_arguments%solver = parse_solver_mode(solver_string)
    if (len(input_string) /= 0) numerical_arguments%input_path = input_string
    if (len(dump_final_raw_string) /= 0) numerical_arguments%dump_final_raw_path = dump_final_raw_string

    if (allocated(numerical_arguments%input_path)) then
      if (numerical_arguments%benchmark .or. numerical_arguments%cells_supplied .or. &
          numerical_arguments%steps_supplied .or. numerical_arguments%dt_supplied) then
        error stop "--input is exclusive with --benchmark, --cells, --steps, and --dt"
      end if
      if (numerical_arguments%order /= default_order) error stop "--input requires --order 4"
      if (numerical_arguments%solver == 0) numerical_arguments%solver = solver_workspace
      if (numerical_arguments%solver /= solver_workspace) error stop "--input requires --solver workspace"
    end if

    if (numerical_arguments%solver == 0) then
      if (numerical_arguments%order == 4) then
        numerical_arguments%solver = solver_workspace
      else
        numerical_arguments%solver = solver_operators
      end if
    end if

    if (numerical_arguments%order == 2 .and. numerical_arguments%solver == solver_workspace) then
      error stop "--solver workspace is unsupported for --order 2"
    end if

    call validate_numerical_arguments(numerical_arguments)

    if (.not. allocated(numerical_arguments%input_path) .and. .not. numerical_arguments%dt_supplied) then
      numerical_arguments%dt = default_dt * min(real(51, kind=real64) / real(numerical_arguments%cells(1), kind=real64), &
                                               real(51, kind=real64) / real(numerical_arguments%cells(2), kind=real64))**2
    end if
  end function

  subroutine validate_numerical_arguments(numerical_arguments)
    type(numerical_arguments_t), intent(in) :: numerical_arguments
    if (.not. any([numerical_arguments%order == 2, numerical_arguments%order == 4])) then
      error stop "unsupported order; expected 2 or 4"
    end if
    if (any(numerical_arguments%cells <= 0)) error stop "cells must be positive"
    if (any(numerical_arguments%cells < 2*numerical_arguments%order + 1)) error stop "cells must satisfy cells >= 2*order + 1"
    if (numerical_arguments%steps <= 0) error stop "steps must be positive"
    if (.not. allocated(numerical_arguments%input_path)) then
      if (.not. numerical_arguments%dt_supplied .and. numerical_arguments%dt <= 0E0) error stop "dt must be positive"
      if (numerical_arguments%dt <= 0E0) error stop "dt must be positive"
    end if
  end subroutine

  integer function parse_solver_mode(raw) result(mode)
    character(len=*), intent(in) :: raw
    character(len=:), allocatable :: trimmed

    trimmed = trim(adjustl(raw))
    select case (trimmed)
    case ("workspace")
      mode = solver_workspace
    case ("operators")
      mode = solver_operators
    case default
      error stop "--solver must be workspace or operators"
    end select
  end function

  subroutine parse_order(raw, value)
    character(len=*), intent(in) :: raw
    integer, intent(out) :: value
    call read_integer_flag(raw, value, "--order")
    if (.not. any([value == 2, value == 4])) error stop "order must be 2 or 4"
  end subroutine

  subroutine parse_positive_integer(raw, value, flag_name)
    character(len=*), intent(in) :: raw
    integer, intent(out) :: value
    character(len=*), intent(in) :: flag_name
    call read_integer_flag(raw, value, flag_name)
    if (value <= 0) error stop flag_name // " must be positive"
  end subroutine

  subroutine parse_positive_real(raw, value, flag_name)
    character(len=*), intent(in) :: raw, flag_name
    real, intent(out) :: value
    call read_real_flag(raw, value, flag_name)
    if (value <= 0E0) error stop flag_name // " must be positive"
  end subroutine

  subroutine read_integer_flag(raw, value, flag_name)
    character(len=*), intent(in) :: raw, flag_name
    integer, intent(out) :: value
    integer :: ios
    character(len=256) :: iomsg
    character(len=:), allocatable :: buffer

    buffer = trim(adjustl(raw))

    read(buffer, *, iostat=ios, iomsg=iomsg) value
    if (ios /= 0) error stop flag_name // " parse error: " // trim(iomsg)
  end subroutine

  subroutine read_real_flag(raw, value, flag_name)
    character(len=*), intent(in) :: raw, flag_name
    real, intent(out) :: value
    integer :: ios
    character(len=256) :: iomsg
    character(len=:), allocatable :: buffer

    buffer = trim(adjustl(raw))

    read(buffer, *, iostat=ios, iomsg=iomsg) value
    if (ios /= 0) error stop flag_name // " parse error: " // trim(iomsg)
    if (.not. ieee_is_finite(value)) error stop flag_name // " must be finite"
  end subroutine

  subroutine parse_int_pair(raw, values)
    character(len=*), intent(in) :: raw
    integer, intent(out) :: values(2)
    character(len=:), allocatable :: trimmed
    integer :: comma, ios
    character(len=256) :: iomsg

    trimmed = trim(adjustl(raw))
    comma = index(trimmed, ',')
    if (comma == 0) then
      call read_integer_flag(trimmed, values(1), "--cells")
      values(2) = values(1)
    else
      read(trimmed, *, iostat=ios, iomsg=iomsg) values
      if (ios /= 0) error stop "--cells parse error: " // trim(iomsg)
    end if
  end subroutine

  subroutine write_native_input(path, scalar_state, velocity_state, args)
    character(len=*), intent(in) :: path
    type(scalar_2D_t), intent(in) :: scalar_state
    type(vector_2D_t), intent(in) :: velocity_state
    type(numerical_arguments_t), intent(in) :: args
    integer :: unit
    character(len=256) :: header

    open(newunit=unit, file=path, access="stream", form="unformatted", status="replace", action="write")
    write(unit) "FADV2D1" // new_line("a")
    write(header,'(i0,1x,i0,1x,i0,1x,es24.16)') args%cells(1), args%cells(2), args%steps, real(args%dt, kind=real64)
    write(unit) trim(header) // new_line("a")
    write(unit) "mode raw" // new_line("a")
    write(unit) real(scalar_state%values(), kind=real32)
    write(unit) real(velocity_state%values(x_dir), kind=real32)
    write(unit) real(velocity_state%values(y_dir), kind=real32)
    close(unit)
  end subroutine

end program
