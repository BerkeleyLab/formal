! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module gpu_benchmark_m
  use iso_c_binding, only : c_double, c_int, c_int64_t
  use iso_fortran_env, only : int64, real64
  implicit none
  private

  type, public :: gpu_benchmark_stats_t
    logical :: gpu_timing_available = .false.
    integer(int64) :: completed_command_buffers = 0
    integer(int64) :: commits = 0
    integer(int64) :: dispatches = 0
    integer(int64) :: host_syncs = 0
    integer(int64) :: host_waits = 0
    integer(int64) :: memcpy_h2d_bytes = 0
    integer(int64) :: memcpy_d2h_bytes = 0
    integer(int64) :: buffer_allocations = 0
    integer(int64) :: buffer_alloc_bytes = 0
    real(real64) :: gpu_elapsed_seconds = 0E0
    real(real64) :: gpu_busy_seconds = 0E0
    real(real64) :: jit_seconds = 0E0
  end type

  public :: gpu_benchmark_begin, gpu_benchmark_end

  interface
    subroutine formal_gpu_benchmark_begin(gpu_timing_available) bind(C, name="formal_gpu_benchmark_begin")
      import c_int
      integer(c_int), intent(out) :: gpu_timing_available
    end subroutine

    subroutine formal_gpu_benchmark_end( &
       gpu_timing_available &
      ,completed_command_buffers &
      ,commits &
      ,dispatches &
      ,host_syncs &
      ,host_waits &
      ,memcpy_h2d_bytes &
      ,memcpy_d2h_bytes &
      ,buffer_allocations &
      ,buffer_alloc_bytes &
      ,gpu_elapsed_seconds &
      ,gpu_busy_seconds &
      ,jit_seconds &
    ) bind(C, name="formal_gpu_benchmark_end")
      import c_double, c_int, c_int64_t
      integer(c_int), intent(out) :: gpu_timing_available
      integer(c_int64_t), intent(out) :: completed_command_buffers
      integer(c_int64_t), intent(out) :: commits
      integer(c_int64_t), intent(out) :: dispatches
      integer(c_int64_t), intent(out) :: host_syncs
      integer(c_int64_t), intent(out) :: host_waits
      integer(c_int64_t), intent(out) :: memcpy_h2d_bytes
      integer(c_int64_t), intent(out) :: memcpy_d2h_bytes
      integer(c_int64_t), intent(out) :: buffer_allocations
      integer(c_int64_t), intent(out) :: buffer_alloc_bytes
      real(c_double), intent(out) :: gpu_elapsed_seconds
      real(c_double), intent(out) :: gpu_busy_seconds
      real(c_double), intent(out) :: jit_seconds
    end subroutine
  end interface

contains

  subroutine gpu_benchmark_begin(stats)
    type(gpu_benchmark_stats_t), intent(out) :: stats
    integer(c_int) :: gpu_timing_available

    call formal_gpu_benchmark_begin(gpu_timing_available)
    stats = gpu_benchmark_stats_t(gpu_timing_available = gpu_timing_available /= 0)
  end subroutine

  subroutine gpu_benchmark_end(stats)
    type(gpu_benchmark_stats_t), intent(out) :: stats
    integer(c_int) :: gpu_timing_available
    integer(c_int64_t) :: completed_command_buffers
    integer(c_int64_t) :: commits
    integer(c_int64_t) :: dispatches
    integer(c_int64_t) :: host_syncs
    integer(c_int64_t) :: host_waits
    integer(c_int64_t) :: memcpy_h2d_bytes
    integer(c_int64_t) :: memcpy_d2h_bytes
    integer(c_int64_t) :: buffer_allocations
    integer(c_int64_t) :: buffer_alloc_bytes
    real(c_double) :: gpu_elapsed_seconds
    real(c_double) :: gpu_busy_seconds
    real(c_double) :: jit_seconds

    call formal_gpu_benchmark_end( &
       gpu_timing_available &
      ,completed_command_buffers &
      ,commits &
      ,dispatches &
      ,host_syncs &
      ,host_waits &
      ,memcpy_h2d_bytes &
      ,memcpy_d2h_bytes &
      ,buffer_allocations &
      ,buffer_alloc_bytes &
      ,gpu_elapsed_seconds &
      ,gpu_busy_seconds &
      ,jit_seconds &
    )

    stats%gpu_timing_available = gpu_timing_available /= 0
    stats%completed_command_buffers = completed_command_buffers
    stats%commits = commits
    stats%dispatches = dispatches
    stats%host_syncs = host_syncs
    stats%host_waits = host_waits
    stats%memcpy_h2d_bytes = memcpy_h2d_bytes
    stats%memcpy_d2h_bytes = memcpy_d2h_bytes
    stats%buffer_allocations = buffer_allocations
    stats%buffer_alloc_bytes = buffer_alloc_bytes
    stats%gpu_elapsed_seconds = gpu_elapsed_seconds
    stats%gpu_busy_seconds = gpu_busy_seconds
    stats%jit_seconds = jit_seconds
  end subroutine

end module gpu_benchmark_m
