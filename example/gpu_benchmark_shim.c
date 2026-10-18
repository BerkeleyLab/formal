#include <stdint.h>
#include <string.h>

#ifdef FORMAL_ENABLE_GPU_BENCHMARK
#include "lfortran_gpu_runtime.h"

static lfortran_gpu_ctx *gpu_ctx = NULL;
static lfortran_gpu_metal_telemetry begin_snapshot;
static int has_begin_snapshot = 0;

static void ensure_context(void)
{
    if (gpu_ctx == NULL) {
        gpu_ctx = lfortran_gpu_init();
    }
}

void formal_gpu_benchmark_begin(int *gpu_timing_available)
{
    ensure_context();
    memset(&begin_snapshot, 0, sizeof(begin_snapshot));
    if (gpu_ctx != NULL) {
        lfortran_gpu_metal_telemetry_snapshot_sync(gpu_ctx, &begin_snapshot);
    }
    has_begin_snapshot = 1;
    if (gpu_timing_available != NULL) {
        *gpu_timing_available = begin_snapshot.gpu_timing_available;
    }
}

void formal_gpu_benchmark_end(
    int *gpu_timing_available,
    long long *completed_command_buffers,
    long long *commits,
    long long *dispatches,
    long long *host_syncs,
    long long *host_waits,
    long long *memcpy_h2d_bytes,
    long long *memcpy_d2h_bytes,
    long long *buffer_allocations,
    long long *buffer_alloc_bytes,
    double *gpu_elapsed_seconds,
    double *gpu_busy_seconds,
    double *jit_seconds)
{
    lfortran_gpu_metal_telemetry end_snapshot;

    ensure_context();
    memset(&end_snapshot, 0, sizeof(end_snapshot));
    if (gpu_ctx != NULL) {
        lfortran_gpu_metal_telemetry_snapshot_sync(gpu_ctx, &end_snapshot);
    }

    if (gpu_timing_available != NULL) {
        *gpu_timing_available = end_snapshot.gpu_timing_available;
    }
    if (completed_command_buffers != NULL) {
        *completed_command_buffers = (long long)(end_snapshot.completed_command_buffers - begin_snapshot.completed_command_buffers);
    }
    if (commits != NULL) {
        *commits = (long long)(end_snapshot.commits - begin_snapshot.commits);
    }
    if (dispatches != NULL) {
        *dispatches = (long long)(end_snapshot.dispatches - begin_snapshot.dispatches);
    }
    if (host_syncs != NULL) {
        *host_syncs = (long long)(end_snapshot.host_syncs - begin_snapshot.host_syncs);
    }
    if (host_waits != NULL) {
        *host_waits = (long long)(end_snapshot.host_waits - begin_snapshot.host_waits);
    }
    if (memcpy_h2d_bytes != NULL) {
        *memcpy_h2d_bytes = (long long)(end_snapshot.memcpy_h2d_bytes - begin_snapshot.memcpy_h2d_bytes);
    }
    if (memcpy_d2h_bytes != NULL) {
        *memcpy_d2h_bytes = (long long)(end_snapshot.memcpy_d2h_bytes - begin_snapshot.memcpy_d2h_bytes);
    }
    if (buffer_allocations != NULL) {
        *buffer_allocations = (long long)(end_snapshot.buffer_allocations - begin_snapshot.buffer_allocations);
    }
    if (buffer_alloc_bytes != NULL) {
        *buffer_alloc_bytes = (long long)(end_snapshot.buffer_alloc_bytes - begin_snapshot.buffer_alloc_bytes);
    }
    if (gpu_elapsed_seconds != NULL) {
        *gpu_elapsed_seconds = end_snapshot.gpu_elapsed_seconds - begin_snapshot.gpu_elapsed_seconds;
    }
    if (gpu_busy_seconds != NULL) {
        *gpu_busy_seconds = end_snapshot.gpu_busy_seconds - begin_snapshot.gpu_busy_seconds;
    }
    if (jit_seconds != NULL) {
        *jit_seconds = end_snapshot.jit_seconds - begin_snapshot.jit_seconds;
    }
    has_begin_snapshot = 0;
}
#else
void formal_gpu_benchmark_begin(int *gpu_timing_available)
{
    if (gpu_timing_available != NULL) {
        *gpu_timing_available = 0;
    }
}

void formal_gpu_benchmark_end(
    int *gpu_timing_available,
    long long *completed_command_buffers,
    long long *commits,
    long long *dispatches,
    long long *host_syncs,
    long long *host_waits,
    long long *memcpy_h2d_bytes,
    long long *memcpy_d2h_bytes,
    long long *buffer_allocations,
    long long *buffer_alloc_bytes,
    double *gpu_elapsed_seconds,
    double *gpu_busy_seconds,
    double *jit_seconds)
{
    if (gpu_timing_available != NULL) {
        *gpu_timing_available = 0;
    }
    if (completed_command_buffers != NULL) {
        *completed_command_buffers = 0;
    }
    if (commits != NULL) {
        *commits = 0;
    }
    if (dispatches != NULL) {
        *dispatches = 0;
    }
    if (host_syncs != NULL) {
        *host_syncs = 0;
    }
    if (host_waits != NULL) {
        *host_waits = 0;
    }
    if (memcpy_h2d_bytes != NULL) {
        *memcpy_h2d_bytes = 0;
    }
    if (memcpy_d2h_bytes != NULL) {
        *memcpy_d2h_bytes = 0;
    }
    if (buffer_allocations != NULL) {
        *buffer_allocations = 0;
    }
    if (buffer_alloc_bytes != NULL) {
        *buffer_alloc_bytes = 0;
    }
    if (gpu_elapsed_seconds != NULL) {
        *gpu_elapsed_seconds = 0.0;
    }
    if (gpu_busy_seconds != NULL) {
        *gpu_busy_seconds = 0.0;
    }
    if (jit_seconds != NULL) {
        *jit_seconds = 0.0;
    }
}
#endif
