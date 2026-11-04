#include <metal_stdlib>
using namespace metal;

struct fa_params_t {
    uint nx;
    uint ny;
    uint stage;
    uint unused0;
    float inv_dx;
    float inv_dy;
    float alpha;
    float dt_over_6;
    float weight;
    float unused1;
    float unused2;
    float unused3;
};

inline float fa_grad_face_line(const device float *line, uint stride, uint n, uint i,
    float inv_h)
{
    if (i == 0) {
        return (((-352.0f / 105.0f) * inv_h) * line[0 * stride] +
                ((35.0f / 8.0f) * inv_h) * line[1 * stride] +
                ((-35.0f / 24.0f) * inv_h) * line[2 * stride] +
                ((21.0f / 40.0f) * inv_h) * line[3 * stride] +
                ((-5.0f / 56.0f) * inv_h) * line[4 * stride]);
    }
    if (i == 1) {
        return (((16.0f / 105.0f) * inv_h) * line[0 * stride] +
                ((-31.0f / 24.0f) * inv_h) * line[1 * stride] +
                ((29.0f / 24.0f) * inv_h) * line[2 * stride] +
                ((-3.0f / 40.0f) * inv_h) * line[3 * stride] +
                ((1.0f / 168.0f) * inv_h) * line[4 * stride]);
    }
    if (i + 3 < n) {
        return (((1.0f / 24.0f) * inv_h) * line[(i - 1) * stride] +
                ((-9.0f / 8.0f) * inv_h) * line[i * stride] +
                ((9.0f / 8.0f) * inv_h) * line[(i + 1) * stride] +
                ((-1.0f / 24.0f) * inv_h) * line[(i + 2) * stride]);
    }
    if (i == n - 3) {
        return (((-1.0f / 168.0f) * inv_h) * line[(n - 5) * stride] +
                ((3.0f / 40.0f) * inv_h) * line[(n - 4) * stride] +
                ((-29.0f / 24.0f) * inv_h) * line[(n - 3) * stride] +
                ((31.0f / 24.0f) * inv_h) * line[(n - 2) * stride] +
                ((-16.0f / 105.0f) * inv_h) * line[(n - 1) * stride]);
    }
    return (((5.0f / 56.0f) * inv_h) * line[(n - 5) * stride] +
            ((-21.0f / 40.0f) * inv_h) * line[(n - 4) * stride] +
            ((35.0f / 24.0f) * inv_h) * line[(n - 3) * stride] +
            ((-35.0f / 8.0f) * inv_h) * line[(n - 2) * stride] +
            ((352.0f / 105.0f) * inv_h) * line[(n - 1) * stride]);
}

inline float fa_interp_c2f_line(const device float *line, uint stride, uint n, uint i)
{
    if (i == 0) {
        return line[0 * stride];
    }
    if (i == 1) {
        return (-16.0f / 112.0f) * line[0 * stride] +
               (70.0f / 112.0f) * line[1 * stride] +
               (70.0f / 112.0f) * line[2 * stride] +
               (-14.0f / 112.0f) * line[3 * stride] +
               (2.0f / 112.0f) * line[4 * stride];
    }
    if (i + 3 < n) {
        return (-7.0f / 112.0f) * line[(i - 1) * stride] +
               (63.0f / 112.0f) * line[i * stride] +
               (63.0f / 112.0f) * line[(i + 1) * stride] +
               (-7.0f / 112.0f) * line[(i + 2) * stride];
    }
    if (i == n - 3) {
        return (2.0f / 112.0f) * line[(n - 5) * stride] +
               (-14.0f / 112.0f) * line[(n - 4) * stride] +
               (70.0f / 112.0f) * line[(n - 3) * stride] +
               (70.0f / 112.0f) * line[(n - 2) * stride] +
               (-16.0f / 112.0f) * line[(n - 1) * stride];
    }
    return line[(n - 1) * stride];
}

inline float fa_div_face_line(const device float *line, uint stride, uint n, uint i,
    float inv_h)
{
    if (i == 0) {
        return (((-11.0f / 12.0f) * inv_h) * line[0 * stride] +
                ((17.0f / 24.0f) * inv_h) * line[1 * stride] +
                ((3.0f / 8.0f) * inv_h) * line[2 * stride] +
                ((-5.0f / 24.0f) * inv_h) * line[3 * stride] +
                ((1.0f / 24.0f) * inv_h) * line[4 * stride]);
    }
    if (i + 2 < n) {
        return (((1.0f / 24.0f) * inv_h) * line[(i - 1) * stride] +
                ((-9.0f / 8.0f) * inv_h) * line[i * stride] +
                ((9.0f / 8.0f) * inv_h) * line[(i + 1) * stride] +
                ((-1.0f / 24.0f) * inv_h) * line[(i + 2) * stride]);
    }
    return (((-1.0f / 24.0f) * inv_h) * line[(n - 5) * stride] +
            ((5.0f / 24.0f) * inv_h) * line[(n - 4) * stride] +
            ((-3.0f / 8.0f) * inv_h) * line[(n - 3) * stride] +
            ((-17.0f / 24.0f) * inv_h) * line[(n - 2) * stride] +
            ((11.0f / 12.0f) * inv_h) * line[(n - 1) * stride]);
}

inline uint fa_scalar_index(uint i, uint j, uint nx)
{
    return i + (nx + 2u) * j;
}

inline uint fa_xface_index(uint i, uint j, uint nx)
{
    return i + (nx + 1u) * j;
}

inline uint fa_yface_index(uint i, uint j, uint nx)
{
    return i + (nx + 2u) * j;
}

inline void fa_compute_fluxes_body(device const float *stage,
    device const float *vx, device const float *vy, device float *diff_x,
    device float *adv_x, device float *diff_y, device float *adv_y,
    constant fa_params_t &p, uint i, uint j)
{
    const uint nx = p.nx;
    const uint ny = p.ny;
    const uint sx = nx + 2u;
    const uint sy = ny + 2u;

    if (i <= nx && j <= ny + 1u) {
        const device float *line = stage + j * sx;
        const uint idx = fa_xface_index(i, j, nx);
        diff_x[idx] = 0.5f * fa_grad_face_line(line, 1u, sx, i, p.inv_dx);
        adv_x[idx] = vx[idx] * fa_interp_c2f_line(line, 1u, sx, i);
    }

    if (i <= nx + 1u && j <= ny) {
        const device float *line = stage + i;
        const uint idx = fa_yface_index(i, j, nx);
        diff_y[idx] = 0.5f * fa_grad_face_line(line, sx, sy, j, p.inv_dy);
        adv_y[idx] = vy[idx] * fa_interp_c2f_line(line, sx, sy, j);
    }
}

inline void fa_update_stage_body(device float *base, device const float *stage_in,
    device float *stage_out, device float *accum, device const float *diff_x,
    device const float *adv_x, device const float *diff_y,
    device const float *adv_y, constant fa_params_t &p, uint i, uint j)
{
    const uint nx = p.nx;
    const uint ny = p.ny;
    const uint stage = p.stage;
    const uint scalar_idx = fa_scalar_index(i, j, nx);

    if (i == 0u || j == 0u || i == nx + 1u || j == ny + 1u) {
        stage_out[scalar_idx] = base[scalar_idx];
        return;
    }

    const uint x_idx = i - 1u;
    const uint y_idx = j - 1u;
    const device float *x_line_diff = diff_x + j * (nx + 1u);
    const device float *x_line_adv = adv_x + j * (nx + 1u);
    const device float *y_line_diff = diff_y + i;
    const device float *y_line_adv = adv_y + i;

    const float diffx = fa_div_face_line(x_line_diff, 1u, nx + 1u, x_idx, p.inv_dx);
    const float diffy = fa_div_face_line(y_line_diff, nx + 2u, ny + 1u, y_idx, p.inv_dy);
    const float advx = fa_div_face_line(x_line_adv, 1u, nx + 1u, x_idx, p.inv_dx);
    const float advy = fa_div_face_line(y_line_adv, nx + 2u, ny + 1u, y_idx, p.inv_dy);
    const float rhs = (diffx + diffy) - (advx + advy);

    if (stage == 1u) {
        accum[scalar_idx] = rhs;
        stage_out[scalar_idx] = base[scalar_idx] + p.alpha * rhs;
    } else if (stage == 2u) {
        accum[scalar_idx] = accum[scalar_idx] + p.weight * rhs;
        stage_out[scalar_idx] = base[scalar_idx] + p.alpha * rhs;
    } else if (stage == 3u) {
        accum[scalar_idx] = accum[scalar_idx] + p.weight * rhs;
        stage_out[scalar_idx] = base[scalar_idx] + p.alpha * rhs;
    } else {
        accum[scalar_idx] = accum[scalar_idx] + rhs;
        base[scalar_idx] = base[scalar_idx] + p.dt_over_6 * accum[scalar_idx];
        stage_out[scalar_idx] = base[scalar_idx];
    }
}

kernel void compute_fluxes_2d(device const float *stage [[buffer(0)]],
    device const float *vx [[buffer(1)]],
    device const float *vy [[buffer(2)]],
    device float *diff_x [[buffer(3)]],
    device float *adv_x [[buffer(4)]],
    device float *diff_y [[buffer(5)]],
    device float *adv_y [[buffer(6)]],
    constant fa_params_t &p [[buffer(7)]],
    uint2 gid [[thread_position_in_grid]])
{
    fa_compute_fluxes_body(stage, vx, vy, diff_x, adv_x, diff_y, adv_y, p,
        gid.x, gid.y);
}

kernel void compute_fluxes_linear(device const float *stage [[buffer(0)]],
    device const float *vx [[buffer(1)]],
    device const float *vy [[buffer(2)]],
    device float *diff_x [[buffer(3)]],
    device float *adv_x [[buffer(4)]],
    device float *diff_y [[buffer(5)]],
    device float *adv_y [[buffer(6)]],
    constant fa_params_t &p [[buffer(7)]],
    uint gid [[thread_position_in_grid]])
{
    const int flat = int(gid);
    const int sx = int(p.nx + 2u);
    const uint i = uint(flat - (flat / sx) * sx);
    const uint j = uint(flat / sx);
    fa_compute_fluxes_body(stage, vx, vy, diff_x, adv_x, diff_y, adv_y, p, i, j);
}

kernel void update_stage_2d(device float *base [[buffer(0)]],
    device const float *stage_in [[buffer(1)]],
    device float *stage_out [[buffer(2)]],
    device float *accum [[buffer(3)]],
    device const float *diff_x [[buffer(4)]],
    device const float *adv_x [[buffer(5)]],
    device const float *diff_y [[buffer(6)]],
    device const float *adv_y [[buffer(7)]],
    constant fa_params_t &p [[buffer(8)]],
    uint2 gid [[thread_position_in_grid]])
{
    fa_update_stage_body(base, stage_in, stage_out, accum, diff_x, adv_x, diff_y,
        adv_y, p, gid.x, gid.y);
}

kernel void update_stage_linear(device float *base [[buffer(0)]],
    device const float *stage_in [[buffer(1)]],
    device float *stage_out [[buffer(2)]],
    device float *accum [[buffer(3)]],
    device const float *diff_x [[buffer(4)]],
    device const float *adv_x [[buffer(5)]],
    device const float *diff_y [[buffer(6)]],
    device const float *adv_y [[buffer(7)]],
    constant fa_params_t &p [[buffer(8)]],
    uint gid [[thread_position_in_grid]])
{
    const int flat = int(gid);
    const int sx = int(p.nx + 2u);
    const uint i = uint(flat - (flat / sx) * sx);
    const uint j = uint(flat / sx);
    fa_update_stage_body(base, stage_in, stage_out, accum, diff_x, adv_x, diff_y,
        adv_y, p, i, j);
}
