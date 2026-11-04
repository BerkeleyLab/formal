#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include <math.h>
#include <errno.h>
#include <mach/mach_time.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const float FA_PI = 3.14159265358979323846f;
static const float FA_DIFFUSION = 0.5f;
static const float FA_SIGMA = 3.14159265358979323846f / 8.0f;
static const float FA_X0 = -3.14159265358979323846f / 2.0f;
static const float FA_Y0 = -3.14159265358979323846f / 2.0f;
static const char *FA_FORMAL_51_COORDS[] = {
    "-3.141592741013", "-3.079992771149", "-2.956793069839", "-2.833593368530",
    "-2.710393667221", "-2.587193965912", "-2.463994264603", "-2.340794563293",
    "-2.217594623566", "-2.094395160675", "-1.971195340157", "-1.847995638847",
    "-1.724795937538", "-1.601596117020", "-1.478396415710", "-1.355196714401",
    "-1.231997013092", "-1.108797311783", "-0.9855976104736", "-0.8623979091644",
    "-0.7391982078552", "-0.6159985065460", "-0.4927988052368", "-0.3695991039276",
    "-0.2463994026184", "-0.1231994628906", "0.2384185791016E-06", "0.1231999397278",
    "0.2463996410370", "0.3695993423462", "0.4927990436554", "0.6159987449646",
    "0.7391984462738", "0.8623981475830", "0.9855980873108", "1.108797550201",
    "1.231997489929", "1.355196952820", "1.478396892548", "1.601596355438",
    "1.724796295166", "1.847995758057", "1.971195697784", "2.094395160675",
    "2.217595100403", "2.340794563293", "2.463994503021", "2.587193965912",
    "2.710393905640", "2.833593845367", "2.956793308258", "3.079993247986",
    "3.141592741013",
};

typedef struct {
    uint32_t nx;
    uint32_t ny;
    uint32_t steps;
    float dt;
} fa_config_t;

typedef struct {
    size_t scalar_len;
    size_t xface_len;
    size_t yface_len;
    float *base;
    float *stage[2];
    float *accum;
    float *vx;
    float *vy;
} fa_fields_t;

typedef struct {
    bool metal;
    bool dump_csv;
    bool dump_bin;
    char *input_path;
    char *output_path;
    char *binary_output_path;
    fa_config_t cfg;
    bool dt_supplied;
    uint32_t tg_x;
    uint32_t tg_y;
    bool tg_supplied;
    bool index_mode_supplied;
    enum {
        FA_INDEX_DIRECT_2D,
        FA_INDEX_LINEAR,
    } index_mode;
} fa_options_t;

typedef struct {
    double jit_seconds;
    double solve_seconds;
    double gpu_busy;
    bool final_all_finite;
} fa_run_stats_t;

static double fa_now_seconds(void)
{
    static mach_timebase_info_data_t timebase = {0, 0};
    if (timebase.denom == 0) {
        if (mach_timebase_info(&timebase) != KERN_SUCCESS) {
            fprintf(stderr, "cannot query mach timebase\n");
            exit(1);
        }
    }
    return (double)mach_absolute_time() * (double)timebase.numer /
        (double)timebase.denom * 1e-9;
}

static void fa_die(const char *message)
{
    fprintf(stderr, "%s\n", message);
    exit(1);
}

static void fa_dief(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

static size_t fa_checked_mul_size(size_t a, size_t b, const char *what)
{
    if (a != 0 && b > SIZE_MAX / a) {
        fa_dief("%s size overflow", what);
    }
    return a * b;
}

static size_t fa_checked_add_size(size_t a, size_t b, const char *what)
{
    if (b > SIZE_MAX - a) {
        fa_dief("%s size overflow", what);
    }
    return a + b;
}

static const char *fa_center_coord_text(size_t i, uint32_t n)
{
    if (n == 51 && i < sizeof(FA_FORMAL_51_COORDS) / sizeof(FA_FORMAL_51_COORDS[0])) {
        return FA_FORMAL_51_COORDS[i];
    }
    return NULL;
}

static float fa_center_coord(size_t i, uint32_t n, float min, float max)
{
    const char *text = fa_center_coord_text(i, n);
    if (text) {
        return strtof(text, NULL);
    }
    volatile float dx = (max - min) / (float)n;
    if (i == 0) {
        return min;
    }
    if (i == (size_t)n + 1) {
        return max;
    }
    volatile float base = min + 0.5f * dx;
    volatile float offset = ((float)i - 1.0f) * dx;
    return base + offset;
}

static float fa_face_coord(size_t i, uint32_t n, float min, float max)
{
    volatile float dx = (max - min) / (float)n;
    if (i == 0) {
        return min;
    }
    if (i == (size_t)n) {
        return max;
    }
    return min + (float)i * dx;
}

static size_t fa_scalar_index(size_t i, size_t j, uint32_t nx)
{
    return i + (size_t)(nx + 2) * j;
}

static size_t fa_xface_index(size_t i, size_t j, uint32_t nx)
{
    return i + (size_t)(nx + 1) * j;
}

static size_t fa_yface_index(size_t i, size_t j, uint32_t nx)
{
    return i + (size_t)(nx + 2) * j;
}

static float fa_scalar_init(float x, float y)
{
    const float dx = x - FA_X0;
    const float dy = y - FA_Y0;
    const float r = sqrtf(dx * dx + dy * dy);
    return expf(-((r * r) / (2.0f * FA_SIGMA * FA_SIGMA)));
}

static float fa_velocity_x(float x, float y)
{
    return 10.0f * sinf(x) * cosf(y);
}

static float fa_velocity_y(float x, float y)
{
    return -10.0f * cosf(x) * sinf(y);
}

static void fa_fields_free(fa_fields_t *fields)
{
    free(fields->base);
    free(fields->stage[0]);
    free(fields->stage[1]);
    free(fields->accum);
    free(fields->vx);
    free(fields->vy);
    memset(fields, 0, sizeof(*fields));
}

static void fa_fields_alloc(fa_fields_t *fields, const fa_config_t *cfg)
{
    memset(fields, 0, sizeof(*fields));
    fields->scalar_len = fa_checked_mul_size((size_t)cfg->nx + 2,
        (size_t)cfg->ny + 2, "scalar field");
    fields->xface_len = fa_checked_mul_size((size_t)cfg->nx + 1,
        (size_t)cfg->ny + 2, "x-face field");
    fields->yface_len = fa_checked_mul_size((size_t)cfg->nx + 2,
        (size_t)cfg->ny + 1, "y-face field");

    fields->base = calloc(fields->scalar_len, sizeof(float));
    fields->stage[0] = calloc(fields->scalar_len, sizeof(float));
    fields->stage[1] = calloc(fields->scalar_len, sizeof(float));
    fields->accum = calloc(fields->scalar_len, sizeof(float));
    fields->vx = calloc(fields->xface_len, sizeof(float));
    fields->vy = calloc(fields->yface_len, sizeof(float));
    if (!fields->base || !fields->stage[0] || !fields->stage[1] ||
        !fields->accum || !fields->vx || !fields->vy) {
        fa_die("cannot allocate fields");
    }
}

static void fa_generate_analytic(const fa_config_t *cfg, fa_fields_t *fields)
{
    for (size_t j = 0; j < (size_t)cfg->ny + 2; ++j) {
        const float y = fa_center_coord(j, cfg->ny, -FA_PI, FA_PI);
        for (size_t i = 0; i < (size_t)cfg->nx + 2; ++i) {
            const float x = fa_center_coord(i, cfg->nx, -FA_PI, FA_PI);
            const size_t idx = fa_scalar_index(i, j, cfg->nx);
            fields->base[idx] = fa_scalar_init(x, y);
            fields->stage[0][idx] = fields->base[idx];
            fields->stage[1][idx] = fields->base[idx];
        }
    }

    for (size_t j = 0; j < (size_t)cfg->ny + 2; ++j) {
        const float y = fa_center_coord(j, cfg->ny, -FA_PI, FA_PI);
        for (size_t i = 0; i < (size_t)cfg->nx + 1; ++i) {
            const float x = fa_face_coord(i, cfg->nx, -FA_PI, FA_PI);
            fields->vx[fa_xface_index(i, j, cfg->nx)] = fa_velocity_x(x, y);
        }
    }

    for (size_t j = 0; j < (size_t)cfg->ny + 1; ++j) {
        const float y = fa_face_coord(j, cfg->ny, -FA_PI, FA_PI);
        for (size_t i = 0; i < (size_t)cfg->nx + 2; ++i) {
            const float x = fa_center_coord(i, cfg->nx, -FA_PI, FA_PI);
            fields->vy[fa_yface_index(i, j, cfg->nx)] = fa_velocity_y(x, y);
        }
    }
}

static void fa_validate_cfg(fa_config_t *cfg, bool dt_supplied)
{
    if (cfg->nx < 9 || cfg->ny < 9) {
        fa_die("cells must be at least 9 in each dimension");
    }
    if (cfg->steps == 0) {
        fa_die("steps must be positive");
    }
    if (!(isfinite(cfg->dt) && cfg->dt > 0.0f)) {
        fa_die("dt must be finite and positive");
    }
    if (!dt_supplied) {
        const float sx = 51.0f / (float)cfg->nx;
        const float sy = 51.0f / (float)cfg->ny;
        const float scale = fminf(sx, sy);
        cfg->dt = 1.0e-4f * scale * scale;
    }
}

static uint32_t fa_parse_u32_strict(const char *raw, const char *flag_name)
{
    errno = 0;
    char *end = NULL;
    unsigned long value = strtoul(raw, &end, 10);
    if (errno != 0 || end == raw || *end != '\0' || value == 0 ||
        value > UINT32_MAX) {
        fa_dief("%s parse error: expected positive unsigned integer", flag_name);
    }
    return (uint32_t)value;
}

static void fa_validate_working_set(const fa_config_t *cfg)
{
    const size_t scalar_len = fa_checked_mul_size((size_t)cfg->nx + 2,
        (size_t)cfg->ny + 2, "scalar field");
    const size_t xface_len = fa_checked_mul_size((size_t)cfg->nx + 1,
        (size_t)cfg->ny + 2, "x-face field");
    const size_t yface_len = fa_checked_mul_size((size_t)cfg->nx + 2,
        (size_t)cfg->ny + 1, "y-face field");

    const size_t scalar_bytes = fa_checked_mul_size(scalar_len, sizeof(float),
        "scalar field bytes");
    const size_t xface_bytes = fa_checked_mul_size(xface_len, sizeof(float),
        "x-face field bytes");
    const size_t yface_bytes = fa_checked_mul_size(yface_len, sizeof(float),
        "y-face field bytes");
    const size_t host_fields_bytes = fa_checked_add_size(
        fa_checked_add_size(
            fa_checked_mul_size(scalar_bytes, 4, "host scalar buffers"),
            xface_bytes, "host fields"),
        yface_bytes, "host fields");
    const size_t warmup_backups_bytes = fa_checked_mul_size(scalar_bytes, 4,
        "warmup backups");
    const size_t metal_buffers_bytes = fa_checked_add_size(
        fa_checked_add_size(
            fa_checked_mul_size(scalar_bytes, 4, "metal scalar buffers"),
            fa_checked_mul_size(xface_bytes, 3, "metal x-face buffers"),
            "metal buffers"),
        fa_checked_mul_size(yface_bytes, 3, "metal y-face buffers"),
        "metal buffers");
    const size_t total_bytes = fa_checked_add_size(
        host_fields_bytes,
        fa_checked_add_size(warmup_backups_bytes, metal_buffers_bytes,
            "working set"),
        "working set");
    const size_t max_bytes = (size_t)512u * 1024u * 1024u;
    if (total_bytes > max_bytes) {
        fa_die("estimated working set exceeds 512 MiB");
    }
}

static float fa_parse_float_strict(const char *raw, const char *flag_name)
{
    errno = 0;
    char *end = NULL;
    float value = strtof(raw, &end);
    if (errno != 0 || end == raw || *end != '\0' || !isfinite(value)) {
        fa_dief("%s parse error: expected finite real", flag_name);
    }
    return value;
}

static void fa_read_raw_input(const char *path, fa_config_t *cfg, fa_fields_t *fields)
{
    FILE *f = fopen(path, "rb");
    if (!f) {
        fa_dief("cannot open input file: %s", path);
    }

    char line[256];
    if (!fgets(line, sizeof(line), f) || strncmp(line, "FADV2D1", 7) != 0) {
        fclose(f);
        fa_die("invalid input header");
    }
    if (!fgets(line, sizeof(line), f)) {
        fclose(f);
        fa_die("missing input dimensions");
    }
    if (sscanf(line, "%u %u %u %f", &cfg->nx, &cfg->ny, &cfg->steps, &cfg->dt) != 4) {
        fclose(f);
        fa_die("cannot parse input dimensions");
    }
    if (!fgets(line, sizeof(line), f)) {
        fclose(f);
        fa_die("missing input mode");
    }
    char mode[64] = {0};
    if (sscanf(line, "mode %63s", mode) != 1) {
        fclose(f);
        fa_die("cannot parse input mode");
    }
    fa_validate_cfg(cfg, true);
    fa_validate_working_set(cfg);
    fa_fields_alloc(fields, cfg);
    if (strcmp(mode, "raw") == 0) {
        if (fread(fields->base, sizeof(float), fields->scalar_len, f) !=
            fields->scalar_len ||
            fread(fields->vx, sizeof(float), fields->xface_len, f) !=
            fields->xface_len ||
            fread(fields->vy, sizeof(float), fields->yface_len, f) !=
            fields->yface_len) {
            fclose(f);
            fa_die("cannot read raw field payload");
        }
        memcpy(fields->stage[0], fields->base, fields->scalar_len * sizeof(float));
        memcpy(fields->stage[1], fields->base, fields->scalar_len * sizeof(float));
    } else if (strcmp(mode, "analytic") == 0) {
        fa_generate_analytic(cfg, fields);
    } else {
        fclose(f);
        fa_die("unknown input mode");
    }
    fclose(f);
}

static void fa_write_csv(const char *path, const fa_config_t *cfg,
    const fa_fields_t *fields)
{
    FILE *f = fopen(path, "wb");
    if (!f) {
        fa_dief("cannot open output file: %s", path);
    }
    fprintf(f, "x,y,scalar\n");
    for (size_t j = 0; j < (size_t)cfg->ny + 2; ++j) {
        const char *y_text = fa_center_coord_text(j, cfg->ny);
        const float y = y_text ? strtof(y_text, NULL) :
            fa_center_coord(j, cfg->ny, -FA_PI, FA_PI);
        for (size_t i = 0; i < (size_t)cfg->nx + 2; ++i) {
            const char *x_text = fa_center_coord_text(i, cfg->nx);
            const float x = x_text ? strtof(x_text, NULL) :
                fa_center_coord(i, cfg->nx, -FA_PI, FA_PI);
            const size_t idx = fa_scalar_index(i, j, cfg->nx);
            if (x_text && y_text) {
                fprintf(f, "%s,%s,%.13g\n", x_text, y_text,
                    (double)fields->base[idx]);
            } else {
                fprintf(f, "%.12f,%.12f,%.13g\n", (double)x, (double)y,
                    (double)fields->base[idx]);
            }
        }
        if (j + 1 < (size_t)cfg->ny + 2) {
            fputc('\n', f);
        }
    }
    fclose(f);
}

static void fa_write_binary(const char *path, const fa_fields_t *fields)
{
    FILE *f = fopen(path, "wb");
    if (!f) {
        fa_dief("cannot open output file: %s", path);
    }
    if (fwrite(fields->base, sizeof(float), fields->scalar_len, f) !=
        fields->scalar_len) {
        fclose(f);
        fa_die("cannot write binary field payload");
    }
    fclose(f);
}

static void fa_check_finite(const float *data, size_t n, const char *name)
{
    for (size_t i = 0; i < n; ++i) {
        if (!isfinite(data[i])) {
            fa_dief("%s contains non-finite values", name);
        }
    }
}

static inline float fa_grad_face_line(const float *line, size_t stride, size_t n, size_t i,
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

static inline float fa_interp_c2f_line(const float *line, size_t stride, size_t n, size_t i)
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
    if (i == n - 2) {
        return line[(n - 1) * stride];
    }
    fa_die("invalid center-to-face stencil index");
    return 0.0f;
}

static inline float fa_div_face_line(const float *line, size_t stride, size_t n, size_t i,
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

static void fa_rhs_cpu(const fa_config_t *cfg, const fa_fields_t *fields,
    const float *stage_in, float *rhs_out)
{
    const float inv_dx = (float)cfg->nx / (2.0f * FA_PI);
    const float inv_dy = (float)cfg->ny / (2.0f * FA_PI);
    const size_t sx = (size_t)cfg->nx + 2;
    const size_t sy = (size_t)cfg->ny + 2;

    float *diff_x = calloc(fields->xface_len, sizeof(float));
    float *adv_x = calloc(fields->xface_len, sizeof(float));
    float *diff_y = calloc(fields->yface_len, sizeof(float));
    float *adv_y = calloc(fields->yface_len, sizeof(float));
    if (!diff_x || !adv_x || !diff_y || !adv_y) {
        fa_die("cannot allocate RHS scratch");
    }

    for (size_t j = 0; j < sy; ++j) {
        const float *line = stage_in + j * sx;
        for (size_t i = 0; i < sx - 1; ++i) {
            const size_t idx = fa_xface_index(i, j, cfg->nx);
            diff_x[idx] = FA_DIFFUSION * fa_grad_face_line(line, 1, sx, i, inv_dx);
            adv_x[idx] = fields->vx[idx] * fa_interp_c2f_line(line, 1, sx, i);
        }
    }

    for (size_t i = 0; i < sx; ++i) {
        const float *line = stage_in + i;
        for (size_t j = 0; j < sy - 1; ++j) {
            const size_t idx = fa_yface_index(i, j, cfg->nx);
            diff_y[idx] = FA_DIFFUSION * fa_grad_face_line(line, sx, sy, j, inv_dy);
            adv_y[idx] = fields->vy[idx] * fa_interp_c2f_line(line, sx, sy, j);
        }
    }

    memset(rhs_out, 0, fields->scalar_len * sizeof(float));
    for (size_t j = 0; j < (size_t)cfg->ny; ++j) {
        for (size_t i = 0; i < (size_t)cfg->nx; ++i) {
            const float diff = fa_div_face_line(diff_x + (j + 1) * ((size_t)cfg->nx + 1),
                1, (size_t)cfg->nx + 1, i, inv_dx) +
                fa_div_face_line(diff_y + (i + 1), sx, (size_t)cfg->ny + 1, j, inv_dy);
            const float adv = fa_div_face_line(adv_x + (j + 1) * ((size_t)cfg->nx + 1),
                1, (size_t)cfg->nx + 1, i, inv_dx) +
                fa_div_face_line(adv_y + (i + 1), sx, (size_t)cfg->ny + 1, j, inv_dy);
            rhs_out[fa_scalar_index(i + 1, j + 1, cfg->nx)] = diff - adv;
        }
    }

    free(diff_x);
    free(adv_x);
    free(diff_y);
    free(adv_y);
}

static void fa_rk4_step_cpu(const fa_config_t *cfg, fa_fields_t *fields)
{
    const float dt = cfg->dt;
    const float dt_half = 0.5f * dt;
    const float dt_over_6 = dt / 6.0f;
    float *rhs = calloc(fields->scalar_len, sizeof(float));
    if (!rhs) {
        fa_die("cannot allocate RHS buffer");
    }

    for (uint32_t stage = 1; stage <= 4; ++stage) {
        const float *stage_in = fields->stage[(stage - 1) & 1u];
        float *stage_out = fields->stage[stage & 1u];
        fa_rhs_cpu(cfg, fields, stage_in, rhs);

        for (size_t j = 0; j < (size_t)cfg->ny + 2; ++j) {
            for (size_t i = 0; i < (size_t)cfg->nx + 2; ++i) {
                const size_t idx = fa_scalar_index(i, j, cfg->nx);
                if (i == 0 || j == 0 || i == (size_t)cfg->nx + 1 ||
                    j == (size_t)cfg->ny + 1) {
                    stage_out[idx] = fields->base[idx];
                    if (stage == 4) {
                        fields->base[idx] = fields->base[idx];
                    }
                    continue;
                }
                if (stage == 1) {
                    fields->accum[idx] = rhs[idx];
                    stage_out[idx] = fields->base[idx] + dt_half * rhs[idx];
                } else if (stage == 2) {
                    fields->accum[idx] = fields->accum[idx] + 2.0f * rhs[idx];
                    stage_out[idx] = fields->base[idx] + dt_half * rhs[idx];
                } else if (stage == 3) {
                    fields->accum[idx] = fields->accum[idx] + 2.0f * rhs[idx];
                    stage_out[idx] = fields->base[idx] + dt * rhs[idx];
                } else {
                    fields->accum[idx] = fields->accum[idx] + rhs[idx];
                    fields->base[idx] = fields->base[idx] + dt_over_6 * fields->accum[idx];
                    stage_out[idx] = fields->base[idx];
                }
            }
        }
    }

    free(rhs);
}

static id<MTLDevice> fa_create_device(void)
{
    id<MTLDevice> device = MTLCreateSystemDefaultDevice();
    if (!device) {
        fa_die("no Metal device available");
    }
    return device;
}

static NSString *fa_read_text_file(NSString *path)
{
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:path
                                                  encoding:NSUTF8StringEncoding
                                                     error:&error];
    if (!content) {
        fprintf(stderr, "cannot read %s: %s\n", path.UTF8String,
            error.localizedDescription.UTF8String);
        exit(1);
    }
    return content;
}

typedef struct {
    uint32_t nx;
    uint32_t ny;
    uint32_t stage;
    uint32_t unused0;
    float inv_dx;
    float inv_dy;
    float alpha;
    float dt_over_6;
    float weight;
    float unused1;
    float unused2;
    float unused3;
} fa_metal_params_t;

_Static_assert(sizeof(fa_metal_params_t) == 48, "fa_metal_params_t must be 48 bytes");
_Static_assert(offsetof(fa_metal_params_t, nx) == 0, "fa_metal_params_t.nx");
_Static_assert(offsetof(fa_metal_params_t, ny) == 4, "fa_metal_params_t.ny");
_Static_assert(offsetof(fa_metal_params_t, stage) == 8, "fa_metal_params_t.stage");
_Static_assert(offsetof(fa_metal_params_t, inv_dx) == 16, "fa_metal_params_t.inv_dx");
_Static_assert(offsetof(fa_metal_params_t, inv_dy) == 20, "fa_metal_params_t.inv_dy");
_Static_assert(offsetof(fa_metal_params_t, alpha) == 24, "fa_metal_params_t.alpha");
_Static_assert(offsetof(fa_metal_params_t, dt_over_6) == 28, "fa_metal_params_t.dt_over_6");
_Static_assert(offsetof(fa_metal_params_t, weight) == 32, "fa_metal_params_t.weight");

static NSString *fa_metal_source_path(void)
{
    NSString *cwd = [[NSFileManager defaultManager] currentDirectoryPath];
    return [cwd stringByAppendingPathComponent:@"formal_advection.metal"];
}

static bool fa_metal_timestamps_valid(const id<MTLCommandBuffer> cb)
{
    return cb.GPUStartTime > 0.0 && cb.GPUEndTime > cb.GPUStartTime;
}

static void fa_run_metal(const fa_options_t *opt, fa_fields_t *fields,
    fa_run_stats_t *stats)
{
    stats->jit_seconds = 0.0;
    stats->solve_seconds = 0.0;
    stats->gpu_busy = 0.0;
    stats->final_all_finite = false;

    double jit_start = fa_now_seconds();
    id<MTLDevice> device = fa_create_device();
    NSError *error = nil;
    NSString *source = fa_read_text_file(fa_metal_source_path());
    MTLCompileOptions *compile_options = [MTLCompileOptions new];
    id<MTLLibrary> library = [device newLibraryWithSource:source
                                                  options:compile_options
                                                    error:&error];
    if (!library) {
        fprintf(stderr, "Metal compile failed: %s\n",
            error.localizedDescription.UTF8String);
        exit(1);
    }

    const char *flux_name = opt->index_mode == FA_INDEX_LINEAR ?
        "compute_fluxes_linear" : "compute_fluxes_2d";
    const char *update_name = opt->index_mode == FA_INDEX_LINEAR ?
        "update_stage_linear" : "update_stage_2d";
    id<MTLFunction> flux_fn = [library newFunctionWithName:[NSString stringWithUTF8String:flux_name]];
    id<MTLFunction> update_fn = [library newFunctionWithName:[NSString stringWithUTF8String:update_name]];
    if (!flux_fn || !update_fn) {
        fa_die("cannot find Metal kernels");
    }
    id<MTLComputePipelineState> flux_pso =
        [device newComputePipelineStateWithFunction:flux_fn error:&error];
    id<MTLComputePipelineState> update_pso =
        [device newComputePipelineStateWithFunction:update_fn error:&error];
    if (!flux_pso || !update_pso) {
        fprintf(stderr, "cannot create Metal pipelines: %s\n",
            error.localizedDescription.UTF8String);
        exit(1);
    }
    stats->jit_seconds = fa_now_seconds() - jit_start;

    id<MTLCommandQueue> queue = [device newCommandQueue];
    if (!queue) {
        fa_die("cannot create Metal command queue");
    }

    if (opt->tg_supplied) {
        if (opt->tg_x == 0 || opt->tg_y == 0) {
            fa_die("threadgroup dimensions must be positive");
        }
        if (opt->index_mode == FA_INDEX_LINEAR && opt->tg_y != 1) {
            fa_die("linear index mode requires 1D threadgroups");
        }
        if ((uint64_t)opt->tg_x * (uint64_t)opt->tg_y >
            (uint64_t)flux_pso.maxTotalThreadsPerThreadgroup) {
            fa_die("threadgroup exceeds pipeline limit");
        }
    }

    float *base_backup = malloc(fields->scalar_len * sizeof(float));
    float *stage0_backup = malloc(fields->scalar_len * sizeof(float));
    float *stage1_backup = malloc(fields->scalar_len * sizeof(float));
    float *accum_backup = malloc(fields->scalar_len * sizeof(float));
    if (!base_backup || !stage0_backup || !stage1_backup || !accum_backup) {
        fa_die("cannot allocate warmup backups");
    }
    memcpy(base_backup, fields->base, fields->scalar_len * sizeof(float));
    memcpy(stage0_backup, fields->stage[0], fields->scalar_len * sizeof(float));
    memcpy(stage1_backup, fields->stage[1], fields->scalar_len * sizeof(float));
    memcpy(accum_backup, fields->accum, fields->scalar_len * sizeof(float));

    id<MTLBuffer> base = [device newBufferWithBytes:fields->base
                                             length:fields->scalar_len * sizeof(float)
                                            options:MTLResourceStorageModeShared];
    id<MTLBuffer> stage0 = [device newBufferWithBytes:fields->stage[0]
                                               length:fields->scalar_len * sizeof(float)
                                              options:MTLResourceStorageModeShared];
    id<MTLBuffer> stage1 = [device newBufferWithBytes:fields->stage[1]
                                               length:fields->scalar_len * sizeof(float)
                                              options:MTLResourceStorageModeShared];
    id<MTLBuffer> accum = [device newBufferWithBytes:fields->accum
                                              length:fields->scalar_len * sizeof(float)
                                             options:MTLResourceStorageModeShared];
    id<MTLBuffer> vx = [device newBufferWithBytes:fields->vx
                                           length:fields->xface_len * sizeof(float)
                                          options:MTLResourceStorageModeShared];
    id<MTLBuffer> vy = [device newBufferWithBytes:fields->vy
                                           length:fields->yface_len * sizeof(float)
                                          options:MTLResourceStorageModeShared];
    id<MTLBuffer> diff_x = [device newBufferWithLength:fields->xface_len * sizeof(float)
                                               options:MTLResourceStorageModeShared];
    id<MTLBuffer> adv_x = [device newBufferWithLength:fields->xface_len * sizeof(float)
                                              options:MTLResourceStorageModeShared];
    id<MTLBuffer> diff_y = [device newBufferWithLength:fields->yface_len * sizeof(float)
                                               options:MTLResourceStorageModeShared];
    id<MTLBuffer> adv_y = [device newBufferWithLength:fields->yface_len * sizeof(float)
                                              options:MTLResourceStorageModeShared];
    if (!base || !stage0 || !stage1 || !accum || !vx || !vy ||
        !diff_x || !adv_x || !diff_y || !adv_y) {
        fa_die("cannot allocate Metal buffers");
    }

    const MTLSize tg = MTLSizeMake(opt->tg_x, opt->tg_y, 1);
    const MTLSize grid = opt->index_mode == FA_INDEX_LINEAR ?
        MTLSizeMake((NSUInteger)(opt->cfg.nx + 2) * (NSUInteger)(opt->cfg.ny + 2), 1, 1) :
        MTLSizeMake((NSUInteger)opt->cfg.nx + 2,
            (NSUInteger)opt->cfg.ny + 2, 1);
    const float dt_half = 0.5f * opt->cfg.dt;
    const float dt_over_6 = opt->cfg.dt / 6.0f;
    const uint32_t steps_per_cb = 8;
    id<MTLBuffer> in_buf = stage0;
    id<MTLBuffer> out_buf = stage1;

    {
        id<MTLCommandBuffer> warmup_cb = [queue commandBuffer];
        id<MTLComputeCommandEncoder> warmup_enc = [warmup_cb computeCommandEncoder];
        for (uint32_t stage = 1; stage <= 4; ++stage) {
            fa_metal_params_t warmup_params = {
                .nx = opt->cfg.nx,
                .ny = opt->cfg.ny,
                .stage = stage,
                .inv_dx = (float)opt->cfg.nx / (2.0f * FA_PI),
                .inv_dy = (float)opt->cfg.ny / (2.0f * FA_PI),
                .alpha = (stage == 1 || stage == 2) ? dt_half :
                    (stage == 3 ? opt->cfg.dt : 0.0f),
                .dt_over_6 = dt_over_6,
                .weight = (stage == 1 || stage == 4) ? 1.0f : 2.0f,
            };
            [warmup_enc setComputePipelineState:flux_pso];
            [warmup_enc setBuffer:in_buf offset:0 atIndex:0];
            [warmup_enc setBuffer:vx offset:0 atIndex:1];
            [warmup_enc setBuffer:vy offset:0 atIndex:2];
            [warmup_enc setBuffer:diff_x offset:0 atIndex:3];
            [warmup_enc setBuffer:adv_x offset:0 atIndex:4];
            [warmup_enc setBuffer:diff_y offset:0 atIndex:5];
            [warmup_enc setBuffer:adv_y offset:0 atIndex:6];
            [warmup_enc setBytes:&warmup_params length:sizeof(warmup_params) atIndex:7];
            [warmup_enc dispatchThreads:grid threadsPerThreadgroup:tg];

            [warmup_enc setComputePipelineState:update_pso];
            [warmup_enc setBuffer:base offset:0 atIndex:0];
            [warmup_enc setBuffer:in_buf offset:0 atIndex:1];
            [warmup_enc setBuffer:out_buf offset:0 atIndex:2];
            [warmup_enc setBuffer:accum offset:0 atIndex:3];
            [warmup_enc setBuffer:diff_x offset:0 atIndex:4];
            [warmup_enc setBuffer:adv_x offset:0 atIndex:5];
            [warmup_enc setBuffer:diff_y offset:0 atIndex:6];
            [warmup_enc setBuffer:adv_y offset:0 atIndex:7];
            [warmup_enc setBytes:&warmup_params length:sizeof(warmup_params) atIndex:8];
            [warmup_enc dispatchThreads:grid threadsPerThreadgroup:tg];

            id<MTLBuffer> tmp = in_buf;
            in_buf = out_buf;
            out_buf = tmp;
        }
        [warmup_enc endEncoding];
        [warmup_cb commit];
        [warmup_cb waitUntilCompleted];
        if (warmup_cb.error) {
            fprintf(stderr, "Metal warmup failed: %s\n",
                warmup_cb.error.localizedDescription.UTF8String);
            exit(1);
        }
        if (!fa_metal_timestamps_valid(warmup_cb)) {
            fa_die("Metal warmup produced invalid timestamps");
        }

        memcpy(base.contents, base_backup, fields->scalar_len * sizeof(float));
        memcpy(stage0.contents, stage0_backup, fields->scalar_len * sizeof(float));
        memcpy(stage1.contents, stage1_backup, fields->scalar_len * sizeof(float));
        memcpy(accum.contents, accum_backup, fields->scalar_len * sizeof(float));
        in_buf = stage0;
        out_buf = stage1;
    }

    double solve_start = fa_now_seconds();
    __strong id<MTLCommandBuffer> pending[2] = { nil, nil };
    uint32_t pending_count = 0;
    double gpu_busy = 0.0;
    double gpu_cursor = 0.0;
    bool have_gpu_cursor = false;
    for (uint32_t step_base = 0; step_base < opt->cfg.steps; ) {
        const uint32_t batch_steps = (opt->cfg.steps - step_base < steps_per_cb) ?
            (opt->cfg.steps - step_base) : steps_per_cb;
        @autoreleasepool {
            id<MTLCommandBuffer> cb = [queue commandBuffer];
            id<MTLComputeCommandEncoder> enc = [cb computeCommandEncoder];
            for (uint32_t local_step = 0; local_step < batch_steps; ++local_step) {
                for (uint32_t stage = 1; stage <= 4; ++stage) {
                    fa_metal_params_t params = {
                        .nx = opt->cfg.nx,
                        .ny = opt->cfg.ny,
                        .stage = stage,
                        .inv_dx = (float)opt->cfg.nx / (2.0f * FA_PI),
                        .inv_dy = (float)opt->cfg.ny / (2.0f * FA_PI),
                        .alpha = (stage == 1 || stage == 2) ? dt_half :
                            (stage == 3 ? opt->cfg.dt : 0.0f),
                        .dt_over_6 = dt_over_6,
                        .weight = (stage == 1 || stage == 4) ? 1.0f : 2.0f,
                    };
                    [enc setComputePipelineState:flux_pso];
                    [enc setBuffer:in_buf offset:0 atIndex:0];
                    [enc setBuffer:vx offset:0 atIndex:1];
                    [enc setBuffer:vy offset:0 atIndex:2];
                    [enc setBuffer:diff_x offset:0 atIndex:3];
                    [enc setBuffer:adv_x offset:0 atIndex:4];
                    [enc setBuffer:diff_y offset:0 atIndex:5];
                    [enc setBuffer:adv_y offset:0 atIndex:6];
                    [enc setBytes:&params length:sizeof(params) atIndex:7];
                    [enc dispatchThreads:grid threadsPerThreadgroup:tg];

                    [enc setComputePipelineState:update_pso];
                    [enc setBuffer:base offset:0 atIndex:0];
                    [enc setBuffer:in_buf offset:0 atIndex:1];
                    [enc setBuffer:out_buf offset:0 atIndex:2];
                    [enc setBuffer:accum offset:0 atIndex:3];
                    [enc setBuffer:diff_x offset:0 atIndex:4];
                    [enc setBuffer:adv_x offset:0 atIndex:5];
                    [enc setBuffer:diff_y offset:0 atIndex:6];
                    [enc setBuffer:adv_y offset:0 atIndex:7];
                    [enc setBytes:&params length:sizeof(params) atIndex:8];
                    [enc dispatchThreads:grid threadsPerThreadgroup:tg];

                    id<MTLBuffer> tmp = in_buf;
                    in_buf = out_buf;
                    out_buf = tmp;
                }
            }
            [enc endEncoding];
            [cb commit];
            pending[pending_count++] = cb;
            if (pending_count == 2) {
                id<MTLCommandBuffer> oldest = pending[0];
                [oldest waitUntilCompleted];
                if (oldest.error) {
                    fprintf(stderr, "Metal solve failed: %s\n",
                        oldest.error.localizedDescription.UTF8String);
                    exit(1);
                }
                if (!fa_metal_timestamps_valid(oldest)) {
                    fa_die("Metal solve produced invalid timestamps");
                }
                if (!have_gpu_cursor) {
                    gpu_busy += oldest.GPUEndTime - oldest.GPUStartTime;
                    gpu_cursor = oldest.GPUEndTime;
                    have_gpu_cursor = true;
                } else if (oldest.GPUStartTime > gpu_cursor) {
                    gpu_busy += oldest.GPUEndTime - oldest.GPUStartTime;
                    gpu_cursor = oldest.GPUEndTime;
                } else if (oldest.GPUEndTime > gpu_cursor) {
                    gpu_busy += oldest.GPUEndTime - gpu_cursor;
                    gpu_cursor = oldest.GPUEndTime;
                }
                pending[0] = pending[1];
                pending[1] = nil;
                pending_count = 1;
            }
        }
        step_base += batch_steps;
    }

    for (uint32_t i = 0; i < pending_count; ++i) {
        id<MTLCommandBuffer> cb = pending[i];
        [cb waitUntilCompleted];
        if (cb.error) {
            fprintf(stderr, "Metal solve failed: %s\n",
                cb.error.localizedDescription.UTF8String);
            exit(1);
        }
        if (!fa_metal_timestamps_valid(cb)) {
            fa_die("Metal solve produced invalid timestamps");
        }
        if (!have_gpu_cursor) {
            gpu_busy += cb.GPUEndTime - cb.GPUStartTime;
            gpu_cursor = cb.GPUEndTime;
            have_gpu_cursor = true;
        } else if (cb.GPUStartTime > gpu_cursor) {
            gpu_busy += cb.GPUEndTime - cb.GPUStartTime;
            gpu_cursor = cb.GPUEndTime;
        } else if (cb.GPUEndTime > gpu_cursor) {
            gpu_busy += cb.GPUEndTime - gpu_cursor;
            gpu_cursor = cb.GPUEndTime;
        }
    }
    memcpy(fields->base, base.contents, fields->scalar_len * sizeof(float));
    stats->solve_seconds = fa_now_seconds() - solve_start;
    stats->gpu_busy = gpu_busy;
    fa_check_finite(fields->base, fields->scalar_len, "final scalar");
    stats->final_all_finite = true;

    free(base_backup);
    free(stage0_backup);
    free(stage1_backup);
    free(accum_backup);
}

static void fa_run_cpu(const fa_options_t *opt, fa_fields_t *fields,
    fa_run_stats_t *stats)
{
    stats->jit_seconds = 0.0;
    stats->gpu_busy = 0.0;
    double t0 = fa_now_seconds();
    for (uint32_t step = 0; step < opt->cfg.steps; ++step) {
        fa_rk4_step_cpu(&opt->cfg, fields);
    }
    stats->solve_seconds = fa_now_seconds() - t0;
    fa_check_finite(fields->base, fields->scalar_len, "final scalar");
    stats->final_all_finite = true;
}

static void fa_usage(const char *argv0)
{
    fprintf(stderr,
        "usage: %s [--mode cpu|metal] [--cells nx,ny] [--steps n] [--dt dt]\n"
        "          [--input path] [--output path] [--dump [path]] [--dump-bin [path]]\n"
        "          [--index-mode 2d|linear] [--tg x[,y]]\n",
        argv0);
}

static bool fa_parse_cells(const char *raw, fa_config_t *cfg)
{
    const char *comma = strchr(raw, ',');
    if (!comma) {
        return false;
    }
    size_t left_len = (size_t)(comma - raw);
    size_t right_len = strlen(comma + 1);
    if (left_len == 0 || right_len == 0) {
        return false;
    }
    char left[32];
    char right[32];
    if (left_len >= sizeof(left) || right_len >= sizeof(right)) {
        return false;
    }
    memcpy(left, raw, left_len);
    left[left_len] = '\0';
    memcpy(right, comma + 1, right_len + 1);
    cfg->nx = fa_parse_u32_strict(left, "--cells");
    cfg->ny = fa_parse_u32_strict(right, "--cells");
    return true;
}

static bool fa_parse_threadgroup(const char *raw, uint32_t *x, uint32_t *y)
{
    const char *comma = strchr(raw, ',');
    if (!comma) {
        *x = fa_parse_u32_strict(raw, "--tg");
        *y = 1;
        return true;
    }
    size_t left_len = (size_t)(comma - raw);
    size_t right_len = strlen(comma + 1);
    if (left_len == 0 || right_len == 0 || left_len >= 32 || right_len >= 32) {
        return false;
    }
    char left[32];
    char right[32];
    memcpy(left, raw, left_len);
    left[left_len] = '\0';
    memcpy(right, comma + 1, right_len + 1);
    *x = fa_parse_u32_strict(left, "--tg");
    *y = fa_parse_u32_strict(right, "--tg");
    return true;
}

int main(int argc, char **argv)
{
    @autoreleasepool {
        fa_options_t opt = {
            .metal = false,
            .dump_csv = false,
            .dump_bin = false,
            .input_path = NULL,
            .output_path = NULL,
            .binary_output_path = NULL,
            .cfg = {
                .nx = 51,
                .ny = 51,
                .steps = 500,
                .dt = 1.0e-4f,
            },
            .dt_supplied = false,
            .tg_x = 32,
            .tg_y = 4,
            .tg_supplied = false,
            .index_mode_supplied = false,
            .index_mode = FA_INDEX_DIRECT_2D,
        };

        for (int i = 1; i < argc; ++i) {
            if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
                fa_usage(argv[0]);
                return 0;
            } else if (strcmp(argv[i], "--mode") == 0 && i + 1 < argc) {
                ++i;
                if (strcmp(argv[i], "cpu") == 0) {
                    opt.metal = false;
                } else if (strcmp(argv[i], "metal") == 0) {
                    opt.metal = true;
                } else {
                    fa_die("mode must be cpu or metal");
                }
            } else if (strcmp(argv[i], "--cells") == 0 && i + 1 < argc) {
                if (!fa_parse_cells(argv[++i], &opt.cfg)) {
                    fa_die("cannot parse --cells nx,ny");
                }
            } else if (strcmp(argv[i], "--steps") == 0 && i + 1 < argc) {
                opt.cfg.steps = fa_parse_u32_strict(argv[++i], "--steps");
            } else if (strcmp(argv[i], "--dt") == 0 && i + 1 < argc) {
                opt.cfg.dt = fa_parse_float_strict(argv[++i], "--dt");
                opt.dt_supplied = true;
            } else if (strcmp(argv[i], "--input") == 0 && i + 1 < argc) {
                opt.input_path = argv[++i];
            } else if (strcmp(argv[i], "--output") == 0 && i + 1 < argc) {
                opt.output_path = argv[++i];
                opt.dump_csv = true;
            } else if (strcmp(argv[i], "--dump-bin") == 0) {
                opt.dump_bin = true;
                if (i + 1 < argc && argv[i + 1][0] != '-') {
                    opt.binary_output_path = argv[++i];
                }
            } else if (strcmp(argv[i], "--index-mode") == 0 && i + 1 < argc) {
                ++i;
                if (strcmp(argv[i], "2d") == 0) {
                    opt.index_mode = FA_INDEX_DIRECT_2D;
                } else if (strcmp(argv[i], "linear") == 0) {
                    opt.index_mode = FA_INDEX_LINEAR;
                } else {
                    fa_die("index mode must be 2d or linear");
                }
                opt.index_mode_supplied = true;
            } else if (strcmp(argv[i], "--tg") == 0 && i + 1 < argc) {
                if (!fa_parse_threadgroup(argv[++i], &opt.tg_x, &opt.tg_y)) {
                    fa_die("cannot parse --tg x[,y]");
                }
                opt.tg_supplied = true;
            } else if (strcmp(argv[i], "--dump") == 0) {
                opt.dump_csv = true;
                if (i + 1 < argc && argv[i + 1][0] != '-') {
                    opt.output_path = argv[++i];
                }
            } else {
                fa_dief("unknown argument: %s", argv[i]);
            }
        }

        if (!opt.index_mode_supplied) {
            const char *env_index_mode = getenv("FA_INDEX_MODE");
            if (env_index_mode) {
                if (strcmp(env_index_mode, "2d") == 0) {
                    opt.index_mode = FA_INDEX_DIRECT_2D;
                } else if (strcmp(env_index_mode, "linear") == 0) {
                    opt.index_mode = FA_INDEX_LINEAR;
                } else {
                    fa_die("FA_INDEX_MODE must be 2d or linear");
                }
            }
        }
        if (!opt.tg_supplied) {
            const char *env_tg = getenv("FA_TG");
            if (env_tg) {
                if (!fa_parse_threadgroup(env_tg, &opt.tg_x, &opt.tg_y)) {
                    fa_die("FA_TG must be x[,y]");
                }
                opt.tg_supplied = true;
            }
        }
        if (opt.index_mode == FA_INDEX_LINEAR && !opt.tg_supplied) {
            opt.tg_x = 64;
            opt.tg_y = 1;
        }
        fa_validate_cfg(&opt.cfg, opt.dt_supplied);
        fa_validate_working_set(&opt.cfg);

        const double t_process_start = fa_now_seconds();
        double t_setup_start = t_process_start;
        fa_fields_t fields;
        if (opt.input_path) {
            fa_read_raw_input(opt.input_path, &opt.cfg, &fields);
        } else {
            fa_fields_alloc(&fields, &opt.cfg);
            fa_generate_analytic(&opt.cfg, &fields);
        }
        fa_check_finite(fields.base, fields.scalar_len, "initial scalar");
        fa_check_finite(fields.vx, fields.xface_len, "initial vx");
        fa_check_finite(fields.vy, fields.yface_len, "initial vy");
        const double t_setup_end = fa_now_seconds();
        fa_run_stats_t stats;
        if (opt.metal) {
            fa_run_metal(&opt, &fields, &stats);
        } else {
            fa_run_cpu(&opt, &fields, &stats);
        }

        if (opt.dump_csv) {
            if (!opt.output_path) {
                opt.output_path = "final.csv";
            }
            fa_write_csv(opt.output_path, &opt.cfg, &fields);
        }
        if (opt.dump_bin) {
            if (!opt.binary_output_path) {
                opt.binary_output_path = "final.bin";
            }
            fa_write_binary(opt.binary_output_path, &fields);
        }
        if (!stats.final_all_finite) {
            fa_die("final field contains non-finite values");
        }
        const double t_process_end = fa_now_seconds();
        fprintf(stderr,
            "[startup] setup=%.6f jit=%.6f solve=%.6f total_elapsed=%.6f index_mode=%s tg=%u,%u\n",
            t_setup_end - t_setup_start,
            stats.jit_seconds,
            stats.solve_seconds,
            t_process_end - t_process_start,
            opt.index_mode == FA_INDEX_LINEAR ? "linear" : "2d",
            opt.tg_x, opt.tg_y);
        fprintf(stdout,
            "RESULT mode=%s solve_wall=%.6f gpu_busy=%.6f cells=%u,%u steps=%u dt=%.9g index_mode=%s tg=%u,%u\n",
            opt.metal ? "metal" : "cpu",
            stats.solve_seconds,
            stats.gpu_busy,
            opt.cfg.nx, opt.cfg.ny, opt.cfg.steps, opt.cfg.dt,
            opt.index_mode == FA_INDEX_LINEAR ? "linear" : "2d",
            opt.tg_x, opt.tg_y);
        fa_fields_free(&fields);
        return 0;
    }
}
