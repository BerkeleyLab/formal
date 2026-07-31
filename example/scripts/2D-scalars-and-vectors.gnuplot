# ============================================================================
#  2D-scalar-field.gnuplot  --  one surface/vector-field GIF per input file
#
#  Scalar-field files (base_name, base_name2):
#     Line 1: column labels
#     Lines 2+: x, y, z data with blank lines between x-slices
#
#  Vector-field file (vec_name), optional third file:
#     Line 1: column labels (x, y, <vector label>)
#     Lines 2+: x, y, vx, vy  (plain rows, one grid point per line)
#
#  Every file supplied produces its own GIF (base_name.gif, base_name2.gif,
#  vec_name.gif), each a single plot. All plots that are generated together
#  in one run share the same x/y/z ranges and the same 3D view angle, so
#  they stay visually comparable even though they're separate files -- and
#  the vector plot is drawn as a flat plane at the same tilt as the x-y
#  plane under the surface plots, not a top-down 2D map.
#
#  Usage (one surface):
#     gnuplot -e "base_name='velocity-potential'" 2D-scalar-field.gnuplot
#
#  Usage (two surfaces, sharing the same x/y/z ranges):
#     gnuplot -e "base_name='velocity-potential'; base_name2='velocity-potential-2'" \
#             2D-scalar-field.gnuplot
#
#  Usage (add a horizontal-plane vector-field GIF):
#     gnuplot -e "base_name='velocity-potential'; vec_name='velocity'" \
#             2D-scalar-field.gnuplot
#
#  All three together (three GIFs: two surfaces + vectors):
#     gnuplot -e "base_name='velocity-potential'; base_name2='velocity-potential-2'; \
#                 vec_name='velocity'" 2D-scalar-field.gnuplot
#
#  Optional overrides for the vector plot (auto-computed if left unset):
#     vec_stride  - plot every Nth grid point in each direction (thins arrows)
#     vec_scale   - multiplier applied to (vx,vy) before drawing each arrow
#
#  Defaults: base_name='velocity-potential', base_name2='' (no 2nd surface),
#            vec_name='' (no vector plot)
# ============================================================================

if (!exists("base_name"))   base_name   = "velocity-potential"
if (!exists("base_name2"))  base_name2  = ""
if (!exists("vec_name"))    vec_name    = ""
if (!exists("vec_stride"))  vec_stride  = -1   # -1 = auto
if (!exists("vec_scale"))   vec_scale   = -1   # -1 = auto

two_plots = (base_name2 ne "")
have_vec  = (vec_name ne "")

datafile  = base_name  . ".csv"
datafile2 = base_name2 . ".csv"
vecfile   = vec_name   . ".csv"

set datafile separator ","

# --- 1. Read column headers from line 1 directly via the shell ------------
get_field(file,n) = system("head -n 1 " . file . " | awk -F',' -v n=" . n . " '{v=$n; gsub(/^[ \\t]+|[ \\t]+$/,\"\",v); print v}'")
xlabel = get_field(datafile,1)
ylabel = get_field(datafile,2)
zlabel = get_field(datafile,3)
if (have_vec) vlabel = get_field(vecfile,3)

# Build each surface plot's title from the text after the first hyphen in
# its base name, e.g. "scalar-initial" -> "Passive Scalar Initial
# Concentration". toupper() isn't a gnuplot builtin, so the first-letter
# capitalization is done with a short shell/awk round-trip via system().
cap_first(s) = (strlen(s) > 0 ? system("echo " . s . " | awk '{print toupper(substr($0,1,1)) substr($0,2)}'") : s)
after_hyphen(s) = (strstrt(s,"-") > 0 ? s[strstrt(s,"-")+1:strlen(s)] : s)
scalar_title(s) = "Passive Scalar " . cap_first(after_hyphen(s)) . " Concentration"

# --- 2. Work out a single set of x/y/z ranges shared by all panels --------
stats datafile using 1 nooutput
xmin = STATS_min ; xmax = STATS_max
stats datafile using 2 nooutput
ymin = STATS_min ; ymax = STATS_max
stats datafile using 3 nooutput
zmin = STATS_min ; zmax = STATS_max

if (two_plots) {
    stats datafile2 using 1 nooutput
    xmin = (STATS_min < xmin) ? STATS_min : xmin
    xmax = (STATS_max > xmax) ? STATS_max : xmax
    stats datafile2 using 2 nooutput
    ymin = (STATS_min < ymin) ? STATS_min : ymin
    ymax = (STATS_max > ymax) ? STATS_max : ymax
    stats datafile2 using 3 nooutput
    zmin = (STATS_min < zmin) ? STATS_min : zmin
    zmax = (STATS_max > zmax) ? STATS_max : zmax
}

set xrange [xmin:xmax]
set yrange [ymin:ymax]
set zrange [zmin:zmax]
set cbrange [zmin:zmax]

# --- 2b. Work out arrow thinning/scaling for the vector panel -------------
if (have_vec) {
    stats vecfile using 1 nooutput
    vxmin = STATS_min ; vxmax = STATS_max
    stats vecfile using 2 nooutput
    vymin = STATS_min ; vymax = STATS_max
    stats vecfile using (sqrt(column(3)**2 + column(4)**2)) nooutput
    vmax = STATS_max

    stats vecfile using (abs(column(2)-vymin)<1e-9 ? column(1) : 1/0) nooutput
    nx = STATS_records                         # grid points per row

    target_arrows = 25
    if (vec_stride > 0) {
        stride = vec_stride
    } else {
        stride = floor(nx/target_arrows + 0.5)
        if (stride < 1) { stride = 1 }
    }

    row(n) = floor(n/nx)
    col(n) = n - row(n)*nx
    keep(n) = ( (int(row(n)) % int(stride) == 0) && (int(col(n)) % int(stride) == 0) ) ? 1 : 0

    if (vec_scale > 0) {
        vscale = vec_scale
    } else {
        grid_spacing = (vxmax-vxmin) / (nx/stride)
        vscale = (vxmax > 0) ? 0.8*grid_spacing/vmax : 1
    }
}

# --- 3. Common plot styling (applies to all panels) ------------------------
if (!exists("view_rotx")) view_rotx = 60   # same 3D view for every panel, so
if (!exists("view_rotz")) view_rotz = 30   # the vector floor lines up with
set view view_rotx,view_rotz               # the surfaces' x-y plane below them

set xlabel xlabel ; set ylabel ylabel
# Removed z-axis label as requested previously
set hidden3d
set pm3d depthorder
set palette rgbformulae 33,13,10
set ticslevel 0 ; set key off

# Header is skipped via a shell "tail" pipe rather than "every ::1", so the
# blank lines separating x-slices are preserved (pm3d still needs them) and
# no per-slice point gets silently dropped the way "every ::1" was doing.
plotcmd(file) = "< tail -n +2 " . file

# --- 4. Render one GIF per input file (each its own plot, own output) -----
set terminal gif size 800,650

set output base_name . ".gif"
set title scalar_title(base_name)
splot plotcmd(datafile) using 1:2:3 with pm3d title ""
set output

if (two_plots) {
    set output base_name2 . ".gif"
    set title scalar_title(base_name2)
    splot plotcmd(datafile2) using 1:2:3 with pm3d title ""
    set output
}

if (have_vec) {
    # --- Dynamic Title Generation ---
    cap_vec_name = (strlen(vec_name) > 0 ? system("echo " . vec_name . " | awk '{print toupper(substr($0,1,1)) substr($0,2)}'") : vec_name)
    
    # 3. Construct the final title: "Velocity Vector Field"
    dynamic_title = cap_vec_name . " Vector Field"

    # 4. Determine the output filename with capitalized name
    output_filename = cap_vec_name . ".gif"
    
    set output output_filename
    set title dynamic_title
    
    splot plotcmd(vecfile) using (keep($0) ? column(1) : 1/0):2:(zmin):(column(3)*vscale):(column(4)*vscale):(0) \
          with vectors filled head size 0.08,20 lw 1.3 lc rgb "#1a5fb4" title ""
    set output
}
