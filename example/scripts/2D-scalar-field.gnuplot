# ============================================================================
#  2D-scalar-field.gnuplot  --  surface plot CSV(s)
#  Line 1: column labels
#  Lines 2+: x, y, z data with blank lines between x-slices
#
#  Usage (one plot):
#     gnuplot -e "base_name='velocity-potential'" 2D-scalar-field.gnuplot
#
#  Usage (two plots side by side, sharing the same x/y/z ranges):
#     gnuplot -e "base_name='velocity-potential'; base_name2='velocity-potential-2'" \
#             2D-scalar-field.gnuplot
#
#  Defaults: base_name='velocity-potential', base_name2='' (no second plot)
# ============================================================================

if (!exists("base_name"))  base_name  = "velocity-potential"
if (!exists("base_name2")) base_name2 = ""

two_plots = (base_name2 ne "")

datafile  = base_name  . ".csv"
datafile2 = base_name2 . ".csv"

set datafile separator ","

# --- 1. Read column headers from line 1 directly via the shell ------------
#     The data is split into blank-line-separated x-slices (needed so pm3d
#     draws the surface correctly). Because of that, gnuplot's own
#     "every ::0::0" doesn't just grab line 1: with no block restriction it
#     samples the first point of *every* slice, and the assignments in the
#     "using" clause just get overwritten slice by slice -- what's left at
#     the end is whatever the last slice's first point happened to be,
#     which is exactly the garbled numeric title ("0.180...E-34(-3.14...,
#     3.14...)") you were seeing. Reading the header straight off disk with
#     the shell sidesteps that entirely.
get_field(file,n) = system("head -n 1 " . file . " | awk -F',' -v n=" . n . " '{v=$n; gsub(/^[ \\t]+|[ \\t]+$/,\"\",v); print v}'")
xlabel = get_field(datafile,1)
ylabel = get_field(datafile,2)
zlabel = get_field(datafile,3)

# --- 2. Work out a single set of x/y/z ranges shared by both plots --------
#     "stats" only handles one or two "using" columns at a time, so each
#     axis is measured separately. Blank lines (the x-slice separators)
#     are simply skipped by stats, so they don't interfere.
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

# --- 3. Common plot styling (applies to both subplots) --------------------
set xlabel xlabel ; set ylabel ylabel
set zlabel zlabel offset 3,0 ; set cblabel zlabel
set hidden3d
set pm3d depthorder
set palette rgbformulae 33,13,10
set ticslevel 0 ; set key off

# Header is skipped via a shell "tail" pipe rather than "every ::1", so the
# blank lines separating x-slices are preserved (pm3d still needs them) and
# no per-slice point gets silently dropped the way "every ::1" was doing.
plotcmd(file) = "< tail -n +2 " . file

# --- 4. Render one or two panels, side by side, with identical axes -------
if (two_plots) {
    set terminal gif size 1500,700
    set output base_name . "_vs_" . base_name2 . ".gif"
    set multiplot layout 1,2

    set title zlabel . "(" . xlabel . ", " . ylabel . ")\n" . base_name
    splot plotcmd(datafile) using 1:2:3 with pm3d title ""

    set title zlabel . "(" . xlabel . ", " . ylabel . ")\n" . base_name2
    splot plotcmd(datafile2) using 1:2:3 with pm3d title ""

    unset multiplot
} else {
    set terminal gif size 800,600
    set output base_name . ".gif"

    set title zlabel . "(" . xlabel . ", " . ylabel . ")"
    splot plotcmd(datafile) using 1:2:3 with pm3d title ""
}

set output    # flush and close the file
