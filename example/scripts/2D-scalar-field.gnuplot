# ============================================================================
#  2D-scalar-field.gnuplot  --  surface plot CSV
#  Line 1: column labels
#  Lines 2+: x, y, z data with blank lines between x-slices
#  Usage:  gnuplot -e "base_name='velocity-potential'" 2D-scalar-field.gnuplot
#  Default: base_name='velocity-potential'
# ============================================================================

if (!exists("base_name")) base_name = "velocity-potential"

datafile = base_name . ".csv"

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
get_field(n) = system("head -n 1 " . datafile . " | awk -F',' -v n=" . n . " '{v=$n; gsub(/^[ \\t]+|[ \\t]+$/,\"\",v); print v}'")
xlabel = get_field(1)
ylabel = get_field(2)
zlabel = get_field(3)

# --- 2. Plot ---
set title  zlabel . "(" . xlabel . ", " . ylabel . ")"
set xlabel xlabel ; set ylabel ylabel
set zlabel zlabel offset 3,0 ; set cblabel zlabel
set hidden3d
set pm3d depthorder
set palette rgbformulae 33,13,10
set ticslevel 0 ; set key off

set terminal gif size 800,600
set output base_name . ".gif"

# Header is skipped via a shell "tail" pipe rather than "every ::1", so the
# blank lines separating x-slices are preserved (pm3d still needs them) and
# no per-slice point gets silently dropped the way "every ::1" was doing.
splot "< tail -n +2 " . datafile . "" using 1:2:3 with pm3d title ""

set output    # flush and close the file
