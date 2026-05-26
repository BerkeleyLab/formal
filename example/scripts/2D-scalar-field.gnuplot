# ============================================================================
#  2D-scalar-field.gnuplot  --  surface plot CSV
#  Line 1: column labels
#  Lines 2+: x, y, z data with blank lines between x-slices
#  Usage:  gnuplot -d "base_name='velocity-potential'" 2D-scalar-field.gnuplot
#  Default: base_name='velocity-potential'
# ============================================================================

if (!exists("base_name")) base_name = "velocity-potential"

datafile = base_name . ".csv"

set datafile separator ","

# --- 1. Read column headers from line 1 ---
xlabel = "" ; ylabel = "" ; zlabel = ""
set table $Dummy
    plot datafile every ::0::0 \
        using (xlabel=strcol(1), ylabel=strcol(2), zlabel=strcol(3), 0):(0) \
        with table
unset table

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

splot datafile every ::1 using 1:2:3 with pm3d title ""

set output    # flush and close the file
