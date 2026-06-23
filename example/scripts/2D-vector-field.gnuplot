# ===============================================================
#  vector-field.gnuplot  --  2D vector/quiver plot from a CSV
#  Line 1: column labels
#  Lines 2+: x, y, velocity_x, velocity_y data
#  Usage:  gnuplot -e "base_name='velocity'" 2D-vector-field.gnuplot
#  Default: base_name='velocity'
# ===============================================================

if (!exists("base_name")) base_name = "velocity"

datafile = base_name . ".csv"
set datafile separator ","

# --- 1. Read column headers from line 1 ---
xlabel = "" ; ylabel = "" ; dxlabel = "" ; dylabel = ""
set table $Dummy
    plot datafile every ::0::0 \
        using (xlabel=strcol(1), ylabel=strcol(2), \
               dxlabel=strcol(3), dylabel=strcol(4), 0):(0) \
        with table
unset table

# --- 2. Plot ---
set title  dxlabel . "," . dylabel . " at each " . xlabel . "," . ylabel
set xlabel xlabel
set ylabel ylabel
set key off
set cblabel "magnitude"

set terminal gif size 800,600
set output base_name . ".gif"

plot datafile every ::1 \
    using ($1-$3/2):($2-$4/2):3:4:(sqrt($3**2+$4**2)) \
    with vectors head filled size screen 0.02,15 lw 1.5 lc palette z title ""
