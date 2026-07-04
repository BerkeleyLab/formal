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

# --- 2. Compute a uniform arrow half-length from the data range -----
#     We scan the data once to find the axis extents, then set
#     SCALE so every arrow spans ~3 % of the shorter axis.
stats datafile every ::1 using 1 nooutput
x_span = STATS_max - STATS_min
stats datafile every ::1 using 2 nooutput
y_span = STATS_max - STATS_min
SCALE = 0.030 * (x_span < y_span ? x_span : y_span)

# --- 3. Plot --------------------------------------------------------
set title  dxlabel . "," . dylabel . " at each " . xlabel . "," . ylabel
set xlabel xlabel
set ylabel ylabel
set key off
set cblabel "magnitude"
set terminal gif size 800,600
set output base_name . ".gif"

# Each arrow is drawn with a NORMALISED (unit) direction vector
# scaled to SCALE, so all arrows have identical length.
# The 5th column carries the raw magnitude and drives lc palette.
plot datafile every ::1 \
    using ($1 - ($3/sqrt($3**2+$4**2+1e-30))*SCALE/2) \
        : ($2 - ($4/sqrt($3**2+$4**2+1e-30))*SCALE/2) \
        : (($3/sqrt($3**2+$4**2+1e-30))*SCALE) \
        : (($4/sqrt($3**2+$4**2+1e-30))*SCALE) \
        : (sqrt($3**2+$4**2)) \
    with vectors head filled size screen 0.02,15 lw 1.5 lc palette z title ""
