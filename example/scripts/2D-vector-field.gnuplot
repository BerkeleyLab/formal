# ===============================================================
#  vector-field.gnuplot  --  2D vector/quiver plot from a CSV
#  Line 1: column labels (3 or 4 comma-separated fields)
#  Lines 2+: x, y, velocity_x, velocity_y data, optionally grouped
#            into blank-line-separated blocks (e.g. one block per
#            scan row of a grid).
#  Usage:  gnuplot -e "base_name='velocity'" 2D-vector-field.gnuplot
#  Default: base_name='velocity'
# ===============================================================
if (!exists("base_name")) base_name = "velocity"
datafile = base_name . ".csv"
set datafile separator ","

# --- 1. Read column headers from line 1 directly via the shell ------
#     Reading the header this way (instead of asking gnuplot's own
#     "every ::0::0" to grab it) sidesteps two problems with the old
#     approach: (a) if the file has blank lines splitting the data
#     into multiple blocks, "every ::0::0" without a block restriction
#     samples the first point of *every* block, not just the true
#     header, and (b) a header with fewer fields than the data rows
#     (e.g. one shared "velocity" column instead of separate
#     velocity_x/velocity_y) makes gnuplot treat the header record as
#     invalid when a 4th field is requested, throwing indexing off.
get_field(n) = system("head -n 1 " . datafile . " | awk -F',' -v n=" . n . " '{v=$n; gsub(/^[ \\t]+|[ \\t]+$/,\"\",v); print v}'")
h1 = get_field(1)
h2 = get_field(2)
h3 = get_field(3)
h4 = get_field(4)

xlabel = h1
ylabel = h2
if (strlen(h4) > 0) {
    dxlabel = h3
    dylabel = h4
} else {
    # Header only supplied one shared name (e.g. "velocity") for both
    # velocity components -- synthesize the two axis labels from it.
    dxlabel = h3 . "_x"
    dylabel = h3 . "_y"
}

# --- 2. Data source with the header line stripped off ----------------
#     Skipping the header via a shell "tail" pipe (rather than
#     gnuplot's "every ::1") means we don't need to worry about blank
#     lines resetting gnuplot's per-block point index, and every real
#     data row -- including the first row of every block -- gets
#     plotted.
data = "'< tail -n +2 " . datafile . "'"

# --- 3. Compute a uniform arrow half-length from the data range -----
#     We scan the data once to find the axis extents, then set
#     SCALE so every arrow spans ~3 % of the shorter axis.
eval("stats " . data . " using 1 nooutput")
x_span = STATS_max - STATS_min
eval("stats " . data . " using 2 nooutput")
y_span = STATS_max - STATS_min
SCALE = 0.030 * (x_span < y_span ? x_span : y_span)

# --- 4. Plot ----------------------------------------------------------
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
eval("plot " . data . " \
    using (\$1 - (\$3/sqrt(\$3**2+\$4**2+1e-30))*SCALE/2) \
        : (\$2 - (\$4/sqrt(\$3**2+\$4**2+1e-30))*SCALE/2) \
        : ((\$3/sqrt(\$3**2+\$4**2+1e-30))*SCALE) \
        : ((\$4/sqrt(\$3**2+\$4**2+1e-30))*SCALE) \
        : (sqrt(\$3**2+\$4**2)) \
    with vectors head filled size screen 0.02,15 lw 1.5 lc palette z title ''")
