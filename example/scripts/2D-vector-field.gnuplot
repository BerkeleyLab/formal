# ===============================================================
#  vector-field.gnuplot  --  2D vector/quiver plot from a CSV
#  Line 1: column labels (3 or 4 comma-separated fields)
#  Lines 2+: x, y, velocity_x, velocity_y data
#  Usage:  gnuplot -e "base_name='velocity'" 2D-vector-field.gnuplot
#  Default: base_name='velocity'
# ===============================================================
if (!exists("base_name")) base_name = "velocity"
datafile = base_name . ".csv"
set datafile separator ","

# --- 1. Read column headers from line 1 directly via the shell ------
#     Pulling the header out with "head | awk" (instead of asking
#     gnuplot's own "every ::0::0" to grab it) sidesteps two problems:
#     (a) if the file has blank lines splitting the data into multiple
#     blocks, "every ::0::0" without a block restriction samples the
#     first point of *every* block, not just the true header, and
#     (b) if the header has fewer fields than the data rows, asking
#     gnuplot's own strcol(4) for a column that doesn't exist falls
#     back to returning the literal text "4" instead of nothing --
#     awk's $4 on a short line just returns an empty string, which is
#     what we want.
get_field(n) = system("head -n 1 " . datafile . " | awk -F',' -v n=" . n . " '{v=$n; gsub(/^[ \\t]+|[ \\t]+$/,\"\",v); print v}'")
h1 = get_field(1)
h2 = get_field(2)
h3 = get_field(3)
h4 = get_field(4)

xlabel  = h1
ylabel  = h2
dxlabel = h3
dylabel = h4   # empty string if the header has no 4th field

# --- 2. Data source with the header line stripped off ----------------
#     Skipping the header via a shell "tail" pipe (rather than
#     gnuplot's "every ::1") means we don't need to worry about blank
#     lines resetting gnuplot's per-block point index -- every real
#     data row, including the first row of any block, gets plotted.
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
# Only show the column-4 label if one was actually present in the
# header; otherwise the title carries just the column-3 label with no
# dangling comma.
vel_label = (strlen(dylabel) > 0) ? (dxlabel . "," . dylabel) : dxlabel
set title  vel_label . " at each " . xlabel . "," . ylabel
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
