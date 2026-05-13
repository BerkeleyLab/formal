# =============================================================================
# Generate an GIF animated at 20 fps showing the mimetic and analytical
# solutions of the 1D Burgers Equation:
#
#   du/dt + u  du/dx = ν d^2u/dx^2
#
# where ν is the diffusivity.
#
# Domain              : x in [0, 2π]  (periodic)
# Initial Condition   : u(x,0) = 10*sin(x) 
# Boundary Conditions : u^(n)(0,t) = u^(n)(2π,t) for n^{th} derivative
#
# Usage:
#
#   gnuplot -e "dt=6.258068143677011E-4; frames=959; nu=1.0; results_file='burgers-order-4.dat'; animation_file='animated-burgers.gif'; order=4" animate-burgers.gnuplot
#
#   where each command-line option has the default value shown above if the option is not specified.
#
# Expected results-file format:
#
#   column  1      : abscissa (x)
#   columns 2... M : numerical solution at successive times separated by dt

if (!exists("dt"))             dt             = 6.258068143677011E-4
t_end = 0.6 # final time
if (!exists("frames"))         frames         = int(t_end / dt)
if (!exists("nu"))             nu             = 1.0
if (!exists("results_file"))   results_file   = "burgers-order-4.dat"
if (!exists("animation_file")) animation_file = "animated-burgers.gif"
if (!exists("order"))          order          = 4

# Obtain the exact solution via the Cole–Hopf transformation:
#
#   phi(x,0) = exp( A*cos(x) ),   A = 10/(2*nu) = 5
#
#   phi(x,t) = I_0(A) + 2 * sum_{n=1}^{N} I_n(A)*cos(n*x)*exp(-nu*n^2*t)
#
#   u(x,t)   = 4*nu * [sum_{n=1}^{N} n*I_n(A)*sin(n*x)*exp(-nu*n^2*t)]
#              / [I_0(A) + 2*sum_{n=1}^{N} I_n(A)*cos(n*x)*exp(-nu*n^2*t)]
#
# I_n(A) are modified Bessel functions computed via Miller's backward
# recurrence and normalised with  exp(A) = I_0 + 2*(I_1 + I_2 + ...).

# Solution parameters:
#
A = 10.0 / (2.0*nu)
N = 40               # Fourier terms (ample convergence for nu=1)

# ---------- Bessel function pre-computation ----------------------------------
# Miller backward recurrence: I_{n-1} = (2n/A)*I_n + I_{n+1}
# Start from n = N+2 with seed values, then normalise.

array IV[N+3]
IV[N+3] = 0.0
IV[N+2] = 1.0
do for [k = N+1 : 1 : -1] {
    IV[k] = (2.0*k / A) * IV[k+1] + IV[k+2]
}

# Normalise:  exp(A) = I_0(A) + 2 * sum_{n>=1} I_n(A)
norm_sum = IV[1]
do for [k = 1 : N+1] {
    norm_sum = norm_sum + 2.0 * IV[k+1]
}
scale = exp(A) / norm_sum

array I_n[N+1]          # I_n[n+1] = I_n(A),  n = 0 … N
do for [k = 0 : N] {
    I_n[k+1] = scale * IV[k+1]
}

# ---------- solution ---------------------------------------------------------
num(x,t) = sum [k=1:N] ( k * I_n[k+1] * sin(k*x) * exp(-nu*k*k*t) )
den(x,t) = 0.5*I_n[1]  + sum [k=1:N] ( I_n[k+1] * cos(k*x) * exp(-nu*k*k*t) )
u(x,t)   = 2.0*nu * num(x,t) / den(x,t)

# Determine the number of columns in the data file.
# total_cols includes the abscissa column; num_time_cols is the number of
# numerical solution snapshots (columns 2 … total_cols).
stats results_file nooutput
total_cols    = STATS_columns
num_time_cols = total_cols - 1

print sprintf("Read '%s': %d abscissa column + %d numerical solution columns.", \
              results_file, 1, num_time_cols)

# ---------- animated GIF terminal --------------------------------------------
set terminal gif animate delay 5 loop 0 size 900,600
# delay 5  →  5/100 s per frame  =  20 fps
# loop  0  →  loop forever
set output animation_file

# ---------- fixed plot cosmetics ---------------------------------------------
set title "1D Burgers Equation Solutions \n\nu_t + u u_x = νu_{xx}\n\n  IC: u(x,0) = 10 sin(x), BC: u^{(n)}(x,t) = u^{(n)}(x+2π,t), ν = 1" font "Arial,13"

set xlabel "x"                    font "Arial,12"
set ylabel "u(x,t)"               font "Arial,12"
set xrange [0 : 2*pi]
set yrange [-10 : 10]
set xtics ("0" 0,  "pi/2" pi/2,  "pi" pi, \
           "3pi/2" 3*pi/2,  "2pi" 2*pi)
set grid lc rgb "#cccccc"
set key top right font "Arial,11"

# Solid blue line for the exact solution
set style line 10 lc rgb "#1A5276" lw 2.5

# Red circles for the numerical solution
set style line 20 lc rgb "#C0392B" pt 7 ps 0.6

ordinal = (order == 1) ? "st" : \
          (order == 2) ? "nd" : \
          (order == 3) ? "rd" : "th"

# ---------- animation loop ---------------------------------------------------
do for [frame = 0 : frames-1] {
    t = frame * dt

    # Colour-map current time: blue
    set style line 10 lc rgb int(255) lw 2.5  # blue

    set label 1 sprintf("t = %.4f", t) \
        at graph 0.04, graph 0.93 \
        font "Arial Bold,13" tc rgb "black" \
        front

    plot u(x, t) ls 10 title sprintf("Analytical solution", t), \
         results_file using 1:(column(frame+2)) ls 20 title "Mimetic (" . sprintf("%d", order). ordinal . "-order, RK" . sprintf("%d", order) . ")"
}

set output   # flush / close GIF
print sprintf("Done – %d frames written to animated-burgers.gif", frames)
