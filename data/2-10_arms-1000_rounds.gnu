reset
rounds = 1000
reps   = 1000
set xtics 0.2
set ytics 0.005
set yrange [0.9:1]
set xrange [0:5]
set grid
set key at 1.2, 0.95
set style data lines
set ylabel "Normalized average cumulative reward"
set xlabel "Observation noise"
set title "" . rounds . " rounds with " . reps . " repetitions"

filename(n) = \
    sprintf("%d-arms/%d-rounds/good-(5.0,2.0)_bad-%d-(3.0,2.0)_ob-0.1-10.0-0.1_reps-%d.data", \
    n, rounds, n - 1, reps)

plot for [num = 2 : 10] \
    filename(num) using 1:($2/5000):($3/5000) title "". num . " arms"
    
