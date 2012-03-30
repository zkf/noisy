reset
reps = 1000
rounds = 10000
set xtics 0.5
set ytics 0.01
set yrange [0.9:1]
set xrange [0:7]
set grid
set key at 1.5, 0.93
set ylabel "Normalized average cumulative reward"
set xlabel "Observation noise"
set title "" . rounds . " rounds with " . reps . " repetitions"
set style data lines

plot for [num = 10 : 50 : 10] \
    "" . num . "-arms/" . rounds . "-rounds/good-(5.0,2.0)_bad-" . (num - 1) . \
        "-(3.0,2.0)_ob-0.1-10.0-0.1_reps-" . reps . ".data" \
    using 1:($2/50000):($3/50000) \
    title "" . num . " arms" 
    
    
    
