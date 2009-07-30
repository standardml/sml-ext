
plot "tmp.dat" using 1:2 title "Analytical" with lines, \
     "tmp.dat" using 1:3 title "L=1, M=2" with lines, \
     "tmp.dat" using 1:4 title "L=2, M=1" with lines 

set term postscript

set output "tmp.ps"

replot

quit
