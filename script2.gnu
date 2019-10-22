# formato y nombre de la imagen
set term png
set output "P3-19-20-fig2.png"

# muestra los ejes
set xzeroaxis
set yzeroaxis

# define el título
set title "x_a_r_r_e_l vs nº d'iteracions"

# define el rango de valores de los ejes que se muestra en pantalla
#set xrange[-0.5:3]
#set yrange[-6:3]

# define los títulos de los ejes
set xlabel "Nombre d'iteracions"
set ylabel "x_a_r_r_e_l"

# format dels nombres de l'eix y: 2 decimals
set format y '%.2f'
set format x '%.0f'

set key bottom

# plot 
plot "aux.dat" using 1:2 with lines t "x_0 = 0.2", \
"aux2.dat" using 1:2 with lines t "x_0 = 0.7", \
"aux3.dat" using 1:2 with lines t "x_0 = 1.5" lt rgb "red"
#pause -1
