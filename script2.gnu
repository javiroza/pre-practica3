# formato y nombre de la imagen
#set term png
#set output "P3-19-20-fig2.png"

# muestra los ejes
set xzeroaxis
set yzeroaxis

# define el título
set title "f(x),f'(x)"

# define el rango de valores de los ejes que se muestra en pantalla
#set xrange[-0.5:3]
#set yrange[-6:3]

# define los títulos de los ejes
set xlabel "Nombre d'iteracions"
set ylabel "Valor de l'arrel"

# format dels nombres de l'eix y: n decimals
set format y '%.2f'
set format x '%.2f'

set key outside

# plotea las 2 primeras columnas del archivo entre comillas usando puntos
plot "aux.dat" 1:2, f(x),g(x),h(x)
pause -1
