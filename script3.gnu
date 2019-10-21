# formato y nombre de la imagen
set term png
set output "P3-19-20-fig3.png"

# muestra los ejes
set xzeroaxis
set yzeroaxis

# define el título
set title "f(x),f'(x)"

# define el rango de valores de los ejes que se muestra en pantalla
set xrange[-0.5:3]
set yrange[-6:3]

# define los títulos de los ejes
set xlabel "x"
set ylabel "y,y'"

# format dels nombres de l'eix y: n decimals
set format y '%.2f'
set format x '%.2f'

set key outside
f(x) = sinh(x)*(35/16+0.5*x-(61/20)*x*x+x*x*x)
d(x) = cosh(x)*(35/16+0.5*x-(61/20)*x*x+x*x*x)+sinh(x)*(0.5-2*(61/20)*x+3*x*x)

# plotea las 2 primeras columnas del archivo entre comillas usando puntos
plot f(x),d(x)
