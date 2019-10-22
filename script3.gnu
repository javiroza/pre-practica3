# formato y nombre de la imagen
set term png
set output "P3-19-20-fig3.png"

# muestra los ejes
set xzeroaxis
set yzeroaxis

# define el título
set title "y'_e_x_a_c_t_e,y'_a_p_r_o_x,"

# define el rango de valores de los ejes que se muestra en pantalla
#set xrange[-0.5:3]
#set yrange[-6:3]

# define los títulos de los ejes
set xlabel "x"
set ylabel "y'_e_x_a_c_t_e,y'_a_p_r_o_x,"

# format dels nombres de l'eix y: n decimals
set format y '%.2f'
set format x '%.2f'

set key top left

# plotea las 2 primeras columnas del archivo entre comillas usando puntos
plot "P3-1920-res3-n34.dat" using 1:4 with points t "Valor exacte (n=34)", \
"P3-1920-res3-n34.dat" using 1:3 with points t "Valor aproximat (n=34)", \
"P3-1920-res3-n420.dat" using 1:4 with points t "Valor exacte (n=420)", \
"P3-1920-res3-n420.dat" using 1:3 with points t "Valor aproximat (n=420)"
#pause -1
