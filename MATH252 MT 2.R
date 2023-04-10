#252MT2
#Problem 1
fxt = expression(3*cos(4*x-2*t), 'x','t')
fxt
D(fxt,'x') 
#-(3 * (sin(4 * x - 2 * t) * 4))
D(fxt,'t')
#3 * (sin(4 * x - 2 * t) * 2)

fxt = expression(6*sin(4*x-2*t), 'x','t')
fxt
D(fxt, 't')
#-(6 * (cos(4 * x - 2 * t) * 2))

fxt = expression(-12*sin(4*x-2*t), 'x','t')
fxt
D(fxt, 'x')
#-(12 * (cos(4 * x - 2 * t) * 4))

#Problem 2
D = 9
x = seq(-40, 40, len = 201)
T = function(x,t){(1/sqrt(D*t))*exp(-x^2/(4*D*t))}
t= 0.1
plot(x, T(x,t),
     type = 'l',
     xlab = 'x', ylab = 'Temperature T(x,t)',
     main = 'Heat diffusion',
     cex.lab = 1.3, cex.axis = 1.3,
     col = 'red')
t = 0.3
lines(x, T(x,t),
      type = 'l', col = 'orange')
t = 0.9
lines(x, T(x,t),
      type = 'l', col = 'yellow')
t = 3.0
lines(x, T(x,t),
      type = 'l', col = 'green')
t = 6.0
lines(x, T(x,t),
      type = 'l', col = 'blue')
t = 9.0
lines(x, T(x,t),
      type = 'l', col = 'black')
legend(-40, 0.95, 
       legend = c('t = 0.1', 't = 0.3',
                  't = 0.9', 't = 3.0',
                  't = 6.0', 't = 9.0'),
       col = c('red', 'orange', 'yellow',
               'green', 'blue', 'black'),
       lty=1, bty = 'n')

#Problem 4
#3D Surface
library(plotly)
par(mar = c(0,0,0,0.0))
n = 301
x =y= seq(-2, 2, len=n)
f = function(x,y){x^2+y^2-4*x*y}
z = outer(x, y, f)
w = z
for(i in 1:n){
  for(j in 1:n){
    if(w[i,j] > 4){z[i,j] = NaN}
  }
}
p <- plot_ly(x = ~x, y = ~y, z = ~ z, 
             type = 'surface')
hide_colorbar(p)

#Filled.contour map
par(mar = c(4.5,4.5,2,0.5))
filled.contour(x,y,w,  nlevels = 60,
               color.palette = colorRampPalette(
                 c("blue", "green", "yellow","orange", "red")),
               plot.title=title("Color map and contour levels", 
                                xlab="x", ylab="y", cex.lab=1.5),
               plot.axes = {axis(1, cex.axis = 1.5); 
                 axis(2, cex.axis = 1.5);
                 points(c(0, 1.3,-1.3), c(0, 1.1,-1.1), 
                        pch =16, cex= 1.2, col = 'white')})

#Problem 5
#Contour plot
del = 0.01
x = y = seq(-1.2*pi+del, 1.2*pi-del, len = 401)
f = function(x,y){cos(x)*cos(y)}
z = outer(x, y, f)
par(mar=c(4.0, 4.0, 0.5, 0.5))
contour(x,y,z, col = 'blue',
        levels = seq(-1,0, len=21),
        lty='dotted',
        xlab = 'x', ylab = 'y',
        cex.lab = 1.4, cex.axis = 1.4)
contour(x,y,z,
        levels = seq(0,1, len=21),
        col = 'red', add = TRUE)
