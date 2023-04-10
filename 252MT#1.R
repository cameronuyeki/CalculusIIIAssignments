#Math 252 Midterm #1

#Problem 1
#1(d)
acos(0)*180/pi
#[1] 90 deg


#Problem 2
#2(a)
par(mar=c(4,4,2,0.2))
n = 41
ome = 2
t = seq(0, 2*pi, len=n)
x = 4*sin(ome*t) + 9*sin(t)
y = 4*cos(ome*t) + 9*cos(t)
plot(x, y, pch = 16, cex = 0.7,
     cex.lab =1.3, cex.axis = 1.3,
     main = 'Heart-shape path of a point')
s = seq(length(x))
arrows(x[s], y[s], x[s+1], y[s+1], 
       col = 1:(length(s)-1),
       angle = 30, length = 0.1)

#2(b)
x = expression(4*sin(2*t) + 9*sin(t), 't')
y = expression(4*cos(2*t) + 9*cos(t), 't')
D(x,'t')
#4 * (cos(2 * t) * 2) + 9 * cos(t)
D(y,'t')
#-(9 * sin(t) + 4 * (sin(2 * t) * 2))

#2(c)
D(D(x,'t'),'t')
#-(9 * sin(t) + 4 * (sin(2 * t) * 2 * 2))
D(D(y,'t'),'t')
#-(9 * cos(t) + 4 * (cos(2 * t) * 2 * 2))

#2(d)
t= 1.5;
#velocity
4 * (cos(2 * t) * 2) + 9 * cos(t)
#-7.283305
-(9 * sin(t) + 4 * (sin(2 * t) * 2))
#-10.10641

#acceleration
-(9 * sin(t) + 4 * (sin(2 * t) * 2 * 2))
#-11.23538
-(9 * cos(t) + 4 * (cos(2 * t) * 2 * 2))
#15.20325

#2(e)
t=2.3
4 * (cos(2 * t) * 2) + 9 * cos(t)
#-6.893704
-(9 * sin(t) + 4 * (sin(2 * t) * 2))
#1.238181
#speed
sqrt((-6.893704)^2+(1.238181)^2)
#7.004016

#2(f)
t=2.3
-(9 * sin(t) + 4 * (sin(2 * t) * 2 * 2))
#9.187709
-(9 * cos(t) + 4 * (cos(2 * t) * 2 * 2))
#7.790925
sqrt((9.187709)^2+(7.790925)^2)
#12.04627


#Problem 3
#3(a)
x = t = seq(-2, 2, len = 41)
y = (1/3)*x^3
plot(x, y, type = 'l', main = 'Parabola')

#3(b)
#Hand work written on pdf doc

#3(c)
yt = function(t){sqrt(1+t^4)}
integrate(yt, -2, 2)
#7.306969 with absolute error < 2e-08
