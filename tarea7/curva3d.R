
library(rasterVis)
library(latticeExtra)
library(lattice)
library(sp)
library(viridisLite)
library(reshape2)
g <- function(x, y) {
  return(5*exp(-(0.8*y+1)^2 -(0.8*x)^2)*(0.8*x-1)^2-(exp(-(0.8*x+1)^2-(0.8*y)^2)/3)+exp(-(0.8*x)^2-(0.8*y)^2)*(10*(0.8*x)^3-2*(0.8*x)+10*(0.8*y)^5))
}
x <- seq(-3, 3, 0.25)
y <-  x
z <- outer(x, y, g)
png("t7_2d.png", width=700, height=700)
persp(x, y, z, shade=0.2, col=colors()[429], theta=40, phi=30)
graphics.off()
