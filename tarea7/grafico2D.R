library(rasterVis)
library(latticeExtra)
library(lattice)
library(sp)
library(viridisLite)
library(reshape2)
library(viridis)
library(raster)

g <- function(x, y) {
  return (5*exp(-(0.8*y+1)^2 -(0.8*x)^2)*(0.8*x-1)^2-(exp(-(0.8*x+1)^2-(0.8*y)^2)/3)+exp(-(0.8*x)^2-(0.8*y)^2)*(10*(0.8*x)^3-2*(0.8*x)+10*(0.8*y)^5))
}
x <- seq(-3, 3, 0.25) 
y <-  x
z <- outer(x, y, f)

x <- seq(-3, 3,length.out=500) #Para mejorar la calidad del gráfico
y <-  x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x", "y", "z")

png(paste0("grafico2D", tmax, ".png", sep=""), width=500, height=500)
plot(levelplot(z ~ x * y, data = d, col.regions = viridis(200)))
graphics.off() 
