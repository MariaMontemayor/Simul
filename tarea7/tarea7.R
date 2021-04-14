#Tarea 7: Búsqueda local
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

low <- -3
high <- 3
step <- 0.25
replicas <- 15
replica <- function(t){
  curr <- c(runif(1, low, high), runif(1, low, high))
  best <- curr
  for (tiempo in 1:t) {
    delta <- runif(1, 0, step)
    x1 <- curr + c(-delta,0)  #izquierda
    x2 <- curr + c(delta,0)   #derecha
    y1 <- curr + c(0,-delta)  #arriba
    y2 <- curr + c(0,delta)   #abajo
    
    puntos <- c(x1,x2,y1,y2)
    for(k in 1:8){
      if(puntos[k] < (-3)){
        puntos[k] <- puntos[k]+3 
      }
      if(puntos[k] > 3){
        puntos[k] <- puntos[k]-3
      }
    }
    vx <- c()
    vy <- c()
    for(p in 1:8){
      if(p %% 2 == 0){
        vy <- c(vy,puntos[p])
      }else{
        vx <- c(vx,puntos[p])
      }
    }
    vg <- c()
    for(q in 1:4){
      vg <- c(vg, g(vx[q], vy[q]) )
    }
    dm <- which.max(vg)
    curr <- c(vx[dm], vy[dm])
    if(g(curr[1],curr[2]) > g(best[1],best[2])){
      best <- curr
    }
  }
  return(best)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (pot in 1:40) { #Réplicas
  tmax <- pot
  resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
  
  vx <- c()
  vy <- c()
  aux <- (2*replicas)
  for(p in 1:aux){
    if(p %% 2 == 0){
      vy <- c(vy,resultados[p])
    }else{
      vx <- c(vx,resultados[p])
    }
  }
  
  valores <- c()
  for(q in 1:replicas){
    valores <- c(valores, g(vx[q], vy[q]))
  }
  mejor <- which.max(valores)
  x <- seq(-3, 3,length.out=500) #Para mejorar la calidad del gráfico
  y <-  x
  z <- outer(x, y, g)
  dimnames(z) <- list(x, y)
  d <- melt(z)
  names(d) <- c("x", "y", "z")
  
  png(paste0("t7_", tmax, ".png", sep=""), width=500, height=500)
  plot(levelplot(z ~ x * y, data = d, col.regions = viridis(200))) #Darle color al gráfico
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx, vy, pch=1, col="white", cex=2)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vx[mejor], vy[mejor], pch=20, col="red",cex=3)
  trellis.unfocus()
  
  graphics.off() 
}
stopImplicitCluster()
