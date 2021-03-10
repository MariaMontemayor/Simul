#Tarea4.R
largos=data.frame()
dim<-  seq(40, 100, by=20)
sem<-seq(12, 42, by=10)

for (n in dim) {
  for (k in sem) {
      zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
      
      x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
      y <- rep(0, k) # igual como las coordenadas y de las semillas
      
      #Se colocan las semillas
      for (semilla in 1:k) {
        while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
          fila <- sample(1:n, 1)
          columna <- sample(1:n, 1)
          if (zona[fila, columna] == 0) {
            zona[fila, columna] = semilla
            x[semilla] <- columna
            y[semilla] <- fila
            break
          }
        }
      }
      
      #Se calcula para cada posición a cuál celda pertenece
      celda <-  function(pos) {
        fila <- floor((pos - 1) / n) + 1
        columna <- ((pos - 1) %% n) + 1
        if (zona[fila, columna] > 0) { # es una semilla
          return(zona[fila, columna])
        } else {
          cercano <- NULL # sin valor por el momento
          menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
          for (semilla in 1:k) {
            dx <- columna - x[semilla]
            dy <- fila - y[semilla]
            dist <- sqrt(dx^2 + dy^2)
            if (dist < menor) {
              cercano <- semilla
              menor <- dist
            }
          }
          return(cercano)
        }
      }
      
      suppressMessages(library(doParallel))
      registerDoParallel(makeCluster(detectCores() - 1))
      celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
      stopImplicitCluster()
      voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
      rotate <- function(x) t(apply(x, 2, rev))
      inicio=paste("p4s", "-", k, ".png", sep="")
      png(inicio)
      par(mar = c(0,0,0,0))
      image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
      graphics.off()
      final=paste("p4c", "-", k, ".png", sep="")
      png(final)
      par(mar = c(0,0,0,0))
      image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
      graphics.off()
    
    limite <- n # grietas de que largo minimo queremos graficar
    
    inicio <- function() {
      direccion <- sample(1:4, 1)
      xg <- NULL
      yg <- NULL
      if (direccion == 1) { # vertical
        xg <- 1
        yg <- sample(1:n, 1)
      } else if (direccion == 2) { # horiz izq -> der
        xg <- sample(1:n, 1)
        yg <- 1
      } else if (direccion == 3) { # horiz der -> izq
        xg <- n
        yg <- sample(1:n, 1)
      } else { # vertical al reves
        xg <- sample(1:n, 1)
        yg <- n
      }
      return(c(xg, yg))
    }
    
    vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
    for (dx in -1:1) {
      for (dy in -1:1) {
        if (dx != 0 | dy != 0) { # descartar la posicion misma
          vp <- rbind(vp, c(dx, dy))
        }
      }
    }
    names(vp) <- c("dx", "dy")
    vc <- dim(vp)[1]
    
    propaga <- function(replica) {
      prob <- 1 # entre fronteras
      dificil <- 0.99 # interno a la celda
      grieta <- voronoi # marcamos la grieta en una copia
      i <- inicio() # posicion inicial al azar
      xg <- i[1]
      yg <- i[2]
      largo <- 0
      while (TRUE) { # hasta que la propagacion termine
        grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
        largo <-  largo + 1
        frontera <- numeric()
        interior <- numeric()
        for (v in 1:vc) {
          vecino <- vp[v,]
          xs <- xg + vecino$dx # columna del vecino potencial
          ys <- yg + vecino$dy # fila del vecino potencial
          if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
            if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
              if (voronoi[yg, xg] == voronoi[ys, xs]) {
                interior <- c(interior, v)
              } else { # frontera
                frontera <- c(frontera, v)
              }
            }
          }
        }
        elegido <- 0
        if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
          if (length(frontera) > 1) {
            elegido <- sample(frontera, 1)
          } else {
            elegido <- frontera # sample sirve con un solo elemento
          }
          prob <- 1 # estamos nuevamente en la frontera
        } else if (length(interior) > 0) { # no hubo frontera para propagar
          if (runif(1) < prob) { # intentamos en el interior
            if (length(interior) > 1) {
              elegido <- sample(interior, 1)
            } else {
              elegido <- interior
            }
            prob <- dificil * prob # mas dificil a la siguiente
          }
        }
        if (elegido > 0) { # si se va a propagar
          vecino <- vp[elegido,]
          xg <- xg + vecino$dx
          yg <- yg + vecino$dy
        } else {
          break # ya no se propaga
        }
      }
      if (largo >= limite) {
        png(paste("p4g_", replica, ".png", sep=""))
        par(mar = c(0,0,0,0))
        image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
        graphics.off()
      }
      manhattan=function(largo){return(sum(abs((i[1]-xg)-(i[2]-yg))))}
      return(largo)
    }
    largos=rbind(largos,foreach(r = 1:200, .combine=c) %dopar% propaga(r))
  }
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

stopImplicitCluster()
summary(largos)
print(sem)
k12 = as.numeric(largos[1,])
k22 = as.numeric(largos[2, ])
k32 = as.numeric(largos[3, ])
k42 = as.numeric(largos[4, ])
png("p4_semillas2.png")
lbls = c("12", "22", "32", "42")
boxplot(n40, n60, n80, n100, col=rainbow(4), names = lbls, ylab="Distancia Manhattan", xlab="Tamaño de zona (n x n)", ylim = c(1, 200))
graphics.off()

n40 = as.numeric(largos[1, ])
n60 = as.numeric(largos[2, ])
n80 = as.numeric(largos[3, ])
n100 = as.numeric(largos[4, ])
png("p4_semillas.png")
lbls = c("40", "60", "80", "100")
boxplot(n40, n60, n80, n100, col=rainbow(4), names = lbls, ylab="Distancia Manhattan", xlab="Tamaño de zona (n x n)", ylim = c(1, 200))
graphics.off()

den <- density(c(t(largos)))
png("Density.png")
plot(den, xlab = "Datos distribuidos", ylab = "Distancia", main = NULL)
graphics.off()

uno=c(k12, n40)
dos=c(k12, n60)
tres=c(k12, n80)
cuatro=c(k12, n100)
cinco=c(k22, n40)
seis=c(k22, n60)
siete=c(k22, n80)
ocho=c(k22, n100)
nueve=c(k32, n40)
diez=c(k32, n60)
once=c(k32, n80)
doce=c(k32, n100)
trece=c(k42, n40)
catorce=c(k42, n60)
quince=c(k42, n80)
dieciseis=c(k42, n100)
png("totaldiagramas.png")
lbls=c("12,40", "12,60", "12,80","12,100","22,40", "22,60", "22,80","22,100","32,40", "32,60", "32,80","32,100","42,40", "42,60", "42,80","42,100")
boxplot(uno,dos,tres,cuatro,cinco,seis,siete,ocho,nueve,diez,once,doce,trece,catorce,quince,dieciseis, col=rainbow(4), names = lbls, ylab="Distancia Manhattan", xlab="Número de semillas y tamaño de zona", ylim = c(1, 200))
graphics.off()

