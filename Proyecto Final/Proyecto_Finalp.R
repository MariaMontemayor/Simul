#PROYECTO FINAL: Simulacion de una Pandemia
# Código base por Schaeffer, se encuentra en el repositorio de GitHub: https://github.com/satuelisa/Simulation/blob/master/MultiAgent/SIR.R

library(ggplot2)
library(parallel)

l <- 1.5
n_vector <- c(20,50,100,200)
pi <- 0.15
pr <- 0.02
v <- l / 30
pv<-seq(0,1,.1)

############Modificaciones
base <- c()
pcb <- seq(0,1,.2) #probabilidad de portar cubrebocas
pd <- .01 #defunciones de los agentes infectados
prefs <- .1 #agentes recuperados con probabilidades de tener efectos secundarios debido al virus
pvefs <- .05 #agentes que se vacunaron con probabilidad de tener efectos secundarios debido a la vacuna
cluster <- makeCluster(detectCores(logical=FALSE) - 1)
vacuna = .1
cb = .1
n=20

foreach(n = n_vector, .combine = rbind) dopar {
  for (vacuna in pv) { 
    for (cb in pcb) {
      segundos <- system.time( {
        agentes <- data.frame(x = double(), y = double(),
                              dx = double(), dy = double(),
                              estado  = character(), cubrebocas = logical(),
                              sec_rec = logical(), sec_vac = logical())
##############        
        for (i in 1:n) {
          e <- "S"
          sv <- FALSE
          if (runif(1) < vacuna) {
            e <- "V"
            if (runif(1) < pvefs) {
              sv <- TRUE
            } else {
              sv <- FALSE
            }
          }
          else if (runif(1) < pi) {
            e <- "I"
          }
          ### Modificaciones
          agentes <- rbind(agentes, data.frame(x = runif(1, 0, l),
                                               y = runif(1, 0, l),
                                               dx = runif(1, -v, v),
                                               dy = runif(1, -v, v),
                                               estado = e,
                                               cubrebocas = FALSE,
                                               sec_rec = FALSE,
                                               sec_vac = sv))
          
        }
        
        levels(agentes$estado) <- c("S", "I", "R", "V", "D")
        epidemia <- integer()
        r <- 0.1
        tmax <- 100
        digitos <- floor(log(tmax, 10)) + 1
        
        for (tiempo in 1:tmax) {
          infectados <- dim(agentes[agentes$estado == "I",])[1]
          epidemia <- c(epidemia, infectados)
          agentes$cubrebocas <- FALSE
          
          for (j in 1:n) {
            if (sum(agentes[j,"estado"] == c("R","S", "I")) > 0) {
              if (runif(1) < cb) {
                agentes[j,"cubrebocas"] <- TRUE
              }
            }
          }
          
          contagios <- rep(FALSE, n)
          for (i in 1:n) { # posibles contagios
            a1 <- agentes[i, ]
            if (a1$estado == "I") { # desde los infectados
              for (j in 1:n) {
                if (!contagios[j]) { # aun sin contagio
                  a2 <- agentes[j, ]
                  if (a2$estado == "S") { # hacia los susceptibles
                    dx <- a1$x - a2$x
                    dy <- a1$y - a2$y
                    d <- sqrt(dx^2 + dy^2)
                    if (d < r) { # umbral
                      p <- (r - d) / r
                      if (runif(1) < p) {
                        contagios[j] <- TRUE
                      }
                    }
                  }
                }
              }
            }
          }
          
          for (i in 1:n) { # movimientos y actualizaciones
            a <- agentes[i, ]
            if (a$estado == "D") {
              break
            }
            if (contagios[i]) {
              a$estado <- "I"
            } else if (a$estado == "I") { # ya estaba infectado
              if (runif(1) < pr) {
                a$estado <- "R" # recupera
                if (runif(1) < prefs) {
                  a$sec_rec <- TRUE
                }
              }
              if (runif(1) < pd) {
                a$estado <- "D"
              }
            }
            if (a$estado == "D") {
              agentes[i, ] <- a
              break
            }
            a$x <- a$x + a$dx
            a$y <- a$y + a$dy
            if (a$x > l) {
              a$x <- a$x - l
            }
            if (a$y > l) {
              a$y <- a$y - l
            }
            if (a$x < 0) {
              a$x <- a$x + l
            }
            if (a$y < 0) {
              a$y <- a$y + l
            }
            agentes[i, ] <- a
          }
          
          aS <- agentes[agentes$estado == "S",]
          aI <- agentes[agentes$estado == "I",]
          aR <- agentes[agentes$estado == "R",]
          aV <- agentes[agentes$estado == "V",]
          aD <- agentes[agentes$estado == "D",]
          tl <- paste(tiempo, "", sep="")
          while (nchar(tl) < digitos) {
            tl <- paste("0", tl, sep="")
          }
          salida <- paste("p6_t", tl, ".png", sep="")
          time <- paste("Paso", tiempo)
          png(salida)
          ############# Modificaciones
          plot(l, type="n", main=time, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
          if (dim(aS)[1] > 0) {
            points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
          }
          if (dim(aI)[1] > 0) {
            points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
          }
          if (dim(aR)[1] > 0) {
            points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
          }
          if (dim(aV)[1] > 0) {
            points(aV$x, aV$y, pch=18, col="blue", bg="blue")
          }
          graphics.off()
        }
      })
      base_epidemia <- cbind(max(epidemia), c(segundos)[3], vacuna, n)
      print(base_epidemia)
      base <-  rbind(base, base_epidemia)
    }
  }
  return(base)
}
