l <- 1.5
n <- 50 #Número de agentes
pi <- 0.05 #Probabilidad de infección
pr <- 0.02
v <- l / 30

pvvar <- seq(0,1,0.1) #Variacion de la probablidad de 0 a 1 
infectadosMax <- c()
for(pv in pvvar) { #Incremento en la probabilidad de vacunados
  for(rep in 1:10) { #Replicas del experimento 
    agentes <- data.frame(x = double(), y = double(),
                          dx = double(), dy = double(),
                          estado  = character())
    
    for (i in 1:n) {
      e <- "S"
      if(runif(1) < pv) { #Vacunar 
        e <- "R"
      } else { #Si no están vacunados
        
        if (runif(1) < pi) {
          e <- "I"
        }
        
      }
      
      agentes <- rbind(agentes, data.frame(x = runif(1, 0, l),
                                           y = runif(1, 0, l),
                                           dx = runif(1, -v, v),
                                           dy = runif(1, -v, v),
                                           estado = e))
      
      levels(agentes$estado) <- c("S", "I", "R")
    }
    
    epidemia <- integer()
    r <- 0.1
    ra <- 0.7
    pa <- 0.4
    ka <- 8
    tmax <- 50
    digitos <- floor(log(tmax, 10)) + 1
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
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
                if (d < ra) { # umbral amistad
                  p <- (r - d) / r
                  if (runif(1) < pa) {
                    for (i in 1:ka) { # iteraciones por la mitad
                      dx <- (a1$x - a2$x)/2
                      dy <- (a1$y - a2$y)/2
                    }
                  }
                }
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
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        a$x <- a$x + a$dx #Avanza con la velocidad en x 
        a$y <- a$y + a$dy #Avanza con la velocidad en y
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
        agentes[i, ] <- a    #parte grafica
      }
      
      aS <- agentes[agentes$estado == "S",]
      aI <- agentes[agentes$estado == "I",]
      aR <- agentes[agentes$estado == "R",]
      tl <- paste(tiempo, "", sep="")
      while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
      }
      salida <- paste("p6_t", tl, ".png", sep="")
      tiempo <- paste("Paso", tiempo)
      png(salida)
      plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
      if (dim(aS)[1] > 0) {
        points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
      }
      if (dim(aI)[1] > 0) {
        points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
      }
      if (dim(aR)[1] > 0) {
        points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
      }
      graphics.off()
    }
    
    salida <- paste("vacuna=", pv, ".png", sep="")
    png(salida)
    plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentaje de infectados",
         main=paste("Vacunados: ",pv * 100, "%"))
    graphics.off()
    infectadosMax <- c(infectadosMax, max(epidemia)) #Se guardan los infectados maximos por cada replica
  }
}

datos <- data.frame(Probabilidad_de_vacuna = c(rep("0",10),rep("0.1",10),
                                               rep("0.2",10),rep("0.3",10),
                                               rep("0.4",10),rep("0.5",10),
                                               rep("0.6",10),rep("0.7",10),
                                               rep("0.8",10),rep("0.9",10),
                                               rep("1",10)), 
                    Infectados_maximos = (infectadosMax/n)*100)

library(ggplot2)
tiff("t6-Violin.png", units="in", width=12, height=6.8, 
     res=300, compression = 'lzw')
g <- ggplot(datos, aes(Probabilidad_de_vacuna,Infectados_maximos))
g + geom_violin(alpha = 0.5,draw_quantiles = c(0.25, 0.5, 0.75), 
                trim = FALSE,adjust=1.5,aes(fill= Probabilidad_de_vacuna)) + geom_jitter() + labs(x= "Probabilidad de vacuna",y = "Porcentaje m\u{E1}ximo de infectados",fill ="Probabilidad de vacuna" )
graphics.off()

#Generación del GIF
library(gifski)
png_files <- list.files( pattern = "p6_t.*png$", full.names = TRUE)
gifski(png_files, gif_file = "animation.gif", width = 800, height = 600, delay = 1)


