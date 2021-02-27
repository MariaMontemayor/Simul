#Tarea1.R correcta
library (parallel)

eucl<-FALSE

for (dim in 1:5){ #en dimensiones de 1 a 5
  for(expo in 4:9){ #exponencial de 4 a 9
    caminata <- 2**expo #en pasos de 2 elevado a los exponentes
    origenes<-numeric()
    for(replica in 1:30){ #en repeticiones de 1 a 30
      origenes<- c(origenes, movpar(dim, caminata))
      movpar<-function(dim, caminata){
        par <- rep(0,dim)
        regreso<-TRUE
        for(t in 1:caminata){
          cambiar <- sample(1:dim,1)
          if(runif(1)<0.5){
            par[cambiar] <- par[cambiar] + 1
          }else{
            par[cambiar] <- par[cambiar]-1
          }
          if(all(par==0)){
            return(t)
            regreso=TRUE
            break
          }
        }
        return(-1)
      }
    }
    regresaron<-origenes[origenes>0]
    cat(dim, caminata, length(origenes[origenes==-1])/30, '\n')
    print(summary(regresaron))
  }
}
source("distance.R")
source("caminata.R")
caminata(5, 30, ed.orig)
caminata(5, 30, md.orig)

if (eucl) {
  png("p1er.png")
  boxplot(data.matrix(datos), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", col=rainbow(5))
} else {
  png("p1mr.png")
  boxplot(data.matrix(datos), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima",col=rainbow(5))
}
graphics.off()

