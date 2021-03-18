#Tarea 5 reto 1
library(parallel)

pi=3.1415926
replicas<-20
n=c(100,1000,10000,1000000,5000000)
cuantos=500

Números<-numeric()
errores<-numeric()
Muestra<-numeric()
pis<-numeric()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

calculando <- function() {
  xs <- runif(i,min=-0.5,max=0.5)
  ys <- runif(i,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  numero <- (sum(in.circle)/i)*4
  return(numero)
}
pis <- numeric()
replica<-numeric()
Muestra<-numeric()
for(i in n){
  for(r in 1:replicas){
    montecarlo<-foreach(i = 1:cuantos, .combine=c) %dopar% calculando()
    real <- sum(montecarlo) / cuantos
    Números <- c(Números, real)
    error<-((abs(pi-real))/(pi))*100
    errores<-c(errores,error)
  }
}
stopImplicitCluster()
total<-replicas*(length(n))
pis<-rep(3.1415926,total)
replica<-rep(1:replicas,length(n))
for (i in n) {
  Muestra <- c(Muestra, c(rep(i, replicas)))
}
datos<-data.frame()
datos<-cbind(replica,Muestra,Números,pis,errores)

png("t5r1-error.png")
vgraf<-data.frame(Muestra=c(rep("100",10),rep("1000",10),rep("100000",10), rep("1000000",10), rep("5000000",10)), Errores = errores)
v <- ggplot(vgraf, aes(Muestra,Errores,fill=Muestra)) + geom_violin(alpha=0.5, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE,adjust=1.5) + geom_jitter() + xlab("Muestras") + ylab("Error absoluto")
grid.arrange(v, nrow = 1)
graphics.off()

png("t5r1-pi.png")
vgraf2<-data.frame(Muestra=c(rep("100",10),rep("1000",10),rep("100000",10), rep("1000000",10), rep("5000000",10)), Números = Números)
v2 <- ggplot(vgraf2, aes(Muestra,Números,fill=Muestra)) + geom_hline(aes(yintercept=pi,size=pi),colour="blue") + geom_violin(alpha=0.5, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE,adjust=1.5) + geom_jitter() + xlab("Muestras") + ylab("Valores") 
grid.arrange(v2, nrow = 1)
graphics.off()
