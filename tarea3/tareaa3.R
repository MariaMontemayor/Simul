#tarea3: Teoría de colas

library(parallel)
detectCores()

numprimos=read.csv("primes.txt", header = FALSE)
n=dim(numprimos)
print(length(numprimos))
noprimos=numprimos + 1
tareas=c(numprimos,noprimos)


primo <- function(n) {
  if (n < 4) {
    return(TRUE)
  }
  if (n %% 2 == 0) { # par
    return(FALSE)
  }
  for (i in seq(3, max(3, n - 1), 2)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
primos <- numeric()
for (n in nrow(numprimos)) {
  if (primo(n)) {
    primos <-  c(primos, n)
  }
}

original <- tareas
invertido <- rev(tareas)
aleatorio <- sample(tareas) 
replicas <- 10
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot <-  numeric()
it <-  numeric()
at <-  numeric()
for (r in 1:replicas) {
  ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
  it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
  at <- c(at, system.time(foreach(n = aleatorio, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
}
stopImplicitCluster()
summary(ot)
summary(it)
summary(at)

tiempo=t(rbind(ot,it,at))
png("t3.png")
boxplot(tiempo,xlab="Orden",ylab="Tiempo de ejecución (seg)",col=palette("Set 2"),border="black")
graphics.off()

names(tiempo)=c("Original","Invertido","Aleatorio")
write.table(tiempo,file="t3-tiempo.txt",quote=FALSE,sep="\t",row.names=FALSE)
