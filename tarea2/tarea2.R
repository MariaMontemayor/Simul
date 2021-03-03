#Regla 1
library(parallel)
celda <- 12
malla <-  celda^2
iter<-30
prob=numeric(length(0.5)-1)
datos=data.frame()

for(cor in 1:(0.50)-1){
  actual <- matrix(round(runif(malla)), nrow=celda, ncol=celda, byrow=TRUE)
  suppressMessages(library("sna"))
  png("p2_t0_r.png")
  plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  graphics.off()
  print(actual)

paso <- function(pos) {
  fila <- floor((pos -1)/ celda) +1
  columna <- ((pos-1) %% celda)+1
  vecinos <-  actual[max(fila - 1, 1) : min(fila + 1, celda),
                     max(columna - 1, 1): min(columna + 1, celda)]
  return(1 * ((sum(vecinos) - actual[fila, columna]) ==3))
}

print(actual)
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "celda")
clusterExport(cluster, "paso")

  for (iteracion in 1:iter) {
   valores=numeric()
   for (i in 1:malla) {valores=c(valores,paso(i))}
   datos=rbind(datos,c(cor,iteracion,vivos))
   clusterExport(cluster, "actual")
   siguiente <- parSapply(cluster, 1:malla, paso) 
   vivos = sum(siguiente)
   cat(iteracion, vivos, '\n')
   if (vivos == 0) { # todos murieron
      print("Todos se petatearon x.x.")
      prob[cor]=iteracion
      break;
    }
  names(datos)=c("Cor","Iteración","vivos")
  write.table(datos,"datos.txt",sep="\t",quote=F,row.names=F)
  actual <- matrix(siguiente, nrow=celda, ncol=celda, byrow=TRUE)
  print(actual)
  
  salida = paste("p2_t", iteracion, "_r.png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
}
}


png('regla1.png')
barplot(datos$vivos~datos$Iteración,xlab="Iteración",ylab="Vivos",col=palette("Pastel 2"),border="black")
dev.off()

stopCluster(cluster)

#Regla 2
library(parallel)
celda <- 12
malla <-  celda^2
iter<-30
prob=numeric(length(0.5)-1)
datos=data.frame()

for(cor in 1:(0.50)-1){
  actual <- matrix(round(runif(malla)), nrow=celda, ncol=celda, byrow=TRUE)
  suppressMessages(library("sna"))
  png("p2_t0_r.png")
  plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  graphics.off()
  print(actual)
  
  paso <- function(pos) {
    fila <- floor((pos -1)/ celda) +1
    columna <- ((pos-1) %% celda)+1
    vecinos <-  actual[max(fila - 1, 1) : min(fila + 1, celda),
                       max(columna - 1, 1): min(columna + 1, celda)]
    return(1 * ((sum(vecinos) - actual[fila, columna]) ==(1&3&5)))
  }
  
  print(actual)
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster, "celda")
  clusterExport(cluster, "paso")
  
  for (iteracion in 1:iter) {
    valores=numeric()
    for (i in 1:malla) {valores=c(valores,paso(i))}
    datos=rbind(datos,c(cor,iteracion,vivos))
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:malla, paso) 
    vivos = sum(siguiente)
    cat(iteracion, vivos, '\n')
    if (vivos == 0) { # todos murieron
      print("Todos se petatearon x.x.")
      prob[cor]=iteracion
      break;
    }
    names(datos)=c("Cor","Iteración","vivos")
    write.table(datos,"datos.txt",sep="\t",quote=F,row.names=F)
    actual <- matrix(siguiente, nrow=celda, ncol=celda, byrow=TRUE)
    print(actual)
    
    salida = paste("p2_t", iteracion, "_r.png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
  }
}


png('regla2.png')
barplot(datos$vivos~datos$Iteración,xlab="Iteración",ylab="Vivos",col=palette("Pastel 2"),border="black")
dev.off()

stopCluster(cluster)

#Regla 3
library(parallel)
celda <- 12
malla <-  celda^2
iter<-30
prob=numeric(length(0.5)-1)
datos=data.frame()

for(cor in 1:(0.50)-1){
  actual <- matrix(round(runif(malla)), nrow=celda, ncol=celda, byrow=TRUE)
  suppressMessages(library("sna"))
  png("p2_t0_r.png")
  plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  graphics.off()
  print(actual)
  
  paso <- function(pos) {
    fila <- floor((pos -1)/ celda) +1
    columna <- ((pos-1) %% celda)+1
    vecinos <-  actual[max(fila - 1, 1) : min(fila + 1, celda),
                       max(columna - 1, 1): min(columna + 1, celda)]
    return(1 * ((sum(vecinos) - actual[fila, columna]) ==(!3)))
  }
  
  print(actual)
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster, "celda")
  clusterExport(cluster, "paso")
  
  for (iteracion in 1:iter) {
    valores=numeric()
    for (i in 1:malla) {valores=c(valores,paso(i))}
    datos=rbind(datos,c(cor,iteracion,vivos))
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:malla, paso) 
    vivos = sum(siguiente)
    cat(iteracion, vivos, '\n')
    if (vivos == 0) { # todos murieron
      print("Todos se petatearon x.x.")
      prob[cor]=iteracion
      break;
    }
    names(datos)=c("Cor","Iteración","vivos")
    write.table(datos,"datos.txt",sep="\t",quote=F,row.names=F)
    actual <- matrix(siguiente, nrow=celda, ncol=celda, byrow=TRUE)
    print(actual)
    
    salida = paste("p2_t", iteracion, "_r.png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
  }
}


png('regla3.png')
barplot(datos$vivos~datos$Iteración,xlab="Iteración",ylab="Vivos",col=palette("Pastel 2"),border="black")
dev.off()

stopCluster(cluster)

#Regla 4
library(parallel)
celda <- 12
malla <-  celda^2
iter<-30
prob=numeric(length(0.5)-1)
datos=data.frame()

for(cor in 1:(0.50)-1){
  actual <- matrix(round(runif(malla)), nrow=celda, ncol=celda, byrow=TRUE)
  suppressMessages(library("sna"))
  png("p2_t0_r.png")
  plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  graphics.off()
  print(actual)
  
  paso <- function(pos) {
    fila <- floor((pos -1)/ celda) +1
    columna <- ((pos-1) %% celda)+1
    vecinos <-  actual[max(fila - 1, 1) : min(fila + 1, celda),
                       max(columna - 1, 1): min(columna + 1, celda)]
    return(1 * ((sum(vecinos) - actual[fila, columna]) ==(seq(from=2, to=8, by=2))))
  }
  
  print(actual)
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster, "celda")
  clusterExport(cluster, "paso")
  
  for (iteracion in 1:iter) {
    valores=numeric()
    for (i in 1:malla) {valores=c(valores,paso(i))}
    datos=rbind(datos,c(cor,iteracion,vivos))
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:malla, paso) 
    vivos = sum(siguiente)
    cat(iteracion, vivos, '\n')
    if (vivos == 0) { # todos murieron
      print("Todos se petatearon x.x.")
      prob[cor]=iteracion
      break;
    }
    names(datos)=c("Cor","Iteración","vivos")
    write.table(datos,"datos.txt",sep="\t",quote=F,row.names=F)
    actual <- matrix(siguiente, nrow=celda, ncol=celda, byrow=TRUE)
    print(actual)
    
    salida = paste("p2_t", iteracion, "_r.png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
  }
}


png('regla4.png')
barplot(datos$vivos~datos$Iteración,xlab="Iteración",ylab="Vivos",col=palette("Pastel 2"),border="black")
dev.off()

stopCluster(cluster)

#Regla 5
library(parallel)
celda <- 12
malla <-  celda^2
iter<-30
prob=numeric(length(0.5)-1)
datos=data.frame()

for(cor in 1:(0.50)-1){
  actual <- matrix(round(runif(malla)), nrow=celda, ncol=celda, byrow=TRUE)
  suppressMessages(library("sna"))
  png("p2_t0_r.png")
  plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  graphics.off()
  print(actual)
  
  paso <- function(pos) {
    fila <- floor((pos -1)/ celda) +1
    columna <- ((pos-1) %% celda)+1
    vecinos <-  actual[max(fila - 1, 1) : min(fila + 1, celda),
                       max(columna - 1, 1): min(columna + 1, celda)]
    return(1 * ((sum(vecinos) - actual[fila, columna]) ==4))
  }
  
  print(actual)
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster, "celda")
  clusterExport(cluster, "paso")
  
  for (iteracion in 1:iter) {
    valores=numeric()
    for (i in 1:malla) {valores=c(valores,paso(i))}
    datos=rbind(datos,c(cor,iteracion,vivos))
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:malla, paso) 
    vivos = sum(siguiente)
    cat(iteracion, vivos, '\n')
    if (vivos == 0) { # todos murieron
      print("Todos se petatearon x.x.")
      prob[cor]=iteracion
      break;
    }
    names(datos)=c("Cor","Iteración","vivos")
    write.table(datos,"datos.txt",sep="\t",quote=F,row.names=F)
    actual <- matrix(siguiente, nrow=celda, ncol=celda, byrow=TRUE)
    print(actual)
    
    salida = paste("p2_t", iteracion, "_r.png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
  }
}


png('regla5.png')
barplot(datos$vivos~datos$Iteración,xlab="Iteración",ylab="Vivos",col=palette("Pastel 2"),border="black")
dev.off()

stopCluster(cluster)


