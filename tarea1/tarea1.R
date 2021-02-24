#tarea1.R

library (parallel)
origenes=data.frame()
reg=data.frame()

for (expo in 4:9){
  caminata <- 2**expo #en pasos de 2 con exponenciales de 4 a 9
  for(dim in 1:5){ #dimensiones de 1 a 5
    for(rep in 1:30){ #repeticiones de 1 a 30
      par <- rep(0,dim)
      regreso <- FALSE
      for(t in 1:caminata){
        cambiar <- sample(1:dim,1)
        if(runif(1)<0.5){
          par[cambiar] <- par[cambiar] + 1
        }else{
          par[cambiar] <- par[cambiar]-1
        }
        if(all(par==0)){
          origenes <- rbind(origenes,c(caminata,dim,t))
          regreso=TRUE
          break
        }
      }
      if(!regreso){nreg=rbind(reg,c(caminata,dim))}
    }
  }
}

names(origenes)=c('caminata','dimension','tiempo')
names(reg)=c('caminata','dimension')

print(origenes)
