#Práctica 12: red neuronal. Código base en Ref.[1].
install.packages("tidyr")
library(tidyr)

binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

######Probabilidades para cada color######
negro=c(0.995,0.95,0.80)
gris=c(0.92,0.80,0.85)
blanco=c(0.002,0.010,0.005)
datos=data.frame()

for(n in negro){
  for(g in gris){
    for(b in blanco){
      for(rep in 1:10){
        modelos <- read.csv("digits.txt", sep=" ", header=FALSE, stringsAsFactors=F)
        modelos[modelos=='n'] <- n
        modelos[modelos=='g'] <- g
        modelos[modelos=='b'] <- b
        
        r <- 5
        c <- 3
        dim <- r * c
        
        tasa <- 0.15
        tranqui <- 0.99
        
        tope <- 9
        digitos <- 0:tope
        k <- length(digitos)
        contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
        rownames(contadores) <- 0:tope
        colnames(contadores) <- c(0:tope, NA)
        
        n <- floor(log(k-1, 2)) + 1
        neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
        
        for (t in 1:5000) { # entrenamiento
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,]
          correcto <- binario(d, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            if (deseada != resultado) {
              ajuste <- tasa * (deseada - resultado)
              tasa <- tranqui * tasa
              neuronas[i,] <- w + ajuste * pixeles
            }
          }
        }
        
        for (t in 1:300) { # prueba
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
          correcto <- binario(d, n)
          salida <- rep(FALSE, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
          }
          r <- min(decimal(salida, n), k) # todos los no-existentes van al final
          contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
        }
        print(contadores)
        precision = diag(contadores) / colSums(contadores[,1:10])
        recall = diag(contadores) / rowSums(contadores)
        f1 = ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
        datos=rbind(datos,c(rep,n,g,b,f1))
      }
    }
  }
}

names(datos) <- c("Réplica", "Negro","Gris","Blanco","Cero", "Uno","Dos","Tres","Cuatro","Cinco","Seis","Siete","Ocho","Nueve")
write.table(datos,"datos.txt",sep="\t",quote=F,row.names = F)

NxGxBx=datos[1:10,5:14]
NxGxBy=datos[11:20,5:14]
NxGxBz=datos[21:30,5:14]
NxGyBx=datos[31:40,5:14]
NxGyBy=datos[41:50,5:14]
NxGyBz=datos[51:60,5:14]
NxGzBx=datos[61:70,5:14]
NxGzBy=datos[71:80,5:14]
NxGzBz=datos[81:90,5:14]

NyGxBx=datos[91:100,5:14]
NyGxBy=datos[101:110,5:14]
NyGxBz=datos[111:120,5:14]
NyGyBx=datos[121:130,5:14]
NyGyBy=datos[131:140,5:14]
NyGyBz=datos[141:150,5:14]
NyGzBx=datos[151:160,5:14]
NyGzBy=datos[161:170,5:14]
NyGzBz=datos[171:180,5:14]

NzGxBx=datos[181:190,5:14]
NzGxBy=datos[191:200,5:14]
NzGxBz=datos[201:210,5:14]
NzGyBx=datos[211:220,5:14]
NzGyBy=datos[221:230,5:14]
NzGyBz=datos[231:240,5:14]
NzGzBx=datos[241:250,5:14]
NzGzBy=datos[251:260,5:14]
NzGzBz=datos[261:270,5:14]

png("t12-digitos.png")
boxplot(NxGxBx,NxGxBy,NxGxBz,NxGyBx,NxGyBy,NxGyBz,NxGzBx,NxGzBy,NxGzBz,
        NyGxBx,NyGxBy,NyGxBz,NyGyBx,NyGyBy,NyGyBz,NyGzBx,NyGzBy,NyGzBz,
        NzGxBx,NzGxBy,NzGxBz,NzGyBx,NzGyBy,NzGyBz,NzGzBx,NzGzBy,NzGzBz,col=rainbow(10), xlab="Dígitos", ylab="Valor F",names=c("0","1","2","3","4","5","6","7","8","9"))
graphics.off()

NxGxBx=gather(datos[1:10,5:14])
NxGxBy=gather(datos[11:20,5:14])
NxGxBz=gather(datos[21:30,5:14])
NxGyBx=gather(datos[31:40,5:14])
NxGyBy=gather(datos[41:50,5:14])
NxGyBz=gather(datos[51:60,5:14])
NxGzBx=gather(datos[61:70,5:14])
NxGzBy=gather(datos[71:80,5:14])
NxGzBz=gather(datos[81:90,5:14])

NyGxBx=gather(datos[91:100,5:14])
NyGxBy=gather(datos[101:110,5:14])
NyGxBz=gather(datos[111:120,5:14])
NyGyBx=gather(datos[121:130,5:14])
NyGyBy=gather(datos[131:140,5:14])
NyGyBz=gather(datos[141:150,5:14])
NyGzBx=gather(datos[151:160,5:14])
NyGzBy=gather(datos[161:170,5:14])
NyGzBz=gather(datos[171:180,5:14])

NzGxBx=gather(datos[181:190,5:14])
NzGxBy=gather(datos[191:200,5:14])
NzGxBz=gather(datos[201:210,5:14])
NzGyBx=gather(datos[211:220,5:14])
NzGyBy=gather(datos[221:230,5:14])
NzGyBz=gather(datos[231:240,5:14])
NzGzBx=gather(datos[241:250,5:14])
NzGzBy=gather(datos[251:260,5:14])
NzGzBz=gather(datos[261:270,5:14])

vf1=cbind(NxGxBx$value,NxGxBy$value,NxGxBz$value,NxGyBx$value,NxGyBy$value,NxGyBz$value,NxGzBx$value,NxGzBy$value,NxGzBz$value,NyGxBx$value,NyGxBy$value,NyGxBz$value,NyGyBx$value,NyGyBy$value,NyGyBz$value,NyGzBx$value,NyGzBy$value,NyGzBz$value,NzGxBx$value,NzGxBy$value,NzGxBz$value,NzGyBx$value,NzGyBy$value,NzGyBz$value,NzGzBx$value,NzGzBy$value,NzGzBz$value)

png("t12_factor.png",width = 900, height = 500)
boxplot(vf1,col=colorRampPalette(c("purple", "white", "pink"))(27), ylab="Valor F",names=c("NxGxBx","NxGxBy","NxGxBz","NxGyBx","NxGyBy","NxGyBz","NxGzBx","NxGzBy","NxGzBz",
                                                                                            "NyGxBx","NyGxBy","NyGxBz","NyGyBx","NyGyBy","NyGyBz","NyGzBx","NyGzBy","NyGzBz",
                                                                                            "NzGxBx","NzGxBy","NzGxBz","NzGyBx","NzGyBy","NzGyBz","NzGzBx","NzGzBy","NzGzBz"),cex.names=0.5,las=2)
graphics.off()


