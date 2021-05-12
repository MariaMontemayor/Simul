library(ggplot2)

######################
#creando funciones
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

######################
#Creación de variables
vc <- 4 #Variables
md <- 3 #Degree
tc <- 5 #Terminos
k <- 2 # cuantas funciones objetivo
obj <- list()
r <- 30 #Numero de replicas
vectorfunciones <- c(2,4,6,8)
poncentajefrente<-matrix(0, nrow=r, ncol=length(vectorfunciones))
for(f in vectorfunciones) {
  k <- f #Cuantas funciones objetivo
  
  ######################
  #Creación de Replicas
  for (q in 1:r) {
    
    ######################
    #Creacion de polinomios
    for (i in 1:k) {
      obj[[i]] <- poli(md, vc, tc)
    }
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim)
    n <- 200 # cuantas soluciones aleatorias
    sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
    val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    
    ######################
    #Creación de soluciones
    for (i in 1:n) { # evaluamos las soluciones
      for (j in 1:k) { # para todos los objetivos
        val[i, j] <- eval(obj[[j]], sol[i,], tc)
      }
    }
    mejor1 <- which.max(sign[1] * val[,1])
    mejor2 <- which.max(sign[2] * val[,2])
    cual <- c("max", "min")
    xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
    yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
    #png("p11_init.png")
    #plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
    #graphics.off()
    
    ######################
    #Graficar soluciones (Mejores)
    
    #png("p11_mejores.png")
    #plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
    #     ylab=paste(yl,"mejor con bolita naranja"),
    #     main="Ejemplo bidimensional")
    #points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
    #points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
    #graphics.off()
    
    ######################
    #Creando puntos dominantes
    no.dom <- logical()
    dominadores <- integer()
    for (i in 1:n) {
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,]))
      }
      cuantos <- sum(d)
      dominadores <- c(dominadores, cuantos)
      no.dom <- c(no.dom, cuantos == 0) # nadie le domina
    }
    frente <- subset(val, no.dom) # solamente las no dominadas
    
    ######################
    #Graficar dominantes
    
    #png("p11_frente.png")
    #plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
    #points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
    #graphics.off()
    
    ######################
    #Graficar violin
    #library(ggplot2) # recordar instalar si hace falta
    #data <- data.frame(pos=rep(0, n), dom=dominadores)
    #png("p11_violin.png")
    #gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
    #gr + geom_boxplot(width=0.2, fill="blue", color="green", lwd=2) +
    #  xlab("") +
    #  ylab("Frecuencia") +
    #  ggtitle("Cantidad de soluciones dominantes")
    #graphics.off()
    
    ######################
    #Porcentaje de cada frente entre soluciones
    poncentajefrente[q,(f/2)]  <- (length(frente[,1])/n)*100
  }
}

######################
#Graficando Porcentaje de Frente 
png("t11_diagrama.png")
frentes<-c(poncentajefrente[,1], poncentajefrente[,2], poncentajefrente[,3], poncentajefrente[,4])
funciones<-c(rep(2, r), rep(4, r), rep(6, r), rep(8, r))
funciones<-as.factor(funciones)
data2 <- data.frame(pos=funciones, dom=frentes)

g2 <- ggplot(data2, aes(x=pos, y=dom),pch=17, col="forestgreen") + geom_violin(fill="mediumpurple1", trim = FALSE,adjust=1.5) + geom_jitter() 
g2 + geom_boxplot(width=0.1, fill="lightcoral", color="lightpink", lwd=1) +
  xlab("") +
  ylab("Frecuencia") 
geom_line(colour="red")  +geom_point( size=2, shape=21, fill="mediumpurple1", colour="red") +
  theme_minimal()
graphics.off()

#En base a los resultados del "violin plot" podemos obersvar que mientras mayor sea el numero de funciones objetivo, 
#mayor será el porcentaje de frentes de pareto entre la cantidad de soluciones existentes. Se pudiera inferir
#que este resultado tiene como causa que al haber muchos funciones que se tienen que optimizar, las soluciones
#existentes si bien no cumplen con algunas funciones en especifico, pudieran estar optimizando en gran manera alguna
#otra, en cierta manera lo veo de esta forma, cada solución al menos le tiene que gustar a alguna función,
#en terminos coloquiales, para cada roto, hay al menos un descosido.

######################
#Pruebas estadisticas de comparación de medias
t.test(poncentajefrente[,1], poncentajefrente[,2])$p.value
t.test(poncentajefrente[,1], poncentajefrente[,3])$p.value
t.test(poncentajefrente[,1], poncentajefrente[,4])$p.value
t.test(poncentajefrente[,2], poncentajefrente[,3])$p.value
t.test(poncentajefrente[,2], poncentajefrente[,4])$p.value
t.test(poncentajefrente[,3], poncentajefrente[,4])$p.value

#Mayoría de las pruebas realizadas muestran que las medias son estadisticamentes diferentes entre las 
#diferentes cantidades de funciones objetivo
