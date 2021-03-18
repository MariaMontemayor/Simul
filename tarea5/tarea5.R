# Tarea 5: Método Monte-Carlo
install.packages("distr")
library(distr)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)

n=c(50000,100000,500000,1000000)
replica=10
inicio= -6
final= -inicio
aum=0.25
x=seq(inicio,final,aum)

suppressMessages(library(distr))
f = function(x) { return(1 / (exp(x) + exp(-x))) }
g = function(x) { return((2 / pi) * f(x)) }
desde = 3
hasta = 7
cuantos = 500
generador = r(AbscontDistribution(d = g)) #Se crea un generador  

muestra <- generador(5000) #Se saca una muestra
png("t5-histograma.png")
hist(muestra, freq=F, breaks=50,
     main="",xlab="Muestra", ylab="Densidad",
     xlim=c(inicio, final), ylim=c(0, 0.4))
lines(x, g(x), col="purple") #Se dibuja g(x) encima del histograma
graphics.off()

parte = function() {
  valores = generador(muestra)
  return(sum(valores >= desde & valores <= hasta))
}
ResultadoEsp <-0.048834 #Se establece el resultado esperado
resultadoEsp_str <- strsplit(paste(resultadoEsp),split = "") #String del resultado esperado
datos <- c()
datos_str <- c()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for(muestra in n){
  for(r in 1:replica){
    montecarlo = foreach(i = 1:cuantos, .combine=c) %dopar% parte()
    integral = sum(montecarlo) / (cuantos * muestra)
    resultado=((pi / 2) * integral)
    datos <- c(datos,resultado)
    print(resultado)
  }
}
stopImplicitCluster()

resultados <- c()
resultados_str <- c()
res_contadores <- c()

for(i in 1:length(data.matrix(datos))) {
  resultados <- c(resultados,data.matrix(datos)[i])
  resultados_str <- c(resultados_str, strsplit(paste(resultados[i]), split=""))
}

for(i in 1:length(resultados)) {
  contador <- 0
  for(muestra in 1:7) {
    b <- (unlist(resultados_str[i])[muestra]) == (unlist(resultadoEsp_str)[muestra])
    if(b) {
      contador <- contador + 1
    } else {
      break;
    }
  }
  
  res_contadores <- c(res_contadores,contador-2)
}


png("t5-decimales.png")
Decimales_Esperadas = 5
graf <- data.frame(T.Muestra = c(rep("50000",10),rep("100000",10),rep("500000",10), rep("1000000",10)), Decimales = res_contadores)
graf2 <- data.frame(T.Muestra = c(rep("50000",10),rep("100000",10),rep("500000",10), rep("1000000",10)), Resultado = resultados)

g <- ggplot(graf, aes(T.Muestra,Decimales,fill=T.Muestra)) + geom_hline(aes(yintercept=Decimales_Esperadas,size=Decimales_Esperadas),colour="blue")  + geom_violin(alpha=0.5, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE,adjust=1.5) + geom_jitter() 
grid.arrange(g, nrow = 1)
graphics.off()

png("t5-resultados.png")
p <- ggplot(graf2, aes(T.Muestra,Resultado,fill=T.Muestra))+ geom_hline(aes(yintercept=resultadoEsp,size=ResultadoEsp),colour="purple")+ geom_violin(alpha=0.5, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE,adjust=1.5) + geom_jitter() 

grid.arrange(p, nrow = 1)
graphics.off()



png("Diagrama1.png")
plot(Decimales = res_contadores, T.Muestra=c(rep("100000",10)),type="b", pch=17, col="forestgreen", xlab="x", ylab="y", main = holis) + 
  lines(Decimales = res_contadores, T.Muestra=c(rep("50000",10)),col="blue",pch=18,type="b",lty=2)
legend(1, 95, legend=c("Línea 1", "Línea 2"),
       col=c("forestgreen", "blue"), lty=1:2, cex=0.8)
graphics.off()
  geom_line(colour="red")  + 
  geom_point( size=2, shape=21, fill="white", colour="red") + 
  theme_minimal()
graphics.off()
png("Diagrama2.png")
ggplot2(graf, aes(x=T.Muestra, y=Decimales)) + 
  geom_line(colour="red")  + 
  geom_point( size=2, shape=21, fill="white", colour="red") + 
  theme_minimal()
graphics.off()
