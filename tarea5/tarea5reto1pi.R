n <- 100000
#Ejecuta muestras de una distribución uniforme
xs <- runif(n,min=-0.5,max=0.5)
ys <- runif(n,min=-0.5,max=0.5)
in.circle <- xs^2 + ys^2 <= 0.5^2
mc.pi <- (sum(in.circle)/n)*4
png("pi.png")
plot(xs,ys,pch='.',col=ifelse(in.circle,"purple","grey")
     ,xlab="",ylab="",asp=1,
     main=paste("Aproximación de Pi =",mc.pi))
graphics.off()
