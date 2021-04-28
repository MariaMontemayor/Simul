library(ggplot2)
library(psych)

n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
mmax=max(p$m)
mmin=min(p$m)
p$m=(p$m-mmin)/(mmax-mmin) + 0.1 #Para obtener masas entre 0.1 y 1, no hay masas cero.

paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi=p[i,]$m 
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy)/(mi)) #Interacción de la masa con la fuerza de las partículas
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
system("rm -f p9_t*.png") # borramos anteriores (requiere bash)
tmax <- 50
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}

p$vel=numeric(n)
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  v=foreach(i=1:n,.combine=c)%dopar% sqrt((delta * f[c(TRUE, FALSE)][i])^2 + (delta * f[c(FALSE, TRUE)][i])^2)
  p$vel=p$vel+v
  
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("t9_t", tl, ".png", sep=""))
  plot(p$x, p$y, size=p$m, col=colores[p$g+6], pch=16, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
       main=paste("Paso", iter), xlab="X", ylab="Y")
  graphics.off()
}
stopImplicitCluster()

a <- seq(0, max(p$v), max(p$v)/5)
a <- sprintf("%.4f",a)
png("DiagramaTernario.png")
print(
  ggtern(data=p,aes(m / 5, v / max(v), abs(c))) +
    geom_mask() +
    geom_point(aes(colour=m)) +
    scale_color_gradient(low='pink',high='purple') +
    labs(x="Masa",y="Velocidad",z="Carga", color = "Masa") +
    scale_T_continuous(labels = a) +
    scale_L_continuous(labels = seq(0, 5, 1)) +
    scale_R_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
)
graphics.off()

