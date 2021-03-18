library(countcolors)
library(jpeg)
star=readJPEG('C:/Users/marii/OneDrive/Documentos/star.jpg')
print(star)

#Definición de rangos de color para usar en el paquete countcolors de R
white.center = c(1, 1, 1)
green.center=c(0, 0, 0)
#Cuenta exacta de pixeles por cada rango de color definido con el paquete countcolors de R
png("star-blanco.png")
star.white=countcolors::sphericalRange(star, center = white.center, radius = 0.5, color.pixels = FALSE, plotting = TRUE, target.color="cyan")  
blanco=star.white$pixel.count
graphics.off()
png("star-verde.png")
star.green=countcolors::sphericalRange(star, center = green.center, radius = 0.3, color.pixels = FALSE, plotting = TRUE, target.color="blue")  
verde=star.green$pixel.count
graphics.off()

#Si se asume que un pixel equivale a 10cm^2 del mural, y que un litro de pintura rinde 10m^2:
pixeles=c(blanco,verde)
pixeles
pintura=pixeles*0.001 #Litros de pintura
pintura

#Estimación con método Monte-Carlo
runs=1000
datos=data.frame()
for(r in 1:length(runs)){
  for(s in 1:10000){
    blue = star[,,3]
    x =sample (blue, runs[r])
    y=sum(x < 0.5)
    print(y)
    datos=rbind(datos,c(s,runs[r],y))
  }
}
names(datos)=c("Réplica","Muestra","Pixeles")
write.table(datos,"datosp.txt",sep="\t",quote=F,row.names=F)
pix = datos [,3]
pixm = mean(pix)
montecarloverde = pixm*(307343/runs)
montecarloverde
mcblanco=307343-montecarloverde
mcblanco
#Litros de pintura
pixelesmc=c(mcblanco,montecarloverde)
pixelesmc
pinturamc=pixelesmc*0.001 
pinturamc

