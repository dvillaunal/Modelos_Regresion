library(car)
library(rsm)
library(rgl)
library(scatterplot3d)
library(GGally)
ggplot2::theme_set(ggplot2::theme_bw())

#Cargando funciones de usuario desde repositorio en github 
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")
#Lectura de los datos desde archivo externo .csv 
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/datosproblemaseleccionpersonal.csv",header=TRUE,sep=";",dec=",")

#O bien, ingreso de datos po teclado
#COL1="punt.Aptitud",COL2="Prueba1",COL3="Prueba2",COL4="Prueba3",COL5="Prueba4"
datos=data.frame(scan(what=list(punt.Aptitud=0,Prueba1=0,Prueba2=0,Prueba3=0,Prueba4=0)))
94 122 121 96 89
71 108 115 98 78
82 120 115 95 90
76 118 117 93 95
111 113 112 109 109
64 112 96 90 88
109 109 129 102 108
104 112 119 106 105
80 115 101 95 88
73 111 95 95 84
127 119 118 107 110
88 112 110 100 87
99 120 89 105 97
80 117 118 99 100
99 109 125 108 95
116 116 122 116 102
100 104 83 100 102
96 110 101 103 103
126 117 120 113 108
58 120 77 80 74

attach(datos) #Permitir acceso a las variables del data.frame datos
names(datos) #Revisando el nombre de las variables guardadas en el data.frame

#Matriz de dispersión de alto nivel con histogramas en la diagonal
gg2<-ggpairs(datos,upper=list(continuous = wrap("smooth",alpha = 0.3, size=1.2,method = "lm")),lower=list(continuous ="cor"))
for(i in 1:ncol(datos)){
gg2[i,i]<-gg2[i,i]+
geom_histogram(breaks=hist(datos[,i],breaks = "FD",plot=F)$breaks,colour = "red",fill="lightgoldenrod1")
}
win.graph()
gg2

#O bien, matriz de dispersion de alto nivel con boxplots en la diagonal
win.graph()
ggpairs(datos,diag=list(continuous=wrap("box_no_facet",color="red",fill="lightgoldenrod1",alpha=0.3)),upper=list(continuous = wrap("smooth",alpha = 0.3, size=1.2,method = "lm")),lower=list(continuous ="cor"))

#Graficos de dispersion con plano ajustado de la respuesta vs. Prueba 1 y Prueba 2
#Grafico interactivo dinamico
scatter3d(punt.Aptitud~Prueba1+Prueba2,fov=60,revolutions=1)

#grafico ordinario. Ejecute juntas las siguientes dos lineas de programa. 
s3d.12=scatterplot3d(Prueba1,Prueba2,punt.Aptitud,cex.symbol=2,color=2,box=F,font.lab=4,font.axis=4)
s3d.12$plane3d(lm(punt.Aptitud~Prueba1+Prueba2), lty.box = "solid")

#Graficos de dispersion con plano ajustado de la respuesta vs. prueba 3 Prueba 4
#Grafico interactivo dinamico. Ver Figura 1.7
scatter3d(punt.Aptitud~Prueba3+Prueba4,fov=60,revolutions=1)

#Grafico ordinario. Ejecute juntas las siguientes dos lineas de programa. 
s3d.34=scatterplot3d(Prueba3,Prueba4,punt.Aptitud,cex.symbol=2,color=2,box=F,font.lab=4,font.axis=4)
s3d.34$plane3d(lm(punt.Aptitud~Prueba3+Prueba4), lty.box = "solid")

#Tabla de correlaciones por pares sin repetir caso
correlaciones(data=datos)
#Exportando al directorio de trabajo tabla con correlaciones de las variables en el data.frame datos 
write.csv2(correlaciones(data=datos),"tablacorrelaciones.csv",row.names=FALSE)

#Ajuste del MRLM: Formula extendida 
modelo=lm(punt.Aptitud~Prueba1+Prueba2+Prueba3+Prueba4)
#O bien de la siguiente forma,
modelo=lm(punt.Aptitud~.,datos)

#Desplegando en la Consola la tabla de parametros estimados
summary(modelo)
#Exportando al directorio de trabajo la tabla de parametros estimados, a un archivo de nombre tablaestimacionMRLM.csv
write.csv2(summary(modelo)$coefficients,"tablaestimacionMRLM.csv",row.names=TRUE)

#Anova del modelo
tablaAnova=MiAnova(model=modelo)
#Exportando al directorio de trabajo la tabla ANOVA del modelo, al archivo de nombre tablaANOVAMRLM.csv
write.csv2(tablaAnova,"tablaANOVAMRLM.csv",row.names=TRUE)

#Grafico de dispersion con ajuste lineal y loess de la respuesta observada vs. la estimada
win.graph()
scatterplot(fitted(modelo),punt.Aptitud,smooth=list(style="none",col.smooth=2),regLine=list(col=3),pch=19,cex=1.5,id=list(method="mahal", n=5, cex=1, col=carPalette()[-1]))
legend("topleft",legend=c(expression(paste("Ajuste lineal entre",sep=" ",Y,sep=", ",widehat(Y))),expression(paste("Ajuste loess entre",sep=" ",Y,sep=", ",widehat(Y)))),col=c(3,2),lty=c(1,2),lwd=2)

#Calculo del coeficiente de correlacion muestral entre la respueta estimada y la respuesta observada
cor(fitted(modelo),punt.Aptitud)

#graficos de variable agregada (added-variable plot) o graficos de regresion parcial (partial regression plot)
avPlots(modelo)

#graficos de residuos mas componente (residuals plus component plot)
crPlots(modelo)

detach(datos) #desanclando variables del data.frame



