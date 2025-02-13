library(car)

#cargando codigos de funciones de usuario
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

#Lectura de los datos
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/tablaDatosVentasPublicidad.csv",
                  header=TRUE,sep=";",dec=",",colClasses=c("factor","numeric","numeric"))

attach(datos)
str(datos)

#Cambiando nivel de referencia a nivel C
Seccion=relevel(Seccion,ref="C")
Seccion #observe que se informa que los niveles en su orden son C, A, B

#grafico de dispersion identificando secciones
win.graph(width=8,height=5)
scatterplot(Publicidad,Ventas,groups=Seccion,smooth=FALSE,regLine=FALSE,col=c(1,2,3),cex=1.5)

#Modelo general: Rectas son diferentes 
modelo1=lm(Ventas~Publicidad*Seccion)
summary(modelo1)
confint(modelo1)
MiAnova(modelo1)

#Grafico de dispersion con rectas ajustadas en cada Seccion
win.graph(width=8,height=5)
scatterplot(Publicidad,Ventas,groups=Seccion,smooth=FALSE,regLine=TRUE,col=c(1,2,3),cex=1.5)

#Graficos residuos externamente estudentizados en el modelo1
win.graph(width=10,height=5)
residualPlots(modelo1,type="rstudent",groups="Seccion",layout=c(1,3),linear=FALSE,tests=FALSE,cex=2,pch=1:3,col=1:3)

test=shapiro.test(rstudent(modelo1))
win.graph()
qqnorm(rstudent(modelo1),pch=as.numeric(Seccion),cex=1.5,col=as.numeric(Seccion))
qqline(rstudent(modelo1))
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test$statistic,test$p.value),digits=4)),cex=0.8)

#Probando hipotesis lineales en el modelo 1
names(coef(modelo1)) #Observe primero el nombre de los terminos en el modelo1
linearHypothesis(modelo1,"Publicidad:SeccionA=Publicidad:SeccionB")
linearHypothesis(modelo1,c("SeccionA=0","SeccionB=0","Publicidad:SeccionA=0","Publicidad:SeccionB=0"))
linearHypothesis(modelo1,c("Publicidad:SeccionA=0","Publicidad:SeccionB=0"))

#Obtener la matriz de diseno del modelo completo
matrizX.modelo1=as.data.frame(model.matrix(modelo1))
names(matrizX.modelo1) #Observe nombres R de las predictoras en modelo 1

#Modelo reducido en test "Publicidad:SeccionA=Publicidad:SeccionB", 
#o sea la igualdad de pendientes de las recta en secciones A y B
MR=lm(Ventas~Publicidad+SeccionA+SeccionB+Publicidad:I(SeccionA+SeccionB),data=matrizX.modelo1)
summary(MR)

#Modelo rectas paralelas (diferentes solo en el intercepto)
#No hay interaccion entre predictor cuantitativo y la Seccion
modelo2=lm(Ventas~Publicidad+Seccion)
summary(modelo2)
confint(modelo2)
MiAnova(modelo2)

#Graficos residuos externamente estudentizados en el  modelo2
win.graph(width=10,height=3.5)
residualPlots(modelo2,type="rstudent",groups="Seccion",layout=c(1,3),linear=FALSE,tests=FALSE,cex=2,pch=1:3,col=1:3)

test2=shapiro.test(rstudent(modelo2))
win.graph()
qqnorm(rstudent(modelo2),pch=as.numeric(Seccion),cex=1.5,col=as.numeric(Seccion))
qqline(rstudent(modelo2))
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test2$statistic,test2$p.value),digits=4)),cex=0.8)

#Grafico de dispersion con rectas ajustadas bajo modelo 2
win.graph()
plot(Publicidad,Ventas,pch=as.integer(Seccion),col=as.integer(Seccion),xlab="Publicidad (cientos de dolares)",
     ylab="Ventas (miles de dolares)",cex=1.5,cex.lab=1.0,panel.first=grid(lty=1))
for(i in 1:length(levels(Seccion))){
lines(Publicidad[Seccion==levels(Seccion)[i]],fitted(modelo2)[Seccion==levels(Seccion)[i]],col=i,lwd=2)
}
legend("bottomleft",legend=levels(Seccion),pch=c(1:length(levels(Seccion))),col=c(1:length(levels(Seccion))),
       horiz=TRUE,inset=c(0,1.02),title="Seccion",xpd=TRUE)

#Ajuste reduccion del modelo 1 eliminando interaccion X*I1  
modelo3=lm(Ventas~Publicidad+SeccionA+SeccionB+Publicidad:SeccionB,data=matrizX.modelo1)
summary(modelo3)
confint(modelo3)
MiAnova(modelo3)

#Graficos residuos externamente estudentizados en el  modelo 3
win.graph(width=10,height=3.5)
layout(cbind(c(1),c(2),c(3)))
plot(Publicidad,rstudent(modelo3),pch=as.integer(Seccion),col=as.integer(Seccion),
     cex=1.5,cex.lab=1.0,panel.first=grid(lty=1))
abline(h=c(-2,0,2),col=2,lty=2)
legend("bottomleft",legend=levels(Seccion),pch=c(1:length(levels(Seccion))),col=c(1:length(levels(Seccion))),
       horiz=TRUE,inset=c(0,1.02),title="Seccion",xpd=TRUE)
plot(rstudent(modelo3)~Seccion,border=1:3)
abline(h=c(-2,0,2),col=2,lty=2)
plot(fitted(modelo3),rstudent(modelo3),pch=as.integer(Seccion),col=as.integer(Seccion),
     xlab="Fitted values",cex=1.5,cex.lab=1.0,panel.first=grid(lty=1))
abline(h=c(-2,0,2),col=2,lty=2)
legend("bottomleft",legend=levels(Seccion),pch=c(1:length(levels(Seccion))),col=c(1:length(levels(Seccion))),
       horiz=TRUE,inset=c(0,1.02),title="Seccion",xpd=TRUE)

test3=shapiro.test(rstudent(modelo3))
win.graph()
qqnorm(rstudent(modelo3),pch=as.numeric(Seccion),cex=1.5,col=as.numeric(Seccion))
qqline(rstudent(modelo3))
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test3$statistic,test3$p.value),digits=4)),cex=0.8)

#Grafico de dispersion con rectas ajustadas bajo modelo 3
win.graph()
plot(Publicidad,Ventas,pch=as.integer(Seccion),col=as.integer(Seccion),xlab="Publicidad (cientos de dolares)",
     ylab="Ventas (miles de dolares)",cex=1.5,cex.lab=1.0,panel.first=grid(lty=1))
for(i in 1:length(levels(Seccion))){
lines(Publicidad[Seccion==levels(Seccion)[i]],fitted(modelo3)[Seccion==levels(Seccion)[i]],col=i,lwd=2)
}
legend("bottomleft",legend=levels(Seccion),pch=c(1:length(levels(Seccion))),col=c(1:length(levels(Seccion))),
       horiz=TRUE,inset=c(0,1.02),title="Seccion",xpd=TRUE)


detach(datos)
