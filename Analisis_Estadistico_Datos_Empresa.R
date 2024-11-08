getwd() 
setwd("E:/Only Authorized Personnel/U. Sergio/1ST YEAR/1ST QUARTER/1. STATISTICS/RSTUDIO/R_Analisis_Datos")
dir()
View("datos_grupo_12.txt")
datos<-read.table("datos_grupo_12.txt",header = TRUE) # Asignar variable en datos_grupo_12.txt
datos # Mostramos por consola
install.packages("openxlsx") # Si no tenemos instalado openxslx, instalamos el paquete, install.package("openxlsx"), ejecutamos paquete library(openxlsx).
library("openxlsx")
write.xlsx(datos, file = "datos.xlsx", rowNames=TRUE,colNames=TRUE) # exportamos archivo de formato "txt" a una hoja de dato.

# 3 EXPLORAR DATOS
dim(datos) # Para conocer el nº de observaciones y el nº de variables de la BBDD.
names(datos) # Para conocer los nombres de las variables, donde vemos que son "Tipo.de.empresa", "Número.de.Servicios" "Tiempo", "Facturado".
str(datos) 
summary(datos) # Con esto veremos un resumen de la medida de cada una de las variables
attach(datos) # Desglosamos las variables.

#FUNCIONES:

medidas.Disp<-function(nombre_tabla,decimales=2){
  resultado<-try({
    media<-round(mean(nombre_tabla),digits = decimales)
    desviacion<-round(sd(nombre_tabla),digits = decimales) 
    varianza<-round(var(nombre_tabla),digits = decimales)
    coeficiente.vr<-round((desviacion/media)*100,digits = decimales)
    
    tabla<-data.frame(max(nombre_tabla)-min(nombre_tabla),median(nombre_tabla),media,varianza,desviacion,coeficiente.vr)
    names(tabla)<-c("Rango","Mediana","Media","Varianza","Desviación estadar","Coeficiente de variación")
    return(tabla)
  },silent = FALSE)
  if(inherits(resultado,"try-error")){
    cat("Ocurre un error verificar: \n")
  }else{
    cat(resultado)
  }
} 
tf.discretos<-function(nombre_tabla,decimales=2){
  resultado<-try({
    tabla<-table(nombre_tabla)
    tablaf<-as.data.frame(tabla)
    tabla.frecuencias<-transform(tablaf,
                                 FreqAc=cumsum(tablaf$Freq),
                                 Relat=round(prop.table(tablaf$Freq),digits = decimales),
                                 RelatAc=round(cumsum(prop.table(tablaf$Freq)),digits = decimales)
    )
    return(tabla.frecuencias)
  },silent = FALSE)
  if(inherits(resultado,"try-error")){
    cat("Ocurre un error verificar \n")
  }else{
    return(resultado)
  }
  
}
tf.continuos<-function(nombre_tabla,observ,decimales=2){
  resultado<-try(
    {
      n.clases<-ceiling(1+3.222*log10(observ))
      anch.inter<-round((max(nombre_tabla)-min(nombre_tabla))/n.clases,digits = decimales)
      limites<-min(nombre_tabla)+c(0:n.clases)*anch.inter
      cortes<-cut(nombre_tabla,breaks =limites,include.lowest = TRUE,dig.lab = 3+decimales)
      tabla<-table(cortes)
      
      marca.clases<-numeric(n.clases)
      for(i in 1:n.clases){
        resultados<-round((2*limites[i]+anch.inter)/2,digits = 2)
        marca.clases[i]<-resultados
      }
      tf<-as.data.frame(tabla)
      tfCompl<-transform(tf,
                         Xi=marca.clases,
                         FreqAc=cumsum(tf$Freq),
                         Relat=round(prop.table(tf$Freq),digits = decimales),
                         RelatAc=round(cumsum(prop.table(tf$Freq)),digits = decimales)
      )
      #rm(anch.inter,limites,cortes,n.clases,marca.clases,i)
      return(tfCompl)
    },silent=FALSE)
  if(inherits(resultado,"try-error")){
    cat("Ocurrio un error, vuelve a verificar: \n")
  }
  cat(resultado)
  
}

# 4 Análisis Descriptivo Univariante

# Variable cualitativa:

#------------------------------------
# 1. Tipo de empresa
tb.TipoEmp<-tf.discretos(Tipo.de.empresa) # Frecuencia relativa, donde el 36...% aproximadamente son empresas "grandes" y el resto (63..%) son empresas "pyme".
tb.TipoEmp
write.xlsx(tb.TipoEmp,file = "tb.TipoEmp.xlsx")

# Gráfico
par(mfrow=c(1,2)) # Configuramos parámetros del gráfico
barplot(tabla.tipo.empresa,main="Tipo de empresa", col = "lightblue",ylab = "Frecuencias absolutas", xlab = "Empresa")
pie(tabla.tipo.empresa,main="Tipo de empresa",ylab = "Frecuencias absolutas", xlab = "Empresa") # Diagrama de circulo

# Variables cuantitativas:

#------------------------------------
# 1. Numero de servicios

# Tabla de Frecuencias:
tb.NumSer<-tf.discretos(Número.de.Servicios)
tb.NumSer
write.xlsx(tb.NumSer,file = "tb.NumSer.xlsx")

# Medidas de dispersión:
md.NumSer<-medidas.Disp(Número.de.Servicios)
cat("Medidas de dispesión de Número de Servicios: \n")
md.NumSer

# Distribución, veremos como se estan distribuyendo los datos
install.packages("moments") # Instalar el paquete moments si no lo tienes
library(moments) # Cargar el paquete
skewness(Número.de.Servicios) # 0.01985621 es >= 0, lo que significa que es practicamente simetrica, lo veremos en un grafico para aclarar más.
plot(density(Número.de.Servicios), main = "Densidad del Número de Servicios") # Curva de densidad

# Gráficos
hist(Número.de.Servicios, breaks = "Sturges", freq = TRUE,main="Histograma de Nº de servicios", xlab = "Nº de servicios", ylab = "Frecuencias Absolutas")
hist(Número.de.Servicios, breaks = "Sturges", freq = FALSE,main="Histograma de Nº de servicios", xlab = "Nº de servicios", ylab = "Frecuencias Relativas")
boxplot(Número.de.Servicios,main="Diagrama de Nº de servicios",ylab="Valores",col = "lightblue")# Resaltaremos los cuartiles de la variable, así como la presencia de valores atípicos (outliers).

#------------------------------------
# 2. Tiempo

# Tabla de Frecuencias
tb.Tiempo<-tf.continuos(Tiempo,207)
tb.Tiempo
write.xlsx(tb.Tiempo,file = "tb.Tiempo.xlsx")

# Medidas de dispersión:
md.Tiempo<-medidas.Disp(Tiempo)
cat("Medidas de dispesión del tiempo: \n")
md.Tiempo

# Distribución
skewness(Tiempo) # 0.01360188, se aproxima a una distribución simétrica.
plot(density(Tiempo),main="Tiempo")

# Gráficos
hist(Tiempo, breaks = "Sturges", freq = TRUE,main="Histograma de tiempo", xlab = "Tiempo", ylab = "Frecuencias Absolutas")
hist(Tiempo, breaks = "Sturges", freq = FALSE,main="Histograma de tiempo", xlab = "Tiempo", ylab = "Frecuencias Relativas")
boxplot(Tiempo ,main="Diagrama de tiempo",ylab="Valores",col = "purple")

#------------------------------------
# 3. Facturado

# Tabla de Frecuencias
tb.Factur<-tf.continuos(Facturado,207)
tb.Factur
write.xlsx(tb.Factur,file = "tb.Factur.xlsx") # Importar a excel

# Medidas de dispersión:
md.Facturado<-medidas.Disp(Facturado)
cat("Medidas de dispesión de Facturado: \n")
md.Facturado

# Distribución
skewness(Facturado) # 0.1519434 (Simétrica).
plot(density(Facturado))

# Gráficos
hist(Facturado, breaks = "Sturges", freq = TRUE,main="Histograma de Facturado", xlab = "Facturado", ylab = "Frecuencias Absolutas")
boxplot(Facturado ,main="Diagrama de Facturado",ylab="Valores",col = "pink")
par(mfrow=c(1,1)) # Volvemos a la configuración

# 4 Análisis de la Variables Tiempo

boxplot(Tiempo ~ Tipo.de.empresa, data=datos, main="Tiempo por Tipo de Empresa", ylab="Tiempo (meses)", xlab = "Tipo de Empresa", col = c("lightblue", "lightgreen")) # Boxplot del Tiempo según el Tipo de empresa
boxplot(Tiempo ~ Número.de.Servicios, data=datos, main="Tiempo por Número de Servicios", ylab="Tiempo (meses)", xlab="Número de Servicios", col="lightblue") # Boxplot del Tiempo por Número de Servicios

media.tiempo.empresa <- aggregate(Tiempo ~ Tipo.de.empresa, data=datos, FUN=mean)
barplot(media.tiempo.empresa$Tiempo,  # Gráfico de barras: Media del Tiempo según Tipo de Empresa
        names.arg = media.tiempo.empresa$Tipo.de.empresa, 
        col = c("lightblue", "lightgreen"), 
        main = "Media del Tiempo por Tipo de Empresa", 
        xlab = "Tipo de Empresa", 
        ylab = "Media del Tiempo (meses)")

media.tiempo.servicios <- aggregate(Tiempo ~ Número.de.Servicios, data=datos, FUN=mean) # Gráfico de barras para la Media del Tiempo por Número de Servicios
barplot(media.tiempo.servicios$Tiempo, 
        names.arg = media.tiempo.servicios$Número.de.Servicios, 
        col = "lightblue", 
        main = "Tiempo Promedio por Servicios Contratados", 
        xlab = "Número de Servicios", 
        ylab = "Tiempo Promedio (meses)")

# Calcular la correlación entre entre el tiempo, tipo de servicio, numero de servicio.:
tipoEmpr.num<-as.numeric(factor(Tipo.de.empresa))
cor.Tiem.tiEmpr <- cor(Tiempo,tipoEmpr.num,use = "complete.obs",method = "spearman") # x, y deben ser numericas!!
cor.Tiem.numSer <- cor(Tiempo,Número.de.Servicios,use = "complete.obs",method = "spearman")

# Diagrama de dispersión para visualizar la relación entre el tiempo, tipo de servicio, numero de servicio.
plot(tipoEmpr.num, Tiempo, main = "Diagrama de Dispersión: Tiempo vs Tipo de Empresa", xlab="Tipo de empresa",col = "lightblue", pch = 19)
abline(lm(Tiempo ~ tipoEmpr.num, data=datos), col="red") # Añadir línea de tendencia al gráfico de dispersión

plot(Número.de.Servicios, Tiempo, main = "Diagrama de Dispersión: Tiempo vs Número de Servicios",xlab="Número de servicios", col = "lightblue", pch = 19)
abline(lm(Tiempo ~ Número.de.Servicios, data=datos), col="red") # Añadir línea de tendencia al gráfico de dispersión

#  Análisis de la Variable Facturado

boxplot(Facturado ~ Tipo.de.empresa, data = datos, main="Facturado por tipo de empresa",ylab = "Facturado",xlab = "Tipo de empresa",col="lightblue")
boxplot(Facturado ~ Número.de.Servicios, data = datos, main="Facturado por Número de Servicios",ylab = "Facturado",xlab = "Número de servicio",col="lightblue")

media.fact.tipoEmp<-aggregate(Facturado ~ Tipo.de.empresa,FUN = mean)
barplot(media.fact.tipoEmp$Facturado,names.arg = media.fact.tipoEmp$Tipo.de.empresa,main="Facturado Promedio por Tipo de servicio",ylab = "Facturado",xlab ="Número de servicios", col="lightblue")

media.fact.numSer<-aggregate(Facturado ~ Número.de.Servicios,FUN = mean)
barplot(media.fact.numSer$Facturado,names.arg = media.fact.numSer$Número.de.Servicios, main = "Facturado Promedio por Número de servicios", ylab = "Facturado",xlab ="Número de servicios", col="lightblue")

# 5. Calcular la correlación entre el facturado, tipo de servicio, numero de servicio:

cor.Fact.tiEmpr <- cor(x = Facturado,y=tipoEmpr.num,use = "complete.obs",method = "spearman") #  -0.4100544
cor.Fact.NumServ <- cor(x = Facturado,y=Número.de.Servicios,use = "complete.obs",method = "spearman") # 0.7041209

plot(tipoEmpr.num,Facturado,main = "Diagrama de dispersión: Facturado vs Tipo de Empresa", xlab="Tipo de empresa",col = "lightblue", pch = 19)
abline(lm(Facturado ~ tipoEmpr.num, data=datos),col="red")

plot(Número.de.Servicios,Facturado,main = "Diagrama de dispersión: Facturado vs Número de servicios", xlab="Número de servicios",col = "lightblue", pch = 19)
abline(lm(Facturado ~ Número.de.Servicios, data=datos),col="red")

# 6. Relación Bivariante entre Tiempo y Facturado

plot(Tiempo, Facturado,main = "Diagrama de dispersión entre tiempo y facturado", col="blue")
abline(lm(Facturado~Tiempo, data = datos), col="red")
cor(Facturado,Tiempo,use="complete.obs",method = "spearman") # 0.8987834
reg.Fact.Tiem<-lm(Facturado~Tiempo,data = datos)
summary(reg.Fact.Tiem)

# 7. Análisis con Distinción de Tipo de empresa# 7. Análisis con Distinción de Tipo de empresa
library(ggplot2)
# ggplot(datos, aes(x=Tipo.de.empresa, y =Facturado, fill=Tipo.de.empresa))+
#  geom_boxplot() + 
#   labs(title="Distribución de Facturado por Tipo de empresa",
#       x = "Tipo de empresa",
#       y = "Facturado")+
#  theme_light()

# ggplot(datos,aes(x=Tipo.de.empresa, y=Tiempo, fill=Tipo.de.empresa))+
#  geom_boxplot() +
#  labs(title="Distribución de Tiempo por Tipo de empresa",
#       x = "Tipo de empresa",
#       y = "Tiempo")+
#  theme_light()

