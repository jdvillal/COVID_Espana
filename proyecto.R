install.packages("COVID19")
library(COVID19)
install.packages("lubridate")
library(lubridate)
install.packages("moments")
library(moments)
install.packages("fdth")
library(fdth)
install.packages("zoo")
library(zoo)
install.packages("plotrix")
library(plotrix)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)

install.packages("rmarkdown")

#ESPAÑA
datos = covid19(country = "Spain")

datosCOVID = data.frame(datos = datos$id,
                      fecha = datos$date,
                      CasosConfirmados = datos$confirmed,
                      CasosCuidadosIntensivos = datos$icu,
                      CasosMuerte = datos$deaths,
                      Vacunados = datos$vaccines,
                      Mes = as.yearmon(datos$date),
                      RigurosidadRestricciones=cut(datos$stringency_index, breaks = c(0,20,40,60,80,100),
                        labels = c("De 0 a 2", "De 2 a 4", "De 4 a 6", "De 6 a 8","De 8 a 10")),
                      RestriccionMobilidad=datos$gatherings_restrictions)
View(datosCOVID)


#*****************UNIVARIANTE***********************
# CUANTITATIVAS

#VACCINES
mean(na.omit(datosCOVID$Vacunados))
var(na.omit(datosCOVID$Vacunados))
sd(na.omit(datosCOVID$Vacunados))

quantile(na.omit(datosCOVID$Vacunados),0.25)
quantile(na.omit(datosCOVID$Vacunados),0.5)
quantile(na.omit(datosCOVID$Vacunados),0.75)
skewness(na.omit(datosCOVID$Vacunados))
kurtosis(na.omit(datosCOVID$Vacunados))

tb=fdt(datosCOVID$Vacunados, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$Vacunados), horizontal  = TRUE)
plot(datosCOVID$fecha, datosCOVID$Vacunados, Type = "1",
     main = "Grafico de lineas - Vacunados", xlab = "Fecha", ylab = "# vacunados")

#CONFIRMADOS
mean(na.omit(datosCOVID$CasosConfirmados))
var(na.omit(datosCOVID$CasosConfirmados))
sd(na.omit(datosCOVID$CasosConfirmados))

quantile(na.omit(datosCOVID$CasosConfirmados),0.25)
quantile(na.omit(datosCOVID$CasosConfirmados),0.5)
quantile(na.omit(datosCOVID$CasosConfirmados),0.75)
skewness(na.omit(datosCOVID$CasosConfirmados))
kurtosis(na.omit(datosCOVID$CasosConfirmados))

tb=fdt(datosCOVID$CasosConfirmados, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$CasosConfirmados), horizontal  = TRUE)
plot(datosCOVID$fecha, datosCOVID$CasosConfirmados, Type = "1",
     main = "Grafico de lineas - Confirmados", xlab = "Fecha", ylab="Casos confirmados")

#RECUPERADOS
mean(na.omit(datosCOVID$CasosCuidadosIntensivos))
var(na.omit(datosCOVID$CasosCuidadosIntensivos))
sd(na.omit(datosCOVID$CasosCuidadosIntensivos))

quantile(na.omit(datosCOVID$CasosCuidadosIntensivos),0.25)
quantile(na.omit(datosCOVID$CasosCuidadosIntensivos),0.5)
quantile(na.omit(datosCOVID$CasosCuidadosIntensivos),0.75)
skewness(na.omit(datosCOVID$CasosCuidadosIntensivos))
kurtosis(na.omit(datosCOVID$CasosCuidadosIntensivos))

tb=fdt(datosCOVID$CasosCuidadosIntensivos, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$CasosCuidadosIntensivos), horizontal  = TRUE)
plot(datosCOVID$fecha, datosCOVID$CasosCuidadosIntensivos, Type = "1",
     main = "Grafico de lineas - Recuperados", xlab = "Fecha", ylab = "Casos recuperados")

#MUERTOS
mean(na.omit(datosCOVID$CasosMuerte))
var(na.omit(datosCOVID$CasosMuerte))
sd(na.omit(datosCOVID$CasosMuerte))

quantile(na.omit(datosCOVID$CasosMuerte),0.25)
quantile(na.omit(datosCOVID$CasosMuerte),0.5)
quantile(na.omit(datosCOVID$CasosMuerte),0.75)
skewness(na.omit(datosCOVID$CasosMuerte))
kurtosis(na.omit(datosCOVID$CasosMuerte))

tb=fdt(datosCOVID$CasosMuerte, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$CasosMuerte), horizontal  = TRUE)
plot(datosCOVID$Date, datosCOVID$CasosMuerte, Type = "1",
     main = "Grafico de lineas - Muertes", xlab = "Fecha", ylab = "# muertes")

#CUALITATIVAS
barplot(table(datosCOVID$Mes), main = "Grafico de barras- Meses")
pie(table(datosCOVID$Mes))
barplot(table(datosCOVID$RigurosidadRestricciones), main = "Grafico de barras- Rigurosidad")
pie(table(datosCOVID$RigurosidadRestricciones))
barplot(table(datosCOVID$RestriccionMobilidad), main = "Grafico de barras- Restriccion Mobilidad")
pie(table(datosCOVID$RestriccionMobilidad))

#**********************BIVARIARIANTES*********************

#Dagrama de caja entre variable cuantitativa y cualitativa
boxplot(datosCOVID$CasosConfirmados~datosCOVID$Mes)
boxplot(datosCOVID$CasosMuerte~datosCOVID$Mes)
boxplot(datosCOVID$Vacunados~datosCOVID$Mes)
boxplot(datosCOVID$CasosCuidadosIntensivos~datosCOVID$Mes)

#Variables cuantitativa

cor(na.omit(datosCOVID[,c(3,4,5,6)]))
cov(na.omit(datosCOVID[,c(3,4,5,6)]))
data=cor(na.omit(datosCOVID[,c(3,4,5,6)]))
corrplot.mixed(data,lower = "circle",upper = "number")
plot(na.omit(datosCOVID[,(c(3,4,5,6))]))
