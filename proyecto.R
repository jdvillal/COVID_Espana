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
                      date = datos$date,
                      confirmed = datos$confirmed,
                      icu = datos$icu,
                      deaths = datos$deaths,
                      vaccines = datos$vaccines,
                      month = as.yearmon(datos$date),
                      rigurosidad=cut(datos$stringency_index, breaks = c(0,20,40,60,80,100),
                        labels = c("De 0 a 2", "De 2 a 4", "De 4 a 6", "De 6 a 8","De 8 a 10")),
                      restriccionMobilidad=datos$gatherings_restrictions)
View(datosCOVID)


#*****************UNIVARIANTE***********************
# CUANTITATIVAS

#VACCINES
mean(na.omit(datosCOVID$vaccines))
var(na.omit(datosCOVID$vaccines))
sd(na.omit(datosCOVID$vaccines))

quantile(na.omit(datosCOVID$vaccines),0.25)
quantile(na.omit(datosCOVID$vaccines),0.5)
quantile(na.omit(datosCOVID$vaccines),0.75)
skewness(na.omit(datosCOVID$vaccines))
kurtosis(na.omit(datosCOVID$vaccines))

tb=fdt(datosCOVID$vaccines, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$vaccines), horizontal  = TRUE)
plot(datosCOVID$date, datosCOVID$vaccines, Type = "1",
     main = "Grafico de lineas - Vacunas", xlab = "Fecha", ylab = "# vacunados")

#CONFIRMADOS
mean(na.omit(datosCOVID$confirmed))
var(na.omit(datosCOVID$confirmed))
sd(na.omit(datosCOVID$confirmed))

quantile(na.omit(datosCOVID$confirmed),0.25)
quantile(na.omit(datosCOVID$confirmed),0.5)
quantile(na.omit(datosCOVID$confirmed),0.75)
skewness(na.omit(datosCOVID$confirmed))
kurtosis(na.omit(datosCOVID$confirmed))

tb=fdt(datosCOVID$confirmed, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$confirmed), horizontal  = TRUE)
plot(datosCOVID$date, datosCOVID$confirmed, Type = "1",
     main = "Grafico de lineas - Confirmados", xlab = "Fecha", ylab="Casos confirmados")

#RECUPERADOS
mean(na.omit(datosCOVID$icu))
var(na.omit(datosCOVID$icu))
sd(na.omit(datosCOVID$icu))

quantile(na.omit(datosCOVID$icu),0.25)
quantile(na.omit(datosCOVID$icu),0.5)
quantile(na.omit(datosCOVID$icu),0.75)
skewness(na.omit(datosCOVID$icu))
kurtosis(na.omit(datosCOVID$icu))

tb=fdt(datosCOVID$icu, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$icu), horizontal  = TRUE)
plot(datosCOVID$date, datosCOVID$icu, Type = "1",
     main = "Grafico de lineas - Recuperados", xlab = "Fecha", ylab = "Casos recuperados")

#MUERTOS
mean(na.omit(datosCOVID$deaths))
var(na.omit(datosCOVID$deaths))
sd(na.omit(datosCOVID$deaths))

quantile(na.omit(datosCOVID$deaths),0.25)
quantile(na.omit(datosCOVID$deaths),0.5)
quantile(na.omit(datosCOVID$deaths),0.75)
skewness(na.omit(datosCOVID$deaths))
kurtosis(na.omit(datosCOVID$deaths))

tb=fdt(datosCOVID$deaths, breaks="Sturges")
tb

boxplot(na.omit(datosCOVID$deaths), horizontal  = TRUE)
plot(datosCOVID$Date, datosCOVID$deaths, Type = "1",
     main = "Grafico de lineas - Muertes", xlab = "Fecha", ylab = "# muertes")

#CUALITATIVAS
barplot(table(datosCOVID$month), main = "Grafico de barras- Meses")
pie(table(datosCOVID$month))
barplot(table(datosCOVID$rigurosidad), main = "Grafico de barras- Rigurosidad")
pie(table(datosCOVID$rigurosidad))
barplot(table(datosCOVID$restriccionMobilidad), main = "Grafico de barras- Restriccion Mobilidad")
pie(table(datosCOVID$restriccionMobilidad))

#**********************BIVARIARIANTES*********************

#Dagrama de caja entre variable cuantitativa y cualitativa
boxplot(datosCOVID$confirmed~datosCOVID$month)
boxplot(datosCOVID$deaths~datosCOVID$month)
boxplot(datosCOVID$vaccines~datosCOVID$month)
boxplot(datosCOVID$icu~datosCOVID$month)

#Variables cuantitativa

cor(na.omit(datosCOVID[,c(3,4,5,6)]))
cov(na.omit(datosCOVID[,c(3,4,5,6)]))
data=cor(na.omit(datosCOVID[,c(3,4,5,6)]))
corrplot.mixed(data,lower = "circle",upper = "number")
plot(na.omit(datosCOVID[,(c(3,4,5,6))]))
