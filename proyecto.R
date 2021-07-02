install.packages("COVID19")
library(COVID19)
install.packages("lubridate")
library(lubridate)
install.packages("moments")
library(moments)

#ESPAÑA
datos = covid19(country = "Spain")
datosCOVID = data.frame(datos,month(datos$date))
View(datosCOVID)

#UNIVARIANTE - CUANTITATIVAS

#VACCINES

mean(na.omit(datosCOVID$vaccines))
var(na.omit(datosCOVID$vaccines))
sd(na.omit(datosCOVID$vaccines))

quantile(na.omit(datosCOVID$vaccines),0.25)
quantile(na.omit(datosCOVID$vaccines),0.5)
quantile(na.omit(datosCOVID$vaccines),0.75)
skewness(na.omit(datosCOVID$vaccines))
kurtosis(na.omit(datosCOVID$vaccines))

boxplot(na.omit(datosCOVID$vaccines), horizontal  = TRUE)
plot(datosCOVID$vaccines, Type = "1")

#CONFIRMADOS
mean(na.omit(datosCOVID$confirmed))
var(na.omit(datosCOVID$confirmed))
sd(na.omit(datosCOVID$confirmed))

quantile(na.omit(datosCOVID$confirmed),0.25)
quantile(na.omit(datosCOVID$confirmed),0.5)
quantile(na.omit(datosCOVID$confirmed),0.75)
skewness(na.omit(datosCOVID$confirmed))
kurtosis(na.omit(datosCOVID$confirmed))

boxplot(na.omit(datosCOVID$confirmed), horizontal  = TRUE)
plot(datosCOVID$confirmed, Type = "1")

#RECUPERADOS
mean(na.omit(datosCOVID$recovered))
var(na.omit(datosCOVID$recovered))
sd(na.omit(datosCOVID$recovered))

quantile(na.omit(datosCOVID$recovered),0.25)
quantile(na.omit(datosCOVID$recovered),0.5)
quantile(na.omit(datosCOVID$recovered),0.75)
skewness(na.omit(datosCOVID$recovered))
kurtosis(na.omit(datosCOVID$recovered))

boxplot(na.omit(datosCOVID$recovered), horizontal  = TRUE)
plot(datosCOVID$recovered, Type = "1")

#MUERTOS
mean(na.omit(datosCOVID$deaths))
var(na.omit(datosCOVID$deaths))
sd(na.omit(datosCOVID$deaths))

quantile(na.omit(datosCOVID$deaths),0.25)
quantile(na.omit(datosCOVID$deaths),0.5)
quantile(na.omit(datosCOVID$deaths),0.75)
skewness(na.omit(datosCOVID$deaths))
kurtosis(na.omit(datosCOVID$deaths))

boxplot(na.omit(datosCOVID$deaths), horizontal  = TRUE)
plot(datosCOVID$deaths, Type = "1")

#CUALITATIVAS
