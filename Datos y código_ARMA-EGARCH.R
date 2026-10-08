##########################################################################
# UNIVERSIDAD NACIONAL DE INGENIERÍA 
# INGENIERÍA EN ECONOMÍA Y NEGOCIOS 
# ECONOMETRÍA AVANZADA 
# ARMA-GARCH Comparativo  
#########################################################################
# INTEGRANTES: 
# David Vanegas 
# Franklin González 
# Dereck Real 
#########################################################################

#########################################################################

#### Librerías ####
library(quantmod) #Para revisar bases de datos
library(rugarch) #Para los modelos garch 
library(ggplot2) #Para los gráficos
library(gridExtra) #Para mejora de gráficos
library(tseries) #Para tests de series de tiempo
library(FinTS) #Para tests de series de tiempo 
library(forecast)
library(moments)

### Datos e información a utilizar ###

getSymbols("MU", src = "yahoo", from="2020-01-01", to="2026-06-05") #Micron Technology
getSymbols("NVDA", src = "yahoo", from="2020-01-01", to="2026-06-05") #NVIDIA
getSymbols("MSFT", src = "yahoo", from="2020-01-01", to="2026-06-05") #Microsoft
#Siempre se trabaja con el valor de los datos al cierre

###Extraer datos a usar (precios al cierre)###
#Datos de Micron Technology
PreMU <- Cl(MU)
roilogMU <- na.omit(diff(log(PreMU)))

#Datos de NVIDIA
PreNVDA <- Cl(NVDA)
roilogNVDA <- na.omit(diff(log(PreNVDA)))

#Datos de Microsoft 
PreMSFT <- Cl(MSFT)
roilogMSFT <- na.omit(diff(log(PreMSFT)))

###Transformación de las bases de datos para gráficar###
#Datos de Micron Technology
PrecioMU <- data.frame(Fecha=index(PreMU), Precio=as.numeric(PreMU))
RendimientosMU <- data.frame(Fecha=index(roilogMU), Rendimiento=as.numeric(roilogMU))

#Datos de NVIDIA
PrecioNVDA <- data.frame(Fecha=index(PreNVDA), Precio=as.numeric(PreNVDA))
RendimientosNVDA <- data.frame(Fecha=index(roilogNVDA), Rendimiento=as.numeric(roilogNVDA))

#Datos de Microsoft 
PrecioMSFT <- data.frame(Fecha=index(PreMSFT), Precio=as.numeric(PreMSFT))
RendimientosMSFT <- data.frame(Fecha=index(roilogMSFT), Rendimiento=as.numeric(roilogMSFT))

###Datos y gráficas conjuntas de precios###
PrecioTotal <- rbind(
  data.frame(
    Fecha = index(PreMU),
    Valor = as.numeric(PreMU),
    Empresa = "Micron Technology"),
  data.frame(
    Fecha = index(PreNVDA),
    Valor = as.numeric(PreNVDA),
    Empresa = "NVIDIA"),
  data.frame(
    Fecha = index(PreMSFT),
    Valor = as.numeric(PreMSFT),
    Empresa = "Microsoft"))

GPrecios <- ggplot(
  PrecioTotal,
  aes(x = Fecha,
      y = Valor,
      color = Empresa)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Precios de Cierre de las Empresas Analizadas",
    x = "Fecha",
    y = "Precio (USD)",
    color = "Empresa") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5))

GPrecios

###Gráficos de rendimientos###
GrendimientosMU <- ggplot(RendimientosMU, aes(x= Fecha, y= Rendimiento))+
  geom_line(color= "#4A4", linewidth = 0.8)+
  geom_hline(yintercept = 0, linetype= "dashed", alpha=0.5)+
  labs(title= "Rendimientos diarios Micron Technology", subtitle = "Evidencia de volatilidad", x= "Fecha", y= "Retorno")+
  theme_minimal()

GrendimientosMU

GrendimientosNVDA <- ggplot(RendimientosNVDA, aes(x= Fecha, y= Rendimiento))+
  geom_line(color= "#4A4", linewidth = 0.8)+
  geom_hline(yintercept = 0, linetype= "dashed", alpha=0.5)+
  labs(title= "Rendimientos diarios NVIDIA", subtitle = "Evidencia de volatilidad", x= "Fecha", y= "Retorno")+
  theme_minimal()

GrendimientosNVDA

GrendimientosMSFT <- ggplot(RendimientosMSFT, aes(x= Fecha, y= Rendimiento))+
  geom_line(color= "#4A4", linewidth = 0.8)+
  geom_hline(yintercept = 0, linetype= "dashed", alpha=0.5)+
  labs(title= "Rendimientos diarios Microsoft", subtitle = "Evidencia de volatilidad", x= "Fecha", y= "Retorno")+
  theme_minimal()

GrendimientosMSFT

###Estadísticos descriptivos###
##Estadísticos de Micron Technology##
rendMU <- na.omit(RendimientosMU$Rendimiento)
estadisticosMU <- data.frame(
  Estadistico = c("Media", "Mediana", "Desv. Estándar", "Mínimo",
                  "Máximo", "Asimetría", "Curtosis"), 
  Valor = c(mean(rendMU),
          median(rendMU),
          sd(rendMU),
          min(rendMU),
          max(rendMU),
          skewness(rendMU),
          kurtosis(rendMU)))

estadisticosMU

##Histograma de Micron Technology##
ggplot(data.frame(rendMU), aes(x = rendMU)) +
  geom_histogram(bins = 30,
                 fill = "steelblue",
                 color = "black") +
  labs(title = "Histograma de Rendimientos",
       x = "Rendimiento",
       y = "Frecuencia") +
  theme_minimal()

##Estadísticos de NVIDIA##
rendNVDA <- na.omit(RendimientosNVDA$Rendimiento)
estadisticosNVDA <- data.frame(
  Estadistico = c("Media", "Mediana", "Desv. Estándar", "Mínimo",
                  "Máximo", "Asimetría", "Curtosis"), 
  Valor = c(mean(rendNVDA),
            median(rendNVDA),
            sd(rendNVDA),
            min(rendNVDA),
            max(rendNVDA),
            skewness(rendNVDA),
            kurtosis(rendNVDA)))

estadisticosNVDA

##Histograma de NVIDIA##
ggplot(data.frame(rendNVDA), aes(x = rendNVDA)) +
  geom_histogram(bins = 30,
                 fill = "steelblue",
                 color = "black") +
  labs(title = "Histograma de Rendimientos",
       x = "Rendimiento",
       y = "Frecuencia") +
  theme_minimal()

##Estadísticos de Microsoft## 
rendMSFT <- na.omit(RendimientosMSFT$Rendimiento)
estadisticosMSFT <- data.frame(
  Estadistico = c("Media", "Mediana", "Desv. Estándar", "Mínimo",
                  "Máximo", "Asimetría", "Curtosis"), 
  Valor = c(mean(rendMSFT),
            median(rendMSFT),
            sd(rendMSFT),
            min(rendMSFT),
            max(rendMSFT),
            skewness(rendMSFT),
            kurtosis(rendMSFT)))

estadisticosMSFT

##Histograma de Microsoft## 
ggplot(data.frame(rendMSFT), aes(x = rendMSFT)) +
  geom_histogram(bins = 30,
                 fill = "steelblue",
                 color = "black") +
  labs(title = "Histograma de Rendimientos",
       x = "Rendimiento",
       y = "Frecuencia") +
  theme_minimal()

###Pruebas estadísticas sobre los rendimientos###
##Micron Technology##
jarque.bera.test(rendMU) #Normalidad 
adf.test(rendMU) #Estacionariedad 
pp.test(rendMU) #Estacionariedad 
Box.test(rendMU) #Autocorrelación 
ArchTest(rendMU, lags = 12) #Heterocedasticidad condicional

##NVIDIA##
jarque.bera.test(rendNVDA) #Normalidad 
adf.test(rendNVDA) #Estacionariedad 
pp.test(rendNVDA) #Estacionariedad 
Box.test(rendNVDA) #Autocorrelación 
ArchTest(rendNVDA, lags = 12) #Heterocedasticidad condicional

##Microsoft##
jarque.bera.test(rendMSFT) #Normalidad 
adf.test(rendMSFT) #Estacionariedad 
pp.test(rendMSFT) #Estacionariedad 
Box.test(rendMSFT) #Autocorrelación 
ArchTest(rendMSFT, lags = 12) #Heterocedasticidad condicional

###Gráficos ACF y PACF de los rendimientos###
##Micron Technology##
pacf(rendMU, 
     main = "Función de autocorrelación parcial (PACF)", 
     xlab = "Rezago", 
     ylab = "PACF", 
     lwd = 3)
grid()

acf(rendMU,
    main="Función de autocorrelación",
    xlab="Rezago",
    ylab="acf",
    lwd=3)
grid()
#Probar los siguientes ordenes de ARMA, aunque la evidencia
#hasta el momento sugiere un ARMA(0,0)
#arma00 <- arima(rendMU, order = c(0,0,0))
#arma10 <- arima(rendMU, order = c(1,0,0))
#arma01 <- arima(rendMU, order = c(0,0,1))
#arma11 <- arima(rendMU, order = c(1,0,1))

##NVIDIA##
pacf(rendNVDA, 
     main = "Función de autocorrelación parcial (PACF)", 
     xlab = "Rezago", 
     ylab = "PACF", 
     lwd = 3)
grid()

acf(rendNVDA,
    main="Función de autocorrelación",
    xlab="Rezago",
    ylab="acf",
    lwd=3)
grid()
#Probar los siguientes ordenes de ARMA, aunque la evidencia
#hasta el momento sugiere un ARMA(2,1)
#arma00 <- arima(rendNVDA, order = c(0,0,0))
#arma10 <- arima(rendNVDA, order = c(1,0,0))
#arma01 <- arima(rendNVDA, order = c(0,0,1))
#arma11 <- arima(rendNVDA, order = c(1,0,1))
#arma20 <- arima(rendNVDA, order = c(2,0,0))
#arma02 <- arima(rendNVDA, order = c(0,0,2))
#arma21 <- arima(rendNVDA, order = c(2,0,1))
#arma12 <- arima(rendNVDA, order = c(1,0,2))

##Microsoft##
pacf(rendMSFT, 
     main = "Función de autocorrelación parcial (PACF)", 
     xlab = "Rezago", 
     ylab = "PACF", 
     lwd = 3)
grid()

acf(rendMSFT,
    main="Función de autocorrelación",
    xlab="Rezago",
    ylab="acf",
    lwd=3)
grid()
#Probar los siguientes ordenes de ARMA, aunque la evidencia
#hasta el momento sugiere un ARMA(1,0)
#arma00 <- arima(rend, order = c(0,0,0))
#arma10 <- arima(rend, order = c(1,0,0))
#arma01 <- arima(rend, order = c(0,0,1))
#arma11 <- arima(rend, order = c(1,0,1))

###Estimación y comparación de modelos ARMA###
##Micron Technology##
#Estimación de modelos#
arma00MU <- arima(rendMU, order = c(0,0,0))
arma10MU <- arima(rendMU, order = c(1,0,0))
arma01MU <- arima(rendMU, order = c(0,0,1))
arma11MU <- arima(rendMU, order = c(1,0,1))

#Diagnóstico de modelos#
summary(arma00MU)
autoplot(arma00MU)
checkresiduals(arma00MU)

summary(arma10MU)
autoplot(arma10MU)
checkresiduals(arma10MU)

summary(arma01MU)
autoplot(arma01MU)
checkresiduals(arma01MU)

summary(arma11MU)
autoplot(arma11MU)
checkresiduals(arma11MU)

#Dado que la dinámica de la media es débil y persisten evidencias de
#heterocedasticidad condicional, se optó por una especificación parsimoniosa 
#para la ecuación de media, es decir, un ARMA(0,0). 

##NVIDIA##
#Estimación de modelos#
arma00NVDA <- arima(rendNVDA, order = c(0,0,0))
arma10NVDA <- arima(rendNVDA, order = c(1,0,0))
arma01NVDA <- arima(rendNVDA, order = c(0,0,1))
arma11NVDA <- arima(rendNVDA, order = c(1,0,1))
arma20NVDA <- arima(rendNVDA, order = c(2,0,0))
arma02NVDA <- arima(rendNVDA, order = c(0,0,2))
arma21NVDA <- arima(rendNVDA, order = c(2,0,1))
arma12NVDA <- arima(rendNVDA, order = c(1,0,2))

#Diagnóstico de modelos#
summary(arma00NVDA)
autoplot(arma00NVDA)
checkresiduals(arma00NVDA)

summary(arma10NVDA)
autoplot(arma10NVDA)
checkresiduals(arma10NVDA)

summary(arma01NVDA)
autoplot(arma01NVDA)
checkresiduals(arma01NVDA)

summary(arma11NVDA)
autoplot(arma11NVDA)
checkresiduals(arma11NVDA)

summary(arma20NVDA)
autoplot(arma20NVDA)
checkresiduals(arma20NVDA)

summary(arma02NVDA)
autoplot(arma02NVDA)
checkresiduals(arma02NVDA)

summary(arma21NVDA)
autoplot(arma21NVDA)
checkresiduals(arma21NVDA)

summary(arma12NVDA)
autoplot(arma12NVDA)
checkresiduals(arma12NVDA)
#Dado que tiene el mejor AIC, sus parámetros son significativos, 
#tiene el mejor diagnóstico de Ljungbox y es consistente con los 
#PACF y ACF, se elige el modelo ARMA(1,1) como ecuación de media. 

##Microsoft## 
#Estimación de modelos#
arma00MSFT <- arima(rendMSFT, order = c(0,0,0))
arma10MSFT <- arima(rendMSFT, order = c(1,0,0))
arma01MSFT <- arima(rendMSFT, order = c(0,0,1))
arma11MSFT <- arima(rendMSFT, order = c(1,0,1))

#Diagnóstico de modelos#
summary(arma00MSFT)
autoplot(arma00MSFT)
checkresiduals(arma00MSFT)

summary(arma10MSFT)
autoplot(arma10MSFT)
checkresiduals(arma10MSFT)

summary(arma01MSFT)
autoplot(arma01MSFT)
checkresiduals(arma01MSFT)

summary(arma11MSFT)
autoplot(arma11MSFT)
checkresiduals(arma11MSFT)
#Dado que presenta el mejor AIC entre los modelos candidatos, 
#su parámetro autorregresivo es estadísticamente significativo, 
#Es más parsimonioso que el ARMA(1,1), y reduce la autocorrelación
#residual respecto al modelo nulo, se elige el modelo ARMA(1,0) como 
#ecuación de media. 

###Pruebas ARCH en los residuos de los modelos elegidos###
ArchTest(residuals(arma00MU), lags = 12) #Micron Technology 
ArchTest(residuals(arma11NVDA), lags = 12) #NVIDIA
ArchTest(residuals(arma10MSFT), lags = 12) #Microsoft 

#Los residuos de cada uno de los modelos elegidos presentan 
#heterocedasticidad condicional, por lo que se justifica el uso
#de modelos tipo GARCH para modelar su volatilidad y riesgo. 

###Modelos GARCH para los rendimientos###
##Modelos para Micron Technology##
#Garch estándar 
specgarchMU <- ugarchspec(
  variance.model = list(model= "sGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(0,0), include.mean= TRUE), 
  distribution.model = "std")

GARCHMU <- ugarchfit(spec = specgarchMU, data = rendMU)
print(GARCHMU)

#E-GARCH
specegarchMU <- ugarchspec(
  variance.model = list(model= "eGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(0,0), include.mean= TRUE), 
  distribution.model = "std")

EGARCHMU <- ugarchfit(spec = specegarchMU, data = rendMU)
print(EGARCHMU)

#GJR-GARCH
specgjrgarchMU <- ugarchspec(
  variance.model = list(model= "gjrGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(0,0), include.mean= TRUE), 
  distribution.model = "std")

GJRGARCHMU <- ugarchfit(spec = specgjrgarchMU, data = rendMU)
print(GJRGARCHMU)

#Dado que tiene un menor AIC, tiene un mayor log-likelihood, 
#captura asimetrías significativas, y mantiene residuos sin 
#autocorrelación o efectos ARCH remanentes, el EGARCH es el 
#mejor modelo entre los 3. Además, interpretándolo, es claro 
#que la volatilidad actual depende casi exclusivamente de la 
#volatilidad pasada (beta) y de la asimetría (gamma), no 
#tanto del tamaño del shock. 

##Modelos para NVIDIA##
#Garch estándar 
specgarchNVDA <- ugarchspec(
  variance.model = list(model= "sGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(1,1), include.mean= TRUE), 
  distribution.model = "std")

GARCHNVDA <- ugarchfit(spec = specgarchNVDA, data = rendNVDA)
print(GARCHNVDA)

#E-GARCH
specegarchNVDA <- ugarchspec(
  variance.model = list(model= "eGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(1,1), include.mean= TRUE), 
  distribution.model = "std")

EGARCHNVDA <- ugarchfit(spec = specegarchNVDA, data = rendNVDA)
print(EGARCHNVDA)

#GJR-GARCH
specgjrgarchNVDA <- ugarchspec(
  variance.model = list(model= "gjrGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(1,1), include.mean= TRUE), 
  distribution.model = "std")

GJRGARCHNVDA <- ugarchfit(spec = specgjrgarchNVDA, data = rendNVDA)
print(GJRGARCHNVDA)

#Dado que tiene menor AIC, mayor log-likelihood, todos sus 
#parámetros son significativos, su parámetro de asímetría (gamma)
#es altamente significativo, elimina completamente la autocorrelación
#y heterocedasticidad remanente, presenta mayor estabilidad paramétrica 
#que el sgarch y el gjrgarch, y es consistente con la teoría financiera 
#para acciones de alta volatilidad como NVIDIA, se elige el modelo 
#EGARCH. 

##Modelos para Microsoft##
#Garch estándar 
specgarchMSFT <- ugarchspec(
  variance.model = list(model= "sGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(1,0), include.mean= TRUE), 
  distribution.model = "std")

GARCHMSFT <- ugarchfit(spec = specgarchMSFT, data = rendMSFT)
print(GARCHMSFT)

#E-GARCH
specegarchMSFT <- ugarchspec(
  variance.model = list(model= "eGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(1,0), include.mean= TRUE), 
  distribution.model = "std")

EGARCHMSFT <- ugarchfit(spec = specegarchMSFT, data = rendMSFT)
print(EGARCHMSFT)

#GJR-GARCH
specgjrgarchMSFT <- ugarchspec(
  variance.model = list(model= "gjrGARCH", garchOrder= c(1,1)), 
  mean.model = list(armaOrder= c(1,0), include.mean= TRUE), 
  distribution.model = "std")

GJRGARCHMSFT <- ugarchfit(spec = specgjrgarchMSFT, data = rendMSFT)
print(GJRGARCHMSFT)

#Dado que tiene un menor AIC y BIC, un mejor LogLik, todos sus 
#parámetros son significativos, captura asimetría de buena manera
#sus diagnósticos son impecables y es más estable que los otros dos, 
#se selecciona el EGARCH. Es importante señalar que el ar1 es marginalmente
#no significativo, lo cual indica que la parte más relevante al modelar 
#la dinámica de la serie es la ecuación de volatilidad y no tanto la de media. 

###Pronósticos para perfiles de riesgo-rendimiento###
##Pronósticos de rendimientos esperados##
#Micron Technology#
forecastMU <- ugarchforecast(EGARCHMU, n.ahead = 15)
rendespMU <- as.numeric(fitted(forecastMU))
rendespMU_df <- data.frame(
                Horizonte = 1:15,
                Rendimiento_Esperado = rendespMU)

#NVIDIA#
forecastNVDA <- ugarchforecast(EGARCHNVDA, n.ahead = 15)
rendespNVDA <- as.numeric(fitted(forecastNVDA))
rendespNVDA_df <- data.frame(
                Horizonte = 1:15,
                Rendimiento_Esperado = rendespNVDA)

#Microsoft#
forecastMSFT <- ugarchforecast(EGARCHMSFT, n.ahead = 15)
rendespMSFT <- as.numeric(fitted(forecastMSFT))
rendespMSFT_df <- data.frame(
                Horizonte = 1:15,
                Rendimiento_Esperado = rendespMSFT)

#Agregando identificación de empresa
rendespMU_df$Empresa <- "Micron"
rendespNVDA_df$Empresa <- "NVIDIA"
rendespMSFT_df$Empresa <- "Microsoft"

#Uniendo bases 
rendesp_total <- rbind(
  rendespMU_df,
  rendespNVDA_df,
  rendespMSFT_df)

#Gráfica de rendimientos esperados 
ggplot(rendesp_total,
       aes(x = Horizonte,
           y = Rendimiento_Esperado,
           color = Empresa)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:15) +
  labs(
    title = "Pronóstico de Rendimientos Esperados a 15 Períodos",
    x = "Horizonte",
    y = "Rendimiento Esperado") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

##Pronósticos de volatilidad esperada##
#Micron Technology# 
volMU <- sigma(forecastMU)
volMU_df <- data.frame(
  Periodo = 1:15,
  Volatilidad = as.numeric(volMU))

#NVIDIA#
volNVDA <- sigma(forecastNVDA)
volNVDA_df <- data.frame(
  Periodo = 1:15,
  Volatilidad = as.numeric(volNVDA))

#Microsoft#
volMSFT <- sigma(forecastMSFT)
volMSFT_df <- data.frame(
  Periodo = 1:15,
  Volatilidad = as.numeric(volMSFT))

#Agregando identificación de empresa
volMU_df$Empresa <- "Micron"
volNVDA_df$Empresa <- "NVIDIA"
volMSFT_df$Empresa <- "Microsoft"

#Uniendo bases 
vol_total <- rbind(
  volMU_df,
  volNVDA_df,
  volMSFT_df)

#Gráfica de volatilidad esperada  
ggplot(vol_total,
       aes(x = Periodo,
           y = Volatilidad,
           color = Empresa)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:15) +
  labs(
    title = "Pronóstico de Volatilidad Esperada a 15 Períodos",
    x = "Horizonte",
    y = "Volatilidad Esperada") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

##Cálculo de VaR para cada empresa##
#Micron Technology# 
#Rendimiento y volatilidad esperada 
muMU <- as.numeric(fitted(forecastMU))
sigmaMU <- as.numeric(sigma(forecastMU))

#Grados de libertad
nuMU <- coef(EGARCHMU)["shape"]

#VaR al 95% y al 99%
VaR95MU <- muMU + sigmaMU * qt(0.05, df = nuMU)
VaR99MU <- muMU + sigmaMU * qt(0.01, df = nuMU)

#Perfil riesgo-rendimiento de Micron Technology  
VaRMU <- data.frame(
  Horizonte = 1:15,
  Rendimiento = muMU,
  Volatilidad = sigmaMU,
  VaR95 = VaR95MU,
  VaR99 = VaR99MU)

#NVIDIA# 
#Rendimiento y volatilidad esperada 
muNVDA <- as.numeric(fitted(forecastNVDA))
sigmaNVDA <- as.numeric(sigma(forecastNVDA))

#Grados de libertad 
nuNVDA <- coef(EGARCHNVDA)["shape"]

#VaR al 95% y al 99%
VaR95NVDA <- muNVDA + sigmaNVDA * qt(0.05, df = nuNVDA)
VaR99NVDA <- muNVDA + sigmaNVDA * qt(0.01, df = nuNVDA)

#Perfil riesgo-rendimiento de NVIDIA
VaRNVDA <- data.frame(
  Horizonte = 1:15,
  Rendimiento = muNVDA,
  Volatilidad = sigmaNVDA,
  VaR95 = VaR95NVDA,
  VaR99 = VaR99NVDA)

#Microsoft#
#Rendimiento y volatilidad esperada 
muMSFT <- as.numeric(fitted(forecastMSFT))
sigmaMSFT <- as.numeric(sigma(forecastMSFT))

#Grados de libertad
nuMSFT <- coef(EGARCHMSFT)["shape"]

#VaR al 95% y al 99%
VaR95MSFT <- muMSFT + sigmaMSFT * qt(0.05, df = nuMSFT)
VaR99MSFT <- muMSFT + sigmaMSFT * qt(0.01, df = nuMSFT)

#Perfil riesgo-rendimiento de Microsoft 
VaRMSFT <- data.frame(
  Horizonte = 1:15,
  Rendimiento = muMSFT,
  Volatilidad = sigmaMSFT,
  VaR95 = VaR95MSFT,
  VaR99 = VaR99MSFT)

##Backtesting de los VaR obtenidos##
##Micron Technology##
#Construyendo VaR histórico
mu_hist_MU <- fitted(EGARCHMU)
sigma_hist_MU <- sigma(EGARCHMU)

nuMU <- coef(EGARCHMU)["shape"]

q95MU <- qdist(
  distribution = "std",
  p = 0.05,
  shape = nuMU)

q99MU <- qdist(
  distribution = "std",
  p = 0.01,
  shape = nuMU)

VaR95_hist_MU <- mu_hist_MU + sigma_hist_MU*q95MU
VaR99_hist_MU <- mu_hist_MU + sigma_hist_MU*q99MU

#Backtesting al 95% y al 99%
VaRTest(
  alpha = 0.05,
  actual = rendMU,
  VaR = as.numeric(VaR95_hist_MU))

VaRTest(
  alpha = 0.01,
  actual = rendMU,
  VaR = as.numeric(VaR99_hist_MU))


## NVIDIA ##
#Construyendo VaR histórico
mu_hist_NVDA <- fitted(EGARCHNVDA)
sigma_hist_NVDA <- sigma(EGARCHNVDA)

nuNVDA <- coef(EGARCHNVDA)["shape"]

q95NVDA <- qdist(
  distribution = "std",
  p = 0.05,
  shape = nuNVDA)

q99NVDA <- qdist(
  distribution = "std",
  p = 0.01,
  shape = nuNVDA)

VaR95_hist_NVDA <- mu_hist_NVDA + sigma_hist_NVDA*q95NVDA
VaR99_hist_NVDA <- mu_hist_NVDA + sigma_hist_NVDA*q99NVDA

#Backtesting al 95% y al 99%
VaRTest(
  alpha = 0.05,
  actual = rendNVDA,
  VaR = as.numeric(VaR95_hist_NVDA))

VaRTest(
  alpha = 0.01,
  actual = rendNVDA,
  VaR = as.numeric(VaR99_hist_NVDA))


## Microsoft ##
#Construyendo VaR histórico
mu_hist_MSFT <- fitted(EGARCHMSFT)
sigma_hist_MSFT <- sigma(EGARCHMSFT)

nuMSFT <- coef(EGARCHMSFT)["shape"]

q95MSFT <- qdist(
  distribution = "std",
  p = 0.05,
  shape = nuMSFT)

q99MSFT <- qdist(
  distribution = "std",
  p = 0.01,
  shape = nuMSFT)

VaR95_hist_MSFT <- mu_hist_MSFT + sigma_hist_MSFT*q95MSFT
VaR99_hist_MSFT <- mu_hist_MSFT + sigma_hist_MSFT*q99MSFT

#Backtesting al 95% y al 99%
VaRTest(
  alpha = 0.05,
  actual = rendMSFT,
  VaR = as.numeric(VaR95_hist_MSFT))

VaRTest(
  alpha = 0.01,
  actual = rendMSFT,
  VaR = as.numeric(VaR99_hist_MSFT))

#Los resultados del backtesting indican que los modelos EGARCH con 
#innovaciones t-Student proporcionan estimaciones adecuadas del 
#Valor en Riesgo para las tres compañías analizadas. Tanto las pruebas 
#de cobertura incondicional de Kupiec como las pruebas de cobertura 
#condicional de Christoffersen no rechazan la hipótesis nula de cobertura 
#correcta e independencia de las excedencias para los niveles de confianza 
#del 95% y 99%. Estos resultados sugieren que los modelos seleccionados 
#capturan adecuadamente la dinámica de la volatilidad y el comportamiento 
#de cola de los rendimientos financieros. Entre las empresas analizadas, NVIDIA 
#presenta la mejor calibración general del VaR, mientras que Micron Technology 
#muestra una ligera sobreestimación del riesgo y Microsoft una leve 
#subestimación al nivel del 95%, aunque dichas diferencias no son 
#estadísticamente significativas.


