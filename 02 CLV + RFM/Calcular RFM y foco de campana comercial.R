##############################################################################################################
##### Joel Cotrado Vargas (c)  
##### joel.cotrado@gmail.com
##### https://www.linkedin.com/in/joel-cotrado-vargas/
##### ConvergenciaX
##### 2021
##### Obtener lista de clientes foco para campaña usando indice RFM (probabilidad de recencia, frecuencia y ##### valor monetario)
####################################################### #######################################################

install.packages("dplyr")
install.packages("psych")
install.packages("writexl")
install.packages("data.table")
#install.packages("colorspace")
#install.packages("ggplot2")  
library(ggplot2) # Load the librarie (you have to do this one on each new session)
library(psych) 
library(dplyr)
library(writexl)

quintilesRFM <- function(x, df) {
  quant <- quantile(x, probs = c(0.2, 0.4, 0.6, 0.8))
  quin <-  rep(0, nrow(df))
  quin <- ifelse(x <= quant[1], 1, ifelse(x <= quant[2], 2, ifelse(x <= quant[3], 3, ifelse(x <= quant[4], 4, 5))))
  return(quin)
}

setwd("./") #setea tu directorio de trabajo aqui.
onlineResponse.df <- read.csv("Online_Response_T2.csv")
onlineRetailData.df <- read.csv("Online_Retail_Data_T2.csv")
describe(onlineResponse.df )
describe(onlineRetailData.df)
colnames(onlineResponse.df) <- c("x","customerid","response") # renombramos customer_id por customerid 
onlineResponse.df <- onlineResponse.df[,-1] #eliminamos campot "X" para facilitar el merge.
onlineRetailData.df <- onlineRetailData.df[,-1]

#onlineResponse.df$response <- as.factor(onlineResponse.df$response )
onlineResponse.df$customerid <- as.factor(onlineResponse.df$customerid)
onlineRetailData.df$customerid  <- as.factor(onlineRetailData.df$customerid)

head(onlineResponse.df)
head(onlineRetailData.df)
describe(onlineResponse.df )
describe(onlineRetailData.df)

#Combinamos ambos data frame en un nuevo data frame dataCustomer.df ######

dataCustomer.df <- merge(onlineResponse.df, onlineRetailData.df, by = "customerid")
head(dataCustomer.df)
#summary(dataCustomer.df)

Sys.setlocale("LC_ALL","English")

dataCustomer.df$date = as.Date(dataCustomer.df$date)
dataCustomer.df$invoicedate = as.Date(dataCustomer.df$invoicedate) # NOTA 1:

write_xlsx(dataCustomer.df,"dataCustomer.df.xlsx")

# Pregunta 2.a - calcular parametros RFM (Recencia, frecuencia y Monto) ####

#Llevar fecha a formato fecha, usa usa la ultima fecha de compra como base para calcular los dias de recencia
timebase=as.Date(max(dataCustomer.df$date)) #
print(timebase)  #La mayor fecha?? confirmar
dataCustomer.df$dRecencia  = as.numeric(difftime(timebase, time2 = dataCustomer.df$date, units = "days"))

dataCustomer.df$monto =  dataCustomer.df$unitprice*dataCustomer.df$quantity
dataCustomer.df <- dataCustomer.df[with(dataCustomer.df, order(dataCustomer.df$customerid)), ]  
head(dataCustomer.df)

#Antes de calcular el monto, se calculan montos agrupados por factura y por cliente (un unico monto por factura)
library(data.table)
wholefoods <- as.data.table(dataCustomer.df)
data_agregada=wholefoods[,.(monto=sum(monto)),by=list(invoiceno,customerid)]
frecuencia=data_agregada[,.N,by=customerid]
head(data_agregada)
head(frecuencia)

Recencia = aggregate(dataCustomer.df$dRecencia , list(dataCustomer.df$customerid), min) # calcula la menor recencia
#Monetario = aggregate(dataCustomer.df$monto, list(dataCustomer.df$customerid), mean)  # Calcula la media del monto.
#Frecuencia = aggregate(dataCustomer.df$customerid , list(dataCustomer.df$customerid), length)

Monetario = aggregate(data_agregada$monto, list(data_agregada$customerid), mean)  # Calcula la media del monto.
Frecuencia = frecuencia



colnames(Monetario) = c('customerid','monetario')
colnames(Frecuencia) = c('customerid','frecuencia')
colnames(Recencia) = c('customerid','recencia')

datosRFM = Recencia
datosRFM$frecuencia=Frecuencia$frecuencia
datosRFM$monetario=Monetario$monetario
#datosRFM$frecuencia=as.data.frame(Frecuencia)
#datosRFM$monetario=as.data.frame(Monetario)
print("Parametros RFM")
head(datosRFM)

#graficar histogrmas de parametros RFM
hist (datosRFM$recencia , main='Distribuciónde Recencia de compra')
hist (datosRFM$frecuencia , main='Distribuciónde de frecuencia de compra')
hist (datosRFM$monetario , main='Distribuciónde de montos  de compra')
summarise(datosRFM)

describe(datosRFM)

# Pregunta 2b - Obtener quintiles RFM  + Agregando indice RFM ####

rec_quin = quintilesRFM(-datosRFM$recencia, datosRFM)
freq_quin = quintilesRFM(datosRFM$frecuencia, datosRFM)
mon_quin =  quintilesRFM(datosRFM$monetario, datosRFM)

datosRFM$rec_quin <- rec_quin
datosRFM$freq_quin <- freq_quin
datosRFM$mon_quin <- mon_quin
head(datosRFM,10)

# Calculando variable RFM
datosRFM$rfm <- 100*datosRFM$rec_quin + 10*datosRFM$freq_quin + datosRFM$mon_quin
head(datosRFM)
write_xlsx(datosRFM,"datosRFM_1.xlsx")
#tail(datosRFM)
#datosRFM$customerid <- as.factor(datosRFM$customerid)
#onlineResponse.df$response <- as.factor(onlineResponse.df$response)
head(onlineResponse.df)
datosRFM <- merge(datosRFM, onlineResponse.df , by = "customerid") #Combinamos data_rfm con respuesta por cliente

#head(datosRFM)
#tail(datosRFM)
#describe(datosRFM)
write_xlsx(datosRFM,"datosRFM_2.xlsx")

resp_prob.df <- aggregate( datosRFM$response , list(datosRFM$rfm ), mean) #Calculamos probabilidad de respuesta por cliente
resp_prob.df

colnames(resp_prob.df) = c('rfm','resp_prob')
head(resp_prob.df)
tail(resp_prob.df)

datosRFM <- merge(datosRFM, resp_prob.df, by ="rfm") #Combinamos probabilidad de respuesta y datos del cliente por rfm

write_xlsx(datosRFM,"datosRFM_3.xlsx")

head(datosRFM)

hist (datosRFM$rfm , main='Distribución de variable RFM')
hist (datosRFM$resp_prob , main='Distribución de probabilidad de RespuestaM')
# Graficos RFM vs Probabilidad de respuesta ####
resp_prob.df$rfm <- as.factor(resp_prob.df$rfm)
g<-ggplot(data=resp_prob.df, aes(x=rfm, y=resp_prob)) +
  geom_bar(stat="identity") +
  theme(axis.text=element_text(size=4)) +
  xlab("Indice RFM") + ylab("Probabilidad de Respuesta")

g

##Graficando Recencia, Frecuencia y Valor Monetario  VS Probabilidad de Respueta ####

#Recencia
resp_prob_by_r <- aggregate(datosRFM$response, list(datosRFM$rec_quin), mean)
colnames(resp_prob_by_r) = c('rec_quin', 'resp_prob')
head(resp_prob_by_r)
g_rec_presp <- ggplot(data=resp_prob_by_r, aes(x=rec_quin, y=resp_prob)) +
  geom_bar(stat="identity") +
  theme(axis.text=element_text(size=4)) +
  xlab("Recencia") + ylab("Probabilidad de Respuesta (por quintil)")

g_rec_presp
head(resp_prob_by_r)

#Frecuencia
resp_prob_by_f <- aggregate(datosRFM$response, list(datosRFM$freq_quin), mean)
colnames(resp_prob_by_f) = c('freq_quin', 'resp_prob')
head(resp_prob_by_f)
 
g_freq_presp <- ggplot(data=resp_prob_by_f, aes(x=freq_quin, y=resp_prob)) +
  geom_bar(stat="identity") +
  theme(axis.text=element_text(size=4)) +
  xlab("Frecuencia") + ylab("Probabilidad de Respuesta (por quintil)")

g_freq_presp
  
#Monto vs probabilidad de respuesta

resp_prob_by_m <- aggregate(datosRFM$response, list(datosRFM$mon_quin), mean)
colnames(resp_prob_by_m) = c('mon_quin', 'resp_prob')
head(resp_prob_by_m)

g_mon_presp <- ggplot(data=resp_prob_by_m, aes(x=mon_quin, y=resp_prob)) +
  geom_bar(stat="identity") +
  theme(axis.text=element_text(size=4)) +
  xlab("Valor Monetario") + ylab("Probabilidad de Respuesta (por quintil)")

g_mon_presp

#Pregunta 2.c - Analisis de Punto de Equilibrio  ####

 
ganancia = 60 
costoOferta = 2
be = costoOferta/ganancia #probabilidad de respuesta
datosRFM$contactar <- ifelse(datosRFM$resp_prob > be, 1, 0) # si es mayor, que la probabilidad, contactar (1)
 
summary(datosRFM)
head(datosRFM)
write_xlsx(datosRFM,"datosRFM_4_contactar.xlsx")

#Numero de clientes que respondieron a la campaña
print("Numero de clientes que respondieron (en valor 1)")
table(datosRFM$response)

#Numero de clientes a contactar
print("Numero de clientes a contactar (en valor 1)")
table(datosRFM$contactar)

#Porcentaje de clientes a contactar
print("Clientes a contactar")
tapply(datosRFM$response, datosRFM$contactar,  function(x) format(mean(x), scientific = FALSE))