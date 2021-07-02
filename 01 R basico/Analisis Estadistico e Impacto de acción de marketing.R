#######################################################
##### Joel Cotrado Vargas (c)  
##### joel.cotrado@gmail.com
##### https://www.linkedin.com/in/joel-cotrado-vargas/
##### ConvergenciaX
##### 2021
##### Analisis de estadistica descriptiva y correlacion de impacto de acción de marketing.
#######################################################
 
install.packages("dplyr")
install.packages("psych")
install.packages("writexl")
install.packages("colorspace")
install.packages("ggplot2")  
library(ggplot2) # Load the librarie (you have to do this one on each new session)
library(psych) 
library(dplyr)
library(writexl)


setwd("./") #setea tu directorio de trabajo
#Carga de datos 
wholefoods <- read.csv("wholefoods.csv")
WH=wholefoods

## Con esto podemos explicar las respuestas 1 y 2:
summary(WH$date)
summary(WH$region)
summary(WH$store)
summary(WH$units)
summary(WH$price)
summary(WH$salesrep)
summary(WH$endcap)
summary(WH$demo)
summary(WH$demo13)
summary(WH$demo45)
summary(WH$natural)
summary(WH$fitness)

 
sd(WH$units)
sd(WH$price)
sd(WH$salesrep)
sd(WH$endcap)
sd(WH$demo)
sd(WH$natural)
sd(WH$fitness)

WH$endcap <- as.factor(WH$endcap)
WH$demo <- as.factor(WH$demo)
summary(WH)
describe(WH)


## Con esto podemos responder la pregunta de los precios en las tiendas. Se puede ver que no son iguales:

#Pregunta 2 - 2.1 Precios Identicos en Tienda? ####
summary(WH$price)
sd(WH$price)
describe(WH$price)
boxplot(WH$price , main='Precios')
hist (WH$price , main='Precios')
 
 
#Pregunta 2 - 2.2 Fraccion de observaciones del producto esta exibido en Gondola? ####


exhibidoGondola <- table(WH$endcap )
exhibidoGondola
barplot(exhibidoGondola,  main="Exhibido en Gondola?", xlab="", ylab="Frecuencia", ylim = c(0, 1500))

 


# Pregunta 3 - 3.1 -  a) Ventas del producto (en unidades), cuando el producto es exhibido en una cabecera de g?ndola vs. ventas unitarias ... ####

# a)  a) Ventas del producto (en unidades), cuando el producto es exhibido en una cabecera de g?ndola vs. ventas unitarias cuando el producto no es exhibido en una cabecera de g?ndola;  cuando el producto no es exhibido en una cabecera de g?ndola; 
 

v1.gondola <- aggregate(WH$units, list(WH$endcap), sum)  # 0 no esta en gondola, 1 esta en gondola
colnames(v1.gondola  ) <- c("EnGondola", "UnidadesTotales")
v1.gondola
print(v1.gondola, quote = FALSE, row.names = FALSE)


f <- ggplot(v1.gondola, aes(EnGondola,UnidadesTotales)) + geom_col() +  theme_classic()  
f

 
#describe(subset(WH, select = c(units, endcap)))

 #ventasUnitarias <- WH %>% select( WH$units, WH$endcap ) group_by( WH$endcap ) 




#WF$price_sq <- WF.df$price2

# b) Ventas del producto (en unidades) cuando se hace una demostraci?n del producto en una tienda y semana 
# determinada vs. en ausencia de una demostraci?n.

v1.demo <- aggregate(WH$units, list(WH$demo), sum)  # 0 no esta en gondola, 1 esta en gondola
colnames(v1.demo ) <- c("ConDemo", "UnidadesTotales")
v1.demo

f <- ggplot(v1.demo, aes(ConDemo,UnidadesTotales)) + geom_col() +  theme_classic()  
f



#Replique el an?lisis anterior para las ventas en d?lares. 
#?Le parecen razonables los resultados obtenidos?

WH$sales <- WH$units * WH$price # Calcular ventas precios x unidades vendidas
head(WH)

v1.dolares <- aggregate(WH$sales, list(WH$demo), sum)  # 0 sin demo, 1 con demo
colnames(v1.dolares) <- c("ConDemo", "VentaDolares")
v1.dolares

f <- ggplot(v1.dolares, aes(ConDemo,VentaDolares)) + geom_col()+  theme_classic()  
f

# 4. Analisis sugerencia privilegiar el producto en cabecera de gondola sobre demostraciones.

#
#4. A partir de los gr?ficos presentados en su respuesta a 3., el gerente de merchandising de la
#cadena sugiere que en el futuro se debiese privilegiar la ubicaci?n del producto en cabecera
#de g?ndola por sobre las demostraciones del producto porque los datos muestran que es una
#forma claramente m?s efectiva de incrementar las ventas. ?Est? de acuerdo con el ejecutivo?




# Pregunta 4 - Extension de analisis

#ventas1.demo <- aggregate(WH$sales, list(WH$demo), )  # 0 sin demo, 1 con demo
#ventas1.gondola <- aggregate(WH$sales, list(WH$endcap), )  # 0 sin demo, 1 con demo
#print(ventas1.demo)
#print(ventas1.gondola)
describe(WH)


# Boxplot Ventasvs demostracion  + ventas vs exhibicion en cabecera de gondola
plot(x=WH$demo, y=WH$sales, main = "Ventas  vs Demostracion",xlab = "Con Demostración?", ylab = "Ventas")

plot(x=WH$endcap, y=WH$sales, main = "Ventas  vs Exhibido en Cabecera de Gondola",xlab = "En Cabecera de Gondola?", ylab = "Ventas")

# Seteamos el precio como factor y obtenemos el valor absoluto del precio.
WH$priceabs <-  round(WH$price)
WH$price <- as.factor(WH$price)
WH$priceabs <- as.factor(WH$priceabs)
WH$natural <- as.factor(WH$natural)
WH$fitness <- as.factor(WH$fitness)
# Boxplot Ventas vs Precios  
plot(x=WH$price, y=WH$sales, main = "Ventas  vs Precio",xlab = "Precios", ylab = "Ventas")
 
# Boxplot Ventas vs Precios en valor absoluto
plot(x=WH$priceabs, y=WH$sales, main = "Ventas  vs Precio en valor absoluto",xlab = "Precios", ylab = "Ventas")

# Boxplot Ventas vs numero de tiendas cercanas
plot(x=WH$natural, y=WH$sales, main = "Ventas cuando existen tiendas especializadas cercanas",xlab = "Número de tiendas cercanas ", ylab = "Ventas")

# Boxplot Ventas vs numero de gimnasios cercanos
plot(x=WH$fitness, y=WH$sales, main = "Ventas  cuando existen gimnasios cercanos ",xlab = "Número de gimnasios cercanos", ylab = "Ventas")




