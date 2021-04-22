pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr)
getwd()
sanguchez <- read.csv2("/Users/gabrielengel/Downloads/sanguchez.csv")
sanguchez
install.packages("tidyverse")
install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages("lmtest")
library(lmtest)
library(tidyverse)
library(ggplot2)
library(dplyr)


sanguchez.sinNA <- sanguchez[complete.cases(sanguchez), ] #eliminar columnas con NA
sanguchez.sinNA <- sanguchez[-c(79, 170, 365, 407),] #eliminar filas sin valores
sanguchez.sinNA
count(sanguchez.sinNA, nota)
view(sanguchez.sinNA.conID)
vec <- c(1:402)
sanguchez.sinNA.conID <- sanguchez.sinNA
sanguchez.sinNA.conID$ID <- vec
sanguchez.sinNA.conID #agregar columna con ID para cada sandwich

str(sanguchez.sinNA.conID)
sanguchez.sinNA.conID.1 <- sanguchez.sinNA.conID[c(2, 4:8)]
sanguchez.sinNA.conID.1 #columnas importantes

install.packages("stringr")

ingredientes<- str_split_fixed(sanguchez.sinNA.conID.1$Ingredientes,",", 12)
ingredientes #separar por ingredientes

sangus <- sanguchez.sinNA.conID.1[c(1:2, 4:5)]

ing_cbind <- cbind(sangus, ingredientes)
ing_sep<- bind_cols(sanguchez.sinNA.conID.1, ingredientes)

ing_cbind_precios_correctos <- ing_cbind #unir los ingredientes separados al dataframe

# convertir todo a CLP
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "12 NZD"] <- "$6.051"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "12 USD"] <- "$8.605"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "17 AUD"] <- "$9.278"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == " 11 USD"] <- "$7.888"
ing_cbind_precios_correctos[ing_cbind_precios_correctos %in% c("15 AUD")] <- "$8.186"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "18 USD"] <- "$12.907"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "$4.200 aprox."] <- "$4.200"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "22 USD"] <- "$15.776"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "8 USD"] <- "$5.736"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "$2.000 (2 por $3.600)"] <- "$2.000"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "18 soles (Aprox $3.600)"] <- "$3.600"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "$3500 sandwich y combo $5.500"] <- "$3.500"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "15.5 AUD"] <- "$8.459"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "9 USD"] <- "$6.453"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "9.5 USD"] <- "$6.812"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "23 NZD"] <- "$11.597"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "28 USD"] <- "$20.078"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "19 AUD"] <- "$10.369"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "20 USD"] <- "$14.342"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "$14.600 aprox."] <- "$14.600"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "14.90 soles (aprox $3.000)"] <- "$3.000"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "7 USD"] <- "$5.019"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "16 USD"] <- "$11.473"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "16.9 AUD en pan normal, y 66.9 en pan de oro"] <- "$9.223"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "14.00 NZD"] <- "$7.059"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "$4.200 con bebida"] <- "$4.200"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "13.5 AUD"] <- "$7.368"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == " 26 USD"] <- "$18.644"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "$140 pesos argentinos (Aprox $3.150)"] <- "$3.150"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "13 USD"] <- "$9.322"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "14 AUD"] <- "$7.640"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "25 USD"] <- "$17.927"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "17 USD"] <- "$12.190"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "22 NZD"] <- "$11.093"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "18 AUD"] <- "$9.824"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "7 USD"] <- "$5.019"

ing_cbind_precios_correctos$Precio = as.numeric(gsub("\\$", "", ing_cbind_precios_correctos$Precio))
ing_cbind_precios_correctos$Precio = as.numeric(gsub("\\.", ",", ing_cbind_precios_correctos$Precio))

ing_cbind_precios_correctos[ing_cbind_precios_correctos == " Hamburguesa"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == " Hamburguesa con queso azul gratinado"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == " Hamburguesa de 180 grs de punta picana y tocino"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == " Hamburguesa de 200 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == " Hamburguesa de carne de vacuno y cerdo molida condimentada con ajo y aj�
"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "$Hamburguesa"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "3 Hamburguesas de mezcla de wagyu con angus
"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Burger casera de 120 gramos"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Cheeseburger con pebre (palta"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Doble hamburguesa"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Doble hamburguesa (220 grs)"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hambuguesa"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa (340 grs de carne)"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa 50% carne y 50% prieta"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa a la parrilla"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa al carb�n"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa Angus de 230 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa Angus rellana con queso"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa Black Angus"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa casera"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa casera a la parrilla"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa casera de 150 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa casera de 170 gramos rellena con queso Cheddar"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa con queso Brie apanado"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa con queso mantecoso fundido"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa con tocino"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa cubierta con crema de queso azul"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 150 gramos"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 180 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 180 grs rellena con queso Cheddar"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 200 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 200 grs rellena de queso Cheddar"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 220 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 225 grs."] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 230 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 250 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 3 cortes de carne"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de 300 gramos rellena de queso"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de ara�ita de Wagyu"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de vacuno (posta)"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de vacuno Hereford de 175 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de Wagyu"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de wagyu de 185 grs"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa de Wagyu y Angus con cebolla caramelizada al maple syrup"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa dry-aged"] <- "Hamburguesa de vacuno"
ing_cbind_precios_correctos[ing_cbind_precios_correctos == "Hamburguesa Hereford"] <- "Hamburguesa de vacuno"

hamb<- filter(ing_cbind_precios_correctos, ingrediente_principal == "Hamburguesa de vacuno")
hamb_mustra <- filter(hamb, texto == "Pucha que echaba de menos comerme un s<e1>ndwich rico!! Y este, de la sangucher<ed>a <93>El Distrito<94> (@eldistritorestaurant), estaba exquisito! Gracias a la recomendaci<f3>n de<a0>@juanpabloswett, hoy no lo dud<e9> y junto a<a0>@speraltav<a0>nos vinimos al local que est<e1> en el Metro El Golf. Se llama <93>Championion Burger<94>: Hamburguesa, queso suizo, champi<f1>ones asados, cebolla grillada y mayonesa alioli. Simple, pero cada uno de los ingredientes estaba en su justa medida. Aplausos aparte para los champi<f1>ones y para la cebolla. Creo que son los mejores que he probado. El queso, sin invadir, aportaba lo suyo. Lo mismo con la mayo. Cero pasada a ajo, sino que con el toque que uno busca. La hamburguesa misma estaba rica, pero me hubiese gustado que preguntaran el punto de cocci<f3>n, porque estaba muy cocida y, por lo mismo, no tan jugosa. Pero rica. As<ed> que buen descubrimiento saber de este local! Detalle notable? Te regalan un helado al final de la comida
")

#buscar relacion entre nota y precio
relacion<- plot(hamb$Precio, hamb$nota)









hamb <- ing_cbind_precios_correctos[c(1:2, 4:5)]






#histograma de notas de sandwiches
hist(ing_cbind_precios_correctos$nota) 
hist(ing_cbind_precios_correctos$Precio)

colnames(ing_cbind_precios_correctos)[colnames(ing_cbind_precios_correctos) == "1"] <- "ingrediente_principal"
summarise(ing_cbind_precios_correctos$`1`)
count(ing_cbind_precios_correctos, nota, ingrediente_pr)
str(ing_cbind_precios_correctos)

count
#ordenar el ingrediente principal
new_ordenado <- ing_cbind_precios_correctos %>% mutate(ingrediente_principal = replace(ingrediente_principal, match("")))

library(tidyverse)
sang_cinco <- filter(ing_cbind_precios_correctos, nota > 4)

cinco_estrellas <- ing_cbind_precios_correctos$nota[nota == "5"]
