rm(list=ls())
setwd("~")

##########################################################
#####       Maestría en Economía Aplicada, ITAM      #####
#####                                                #####
#####       Andrea Moreno, Alejandro Grimaldi,       #####
#####       Alejandro Flores y Rodrigo Salinas       #####
#####                                                #####
#####   Proyecto: Consumo de Energía y Temperatura   #####
#####                                                #####
#####         Armado de la Base de Datos             #####
#####                                                #####
#####          Última versión: 16/03/2021            #####
##########################################################





############################################
######## Paquetes y Direcciones (1) ########
############################################

library(openxlsx)
library(rio)
library(tinytex)
library(tidyverse)
library(bit64)
library(data.table)
library(lfe)
library(knitr)
library(stargazer)
library(lubridate)
library(dplyr)
library(plm)

setwd("C:/Users/jesus.flores/OneDrive - CFEnergia, S.A. de C.V/ITAM/Data")





######################################################
######## Precios Marginales Locales 2017-2021 ########
######################################################

pml_files <- list.files(pattern = "*.csv")

pml_prices = bind_rows(lapply(pml_files, read.csv, header = TRUE, sep = ",", skip = 7))

pml_prices = select(pml_prices, -8:-34)

str(pml_prices)

colnames(pml_prices)[colnames(pml_prices)=="Zona.de.Carga"] <- "Node"
colnames(pml_prices)[colnames(pml_prices)=="Precio.Zonal.....MWh."] <- "Node_price"
colnames(pml_prices)[colnames(pml_prices)=="Componente.energia.....MWh."] <- "Energy_component"
colnames(pml_prices)[colnames(pml_prices)=="Componente.perdidas.....MWh."] <- "Losses_component"
colnames(pml_prices)[colnames(pml_prices)=="Componente.Congestion.....MWh."] <- "Congestion_component"

colnames(pml_prices)

pml_prices <- pml_prices[, c(3,1,2,4,5,6,7)]

pml_prices$Fecha <- as.Date(pml_prices$Fecha)

str(pml_prices)

colnames(pml_prices)[colnames(pml_prices)=="Fecha"] <- "Date"
colnames(pml_prices)[colnames(pml_prices)=="Hora"] <- "Hour"

daily_pml <- pml_prices %>%
  group_by(Node,Date) %>%
  summarise(mean_pml = mean(Node_price), mean_energy = mean(Energy_component), mean_losses = mean(Losses_component), mean_congestion = mean (Congestion_component))

write.csv(daily_pml, "C:/Users/jesus.flores/OneDrive - CFEnergia, S.A. de C.V/ITAM/daily_pml.csv", row.names = FALSE )

setwd("C:/Users/jesus.flores/OneDrive - CFEnergia, S.A. de C.V/ITAM/Data_1")

dailygen_files <- list.files(pattern = "*.csv")

daily_gen = bind_rows(lapply(dailygen_files, read.csv, header = TRUE, sep = ",", skip = 7))





############################################
######## Paquetes y Direcciones (2) ########
############################################


library(pacman)
p_load(tidyverse, haven, taRifx, dplyr, foreign, openxlsx, lubridate)

input_16 <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Consumo 2016"
input_17 <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Consumo 2017"
input_18_1 <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Consumo 2018 (Parte 1)"
input_18_2 <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Consumo 2018 (Parte 2)"
input_19 <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Consumo 2019"
input_20 <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Consumo 2020"
input_21 <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Consumo 2021"
output <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos/Output"
output_final <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos"





#########################################
######## Consumo de Energía 2016 ########
#########################################


### Vector con el nombre de las bases.

bases <- list.files(input_16, pattern=".csv")

### Cascarón de la base de datos.

base_final <- data.frame()

### Secuencia de fechas.

fechas <- seq(as.Date("2016-05-04"), as.Date("2016-12-31"), by=1)
fechas <- as.Date("2016-05-04")

### Barra de progreso.

pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

### Loop para abrir, limpiar y juntar las bases.

for(x in 1:length(bases)) {
  
  datos <- read.csv(paste(input_16, bases[x], sep="/"), skip=9, header=F)
  datos <- as.data.frame(datos)
  
  datos <- rename(datos, cve = V1)
  datos <- rename(datos, zona = V2)
  datos <- rename(datos, hora = V3)
  datos <- rename(datos, energia = V4)
  datos$V5 <- NULL
  
  datos <- datos %>% group_by(zona) %>% 
    summarise_at(vars(energia),
                 funs(mean(.)))
  
  estados <- read.xlsx(paste(output_final, "Datos por estado_V1.xlsx", sep = "/"))
  estados <- as.data.frame(estados)
  
  datos <- merge(datos, estados, by = c("zona"), all.x=T)
  
  datos <- datos[complete.cases(datos), ]
  
  fechas <- if(bases[x] == "Demanda Real Retiro_0 Dia Operacion 2016-05-04 v2016 05 18_12 00 00.csv") {fechas} else {fechas + 1}
  
  datos$fecha <- fechas
  
  base_final <- rbind(base_final, datos) # Metemos la información en el cascarón.
  
  order <- c("estado", "zona", "sistema", "energia", "fecha")
  base_final <- base_final[, order]

  setTxtProgressBar(pb,x) # Prendemos barra de progreso.
  
}

### Guardar la base de datos.

write.csv(base_final, paste(output, "Consumo de Energía 2016.csv", sep="/"))





#########################################
######## Consumo de Energía 2017 ########
#########################################


### Vector con el nombre de las bases.

bases <- list.files(input_17, pattern=".csv")

### Cascarón de la base de datos.

base_final <- data.frame()

### Secuencia de fechas.

fechas <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by="days")
fechas <- as.Date("2017-01-01")

### Barra de progreso.

pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

### Loop para abrir, limpiar y juntar las bases.

for(x in 1:length(bases)) {
    
    datos <- read.csv(paste(input_17, bases[x], sep="/"), skip=9, header=F)
    datos <- as.data.frame(datos)
    
    datos <- rename(datos, cve = V1)
    datos <- rename(datos, zona = V2)
    datos <- rename(datos, hora = V3)
    datos <- rename(datos, energia = V4)
    datos$V5 <- NULL
    
    datos <- datos %>% group_by(zona) %>% 
      summarise_at(vars(energia),
                   funs(mean(.)))
    
    estados <- read.xlsx(paste(output_final, "Datos por estado_V1.xlsx", sep = "/"))
    estados <- as.data.frame(estados)
    
    datos <- merge(datos, estados, by = c("zona"), all.x=T)
    
    datos <- datos[complete.cases(datos), ]
    
    fechas <- if(bases[x] == "Demanda Real Retiro_0 Dia Operacion 2017-01-01 v2017 01 15_12 00 00.csv") {fechas} else {fechas + 1}
    
    datos$fecha <- fechas
    
    base_final <- rbind(base_final, datos) # Metemos la información en el cascarón.
    
    order <- c("estado", "zona", "sistema", "energia", "fecha")
    base_final <- base_final[, order]
    
    setTxtProgressBar(pb,x) # Prendemos barra de progreso.
    
  }

### Guardar la base de datos.

write.csv(base_final, paste(output, "Consumo de Energía 2017.csv", sep="/"))





############################################################
######## Consumo de Energía 2018 (hasta 2018-05-21) ########
############################################################


### Vector con el nombre de las bases.

bases <- list.files(input_18_1, pattern=".csv")

### Cascarón de la base de datos.

base_final <- data.frame()

### Secuencia de fechas.

fechas <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")
fechas <- as.Date("2018-01-01")

### Barra de progreso.

pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

### Loop para abrir, limpiar y juntar las bases.

for(x in 1:length(bases)) {
    
    datos <- read.csv(paste(input_18_1, bases[x], sep="/"), skip=9, header=F)
    datos <- as.data.frame(datos)
    
    datos <- rename(datos, cve = V1)
    datos <- rename(datos, zona = V2)
    datos <- rename(datos, hora = V3)
    datos <- rename(datos, energia = V4)
    datos$V5 <- NULL
    
    datos <- datos %>% group_by(zona) %>% 
      summarise_at(vars(energia),
                   funs(mean(.)))
    
    estados <- read.xlsx(paste(output_final, "Datos por estado_V1.xlsx", sep = "/"))
    estados <- as.data.frame(estados)
    
    datos <- merge(datos, estados, by = c("zona"), all.x=T)
    
    datos <- datos[complete.cases(datos), ]
    
    fechas <- if(bases[x] == "Demanda Real Retiro_0 Dia Operacion 2018-01-01 v2018 01 15_12 25 01.csv") {fechas} else {fechas + 1}
    
    datos$fecha <- fechas
    
    base_final <- rbind(base_final, datos) # Metemos la información en el cascarón.
    
    order <- c("estado", "zona", "sistema", "energia", "fecha")
    base_final <- base_final[, order]
    
    setTxtProgressBar(pb,x) # Prendemos barra de progreso.
    
  }

### Guardar la base de datos.

write.csv(base_final, paste(output, "Consumo de Energía 2018 (Parte 1).csv", sep="/"))





############################################################
######## Consumo de Energía 2018 (desde 2018-05-22) ########
############################################################


### Vector con el nombre de las bases.

bases <- list.files(input_18_2, pattern=".csv")

### Cascarón de la base de datos.

base_final <- data.frame()

### Secuencia de fechas.

fechas <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")
fechas <- as.Date("2018-05-22")

### Barra de progreso.

pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

### Loop para abrir, limpiar y juntar las bases.

for(x in 1:length(bases)) {
  
  datos <- read.csv(paste(input_18_2, bases[x], sep="/"), skip=16, header=F)
  datos <- as.data.frame(datos)
  
  datos <- rename(datos, cve = V1)
  datos <- rename(datos, zona = V2)
  datos <- rename(datos, hora = V3)
  datos <- rename(datos, energia = V4)
  datos$V5 <- NULL
  
  datos <- datos %>% group_by(zona) %>% 
    summarise_at(vars(energia),
                 funs(mean(.)))
  
  estados <- read.xlsx(paste(output_final, "Datos por estado_V1.xlsx", sep = "/"))
  estados <- as.data.frame(estados)
  
  datos <- merge(datos, estados, by = c("zona"), all.x=T)
  
  datos <- datos[complete.cases(datos), ]
  
  fechas <- if(bases[x] == "Demanda Real Retiro_0 Dia Operacion 2018-05-22 v2018 06 05_12 00 00.csv") {fechas} else {fechas + 1}
  
  datos$fecha <- fechas
  
  base_final <- rbind(base_final, datos) # Metemos la información en el cascarón.
  
  order <- c("estado", "zona", "sistema", "energia", "fecha")
  base_final <- base_final[, order]
  
  setTxtProgressBar(pb,x) # Prendemos barra de progreso.
  
}

### Guardar la base de datos.

write.csv(base_final, paste(output, "Consumo de Energía 2018 (Parte 2).csv", sep="/"))





#########################################
######## Consumo de Energía 2019 ########
#########################################


### Vector con el nombre de las bases.

bases <- list.files(input_19, pattern=".csv")

### Cascarón de la base de datos.

base_final <- data.frame()

### Secuencia de fechas.

fechas <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="days")
fechas <- as.Date("2019-01-01")

### Barra de progreso.

pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

### Loop para abrir, limpiar y juntar las bases.

for(x in 1:length(bases)) {
    
    datos <- read.csv(paste(input_19, bases[x], sep="/"), skip=16, header=F)
    datos <- as.data.frame(datos)
    
    datos <- rename(datos, cve = V1)
    datos <- rename(datos, zona = V2)
    datos <- rename(datos, hora = V3)
    datos <- rename(datos, energia = V4)
    datos$V5 <- NULL
    
    datos <- datos %>% group_by(zona) %>% 
      summarise_at(vars(energia),
                   funs(mean(.)))
    
    estados <- read.xlsx(paste(output_final, "Datos por estado_V1.xlsx", sep = "/"))
    estados <- as.data.frame(estados)
    
    datos <- merge(datos, estados, by = c("zona"), all.x=T)
    
    datos <- datos[complete.cases(datos), ]
    
    fechas <- if(bases[x] == "Demanda Real Retiro_0 Dia Operacion 2019-01-01 v2019 01 15_12 25 01.csv") {fechas} else {fechas + 1}
    
    datos$fecha <- fechas
    
    base_final <- rbind(base_final, datos) # Metemos la información en el cascarón.
    
    order <- c("estado", "zona", "sistema", "energia", "fecha")
    base_final <- base_final[, order]
    
    setTxtProgressBar(pb,x) # Prendemos barra de progreso.
    
  }

### Guardar la base de datos.

write.csv(base_final, paste(output, "Consumo de Energía 2019.csv", sep="/"))





#########################################
######## Consumo de Energía 2020 ########
#########################################


### Vector con el nombre de las bases.

bases <- list.files(input_20, pattern=".csv")

### Cascarón de la base de datos.

base_final <- data.frame()

### Secuencia de fechas.

fechas <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="days")
fechas <- as.Date("2020-01-01")

### Barra de progreso.

pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

### Loop para abrir, limpiar y juntar las bases.

for(x in 1:length(bases)) {
    
    datos <- read.csv(paste(input_20, bases[x], sep="/"), skip=16, header=F)
    datos <- as.data.frame(datos)
    
    datos <- rename(datos, cve = V1)
    datos <- rename(datos, zona = V2)
    datos <- rename(datos, hora = V3)
    datos <- rename(datos, energia = V4)
    datos$V5 <- NULL
    
    datos <- datos %>% group_by(zona) %>% 
      summarise_at(vars(energia),
                   funs(mean(.)))
    
    estados <- read.xlsx(paste(output_final, "Datos por estado_V1.xlsx", sep = "/"))
    estados <- as.data.frame(estados)
    
    datos <- merge(datos, estados, by = c("zona"), all.x=T)
    
    datos <- datos[complete.cases(datos), ]
    
    fechas <- if(bases[x] == "Demanda Real Retiro_0 Dia Operacion 2020-01-01 v2020 01 15_01 25 01.csv") {fechas} else {fechas + 1}
    
    datos$fecha <- fechas
    
    base_final <- rbind(base_final, datos) # Metemos la información en el cascarón.
    
    order <- c("estado", "zona", "sistema", "energia", "fecha")
    base_final <- base_final[, order]
    
    setTxtProgressBar(pb,x) # Prendemos barra de progreso.
    
  }

### Guardar la base de datos.

write.csv(base_final, paste(output, "Consumo de Energía 2020.csv", sep="/"))





#########################################
######## Consumo de Energía 2021 ########
#########################################


### Vector con el nombre de las bases.

bases <- list.files(input_21, pattern=".csv")

### Cascarón de la base de datos.

base_final <- data.frame()

### Secuencia de fechas.

fechas <- seq(as.Date("2021-01-01"), as.Date("2021-02-05"), by="days")
fechas <- as.Date("2021-01-01")

### Barra de progreso.

pb <- txtProgressBar(min = 1, max = length(bases), style = 3)

### Loop para abrir, limpiar y juntar las bases.

for(x in 1:length(bases)) {
    
    datos <- read.csv(paste(input_21, bases[x], sep="/"), skip=16, header=F)
    datos <- as.data.frame(datos)
    
    datos <- rename(datos, cve = V1)
    datos <- rename(datos, zona = V2)
    datos <- rename(datos, hora = V3)
    datos <- rename(datos, energia = V4)
    datos$V5 <- NULL
    
    datos <- datos %>% group_by(zona) %>% 
      summarise_at(vars(energia),
                   funs(mean(.)))
    
    estados <- read.xlsx(paste(output_final, "Datos por estado_V1.xlsx", sep = "/"))
    estados <- as.data.frame(estados)
    
    datos <- merge(datos, estados, by = c("zona"), all.x=T)
    
    datos <- datos[complete.cases(datos), ]
    
    fechas <- if(bases[x] == "Demanda Real Retiro_0 Dia Operacion 2021-01-01 v2021 01 15_01 25 01.csv") {fechas} else {fechas + 1}
    
    datos$fecha <- fechas
    
    base_final <- rbind(base_final, datos) # Metemos la información en el cascarón.
    
    order <- c("estado", "zona", "sistema", "energia", "fecha")
    base_final <- base_final[, order]
    
    setTxtProgressBar(pb,x) # Prendemos barra de progreso.
    
  }

### Guardar la base de datos.

write.csv(base_final, paste(output, "Consumo de Energía 2021.csv", sep="/"))





##############################################
######## Consumo de Energía 2016-2021 ########
##############################################

### Juntamos las bases de datos.

datos_16 <- read.csv(paste(output, "Consumo de Energía 2016.csv", sep="/"))
datos_17 <- read.csv(paste(output, "Consumo de Energía 2017.csv", sep="/"))
datos_18_1 <- read.csv(paste(output, "Consumo de Energía 2018 (Parte 1).csv", sep="/"))
datos_18_2 <- read.csv(paste(output, "Consumo de Energía 2018 (Parte 2).csv", sep="/"))
datos_19 <- read.csv(paste(output, "Consumo de Energía 2019.csv", sep="/"))
datos_20 <- read.csv(paste(output, "Consumo de Energía 2020.csv", sep="/"))
datos_21 <- read.csv(paste(output, "Consumo de Energía 2021.csv", sep="/"))

base <- rbind(datos_16, datos_17, datos_18_1, datos_18_2, datos_19, datos_20, datos_21)
base$X <- NULL

write.csv(base, paste(output_final, "Consumo de Energía 2016-2021.csv", sep="/"))





############################################
######## Paquetes y Direcciones (3) ########
############################################

Lluvias <- read_excel("C:/Users/andrea.moreno/Desktop/Shaun/Lluvias.xlsx")





################################################
######## Lluvia y Temperatura 2016-2020 ########
################################################

Rainfall <- gather(Lluvias, date, rainfall, mes1:mes60, factor_key = T)

Temperatura_16_20_version_1_xlsb <- read_excel("C:/Users/andrea.moreno/Desktop/Shaun/Temperatura/Temperatura 16-20 (version 1).xlsb.xlsx")

Temp <- gather(Temperatura_16_20_version_1_xlsb, date, temp, mes1:mes60, factor_key = T)

Clima <- merge(Rainfall, Temp)

write.csv(Rainfall, paste(output_final, "Clima.csv", sep="/"))





#######################################################
######## Juntamos las bases de datos 2017-2020 ########
#######################################################


### Base de Precios Marginales Locales.

precios <- read.csv(paste(output_final, "daily_pml.csv", sep="/"))

precios <- rename(precios, fecha = Date)
precios <- rename(precios, nodo = Node)

precios$fecha <- ymd(precios$fecha)

precios <- precios[complete.cases(precios), ]

### Base de Consumo de Energía.

consumo <- read.csv(paste(output_final, "Consumo de Energía 2016-2021.csv", sep="/"))

consumo <- rename(consumo, nodo = zona)
consumo <- rename(consumo, cons_energy = energia)

consumo$fecha <- ymd(consumo$fecha)

consumo$X <- NULL

### Juntamos las bases de datos.

datos <- merge(precios, consumo, by = c("nodo", "fecha"), all.x=T)

### Promedios mensuales de precios y consumo.

datos$fecha <- format(as.Date(datos$fecha), "%Y/%m/%d")
datos$mes_anio <- format(as.Date(datos$fecha), "%Y/%m")

datos <- datos %>% group_by(estado, nodo, mes_anio, sistema) %>% 
  summarise_at(vars(mean_pml, cons_energy),
               funs(mean(.)))

### Base de Temperaturas.

temp <- read.csv(paste(output_final, "temp_panel.csv", sep="/"))

temp <- rename(temp, estado = state)
temp <- rename(temp, fecha = date)

temp$fecha <- dmy(temp$fecha)
temp$fecha <- format(as.Date(temp$fecha), "%Y/%m/%d")
temp$mes_anio <- format(as.Date(temp$fecha), "%Y/%m")
temp$fecha <- NULL

### Base de Lluvias.

lluvia <- read.csv(paste(output_final, "Clima.csv", sep="/"))

lluvia$fecha <- dmy(lluvia$fecha)
lluvia$fecha <- format(as.Date(lluvia$fecha), "%Y/%m/%d")
lluvia$mes_anio <- format(as.Date(lluvia$fecha), "%Y/%m")
lluvia$fecha <- NULL

### Juntamos las bases de datos.

datos <- merge(datos, temp, by = c("estado", "mes_anio"), all.x=T)

datos <- merge(datos, lluvia, by = c("estado", "mes_anio"), all.x=T)

datos <- rename(datos, fecha = mes_anio)

datos <- datos[complete.cases(datos), ]

datos <- datos[order(datos[,1],datos[,2],datos[,3]),]

### Región por estado.

regiones <- read.xlsx(paste(output_final, "regiones_V2.xlsx", sep = "/"))

datos <- merge(datos, regiones, by = c("estado"), all.x=T)

### Población por estado.

datos$fecha <- ym(datos$fecha)
datos$anio <- year(datos$fecha)

pob <- read.csv(paste(output_final, "Población a mitad de año.csv", sep="/"))

datos <- merge(datos, pob, by = c("estado", "anio"), all.x=T)

datos$cons_pc <- datos$cons_energy / datos$pob

write.csv(datos, paste(output_final, "Temperatura y Consumo de Energía 2017-2020.csv", sep="/"))




