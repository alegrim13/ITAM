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
#####       Procesamiento de la Base de Datos        #####
#####                                                #####
#####          Última versión: 16/03/2021            #####
##########################################################





########################################
######## Paquetes y Direcciones ########
########################################

library(pacman)
p_load(tidyverse, haven, taRifx, dplyr, foreign, openxlsx, plm, clubSandwich, stargazer, mxmaps, lubridate)

input <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Bases de Datos"
output <- "C:/Users/ale_g/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 1/Imágenes y Tablas"





###########################################
######## Estadísticas descriptivas ########
###########################################


### Abrir la base de datos.

datos <- read.csv(paste(input, "Temperatura y Consumo de Energía 2017-2020.csv", sep="/"))

datos <- as.data.frame(datos)

datos$fecha <- ymd(datos$fecha)

datos$X <- NULL


### Tabla de estadísticas descriptivas.

p_datos <- pdata.frame(datos, index = c("nodo", "fecha"))
is.pbalanced(p_datos)
p_datos <- make.pbalanced(p_datos, balance.type = "shared.times") # Nos quedamos con el panel balanceado.
is.pbalanced(p_datos)

datos <- as.data.frame(p_datos)

xtabs(~nodo, data=datos)

stargazer(datos, type="latex", digits=2, title = "Descriptive statistics", omit = c("cve_estado", "anio"),
          covariate.labels = c("Marginal Price", "Energy Consumption", "Temperature", "Rainfalls", "Population", "Energy Consumption per capita"), font.size="footnotesize", header=FALSE,
          out=(paste(output, "Tabla de Estadística Descriptiva.tex", sep="/")))




          
##########################
######## Gráficas ########
##########################


### Gráfica de dispersión.

gr <- ggplot(datos, aes(x=temp, y=cons_pc)) +
  geom_point(colour="dodgerblue4") +
  geom_smooth(method="lm", formula= y ~ x, colour="darkred") +
  ggtitle("Temperature and energy consumption") +
  labs(x="Temperature", y="Energy Consumption per capita")
gr

ggsave(paste(output, "Gráfica de Temperatura y Consumo de Energía.png", sep="/"), plot=gr, width=10, height=6)


### Gráfica de dispersión por regiones.

xtabs(~region, data=datos)

gr <- ggplot(datos, aes(x=temp, y=cons_pc, color=region)) +
  geom_smooth(method="lm", formula=y ~ x + I(x^2), alpha=0.3) +
  labs(title="Quadratic relation between temperature and energy consumption by region",
       x="Temperature", y="Energy Consumption per capita", color="Region") + ylim(0,NA)
gr

ggsave(paste(output, "Gráfica de Temperatura y Consumo de Energía por regiones.png", sep="/"), plot=gr, width=10, height=6)


### Gráfica de medias por regiones.

medias <- datos %>% group_by(region, fecha) %>% 
  summarise_at(vars(mean_pml:lluvias, cons_pc),
               funs(mean(.)))

gr <- ggplot(medias, aes(x=temp, y=cons_pc, color=region)) +
  geom_line() +
  geom_point() +
  labs(title="Means of temperature and energy consumption by region",
       x="Temperature", y="Energy Consumption per capita", color="Region")
gr

ggsave(paste(output, "Medias de Temperatura y Consumo de Energía por regiones.png", sep="/"), plot=gr, width=10, height=6)


### Gráfica de dispersión y de medias por regiones.

medias <- datos %>% group_by(region, fecha) %>% 
  summarise_at(vars(mean_pml:lluvias, cons_pc),
               funs(mean(.)))

gr <- ggplot(datos, aes(x=temp, y=cons_pc, color=region)) +
  geom_smooth(method="lm", formula=y ~ x + I(x^2), alpha=0.3) +
  geom_line(data=medias) +
  geom_point(data=medias) +
  labs(title="Quadratic relationship and means of temperature and energy consumption by region",
       x="Temperature", y="Energy Consumption per capita", color="Region") + ylim(0,NA)
gr

ggsave(paste(output, "Relación y medias de Temperatura y Consumo de Energía por regiones.png", sep="/"), plot=gr, width=10, height=6)


### Temperaturas en el tiempo por regiones.

medias <- datos %>% group_by(region, fecha) %>% 
  summarise_at(vars(mean_pml:lluvias, cons_pc),
               funs(mean(.)))

medias$fecha <- ymd(medias$fecha)

gr <- ggplot(medias, aes(x=fecha, y=temp, color=region)) +
  geom_line() +
  geom_point() +
  labs(title="Means of temperature by month and by region",
       x="Months", y="Temperature", color="Region") +
  scale_x_date(date_labels = "%b/%y")
gr

ggsave(paste(output, "Temperatura por meses y por regiones.png", sep="/"), plot=gr, width=10, height=6)


### Mapa de temperaturas promedio en 2020.

temp <- as.data.frame(p_datos)
temp <- subset(temp, anio==2020)

temp <- temp %>%
  group_by(cve_estado) %>% 
  summarise_at(vars(mean_pml:lluvias),
               funs(mean(.)))

temp <- temp %>% select(cve_estado, temp)
temp <- rename(temp, region = cve_estado)
temp <- rename(temp, value = temp)

mxstate_choropleth(temp,
                   num_colors = 1,
                   title = "Mean temperature by state in 2020",
                   legend = "°C") # Tamaño: 580 w | 380 h.

mxhexbin_choropleth(temp,
                   num_colors = 1,
                   title = "Mean temperature by state in 2020",
                   legend = "°C")


### Mapa de temperaturas máximas en 2020.

temp <- as.data.frame(p_datos)
temp <- subset(temp, anio==2020)

temp <- temp %>%
  group_by(cve_estado) %>% 
  summarise_at(vars(mean_pml:lluvias),
               funs(max(.)))

temp <- temp %>% select(cve_estado, temp)
temp <- rename(temp, region = cve_estado)
temp <- rename(temp, value = temp)

mxstate_choropleth(temp,
                   num_colors = 1,
                   title = "Maximum temperature by state in 2020",
                   legend = "°C")

mxhexbin_choropleth(temp,
                   num_colors = 1,
                   title = "Maximum temperature by state in 2020",
                   legend = "°C")


### Mapa de temperaturas mínimas en 2020.

temp <- as.data.frame(p_datos)
temp <- subset(temp, anio==2020)

temp <- temp %>%
  group_by(cve_estado) %>% 
  summarise_at(vars(mean_pml:lluvias),
               funs(min(.)))

temp <- temp %>% select(cve_estado, temp)
temp <- rename(temp, region = cve_estado)
temp <- rename(temp, value = temp)

mxstate_choropleth(temp,
                   num_colors = 1,
                   title = "Minimum temperature by state in 2020",
                   legend = "°C")

mxhexbin_choropleth(temp,
                   num_colors = 1,
                   title = "Minimum temperature by state in 2020",
                   legend = "°C")


### Mapa de consumo promedio en 2020.

temp <- as.data.frame(p_datos)
temp <- subset(temp, anio==2020)

temp <- temp %>%
  group_by(cve_estado) %>% 
  summarise_at(vars(mean_pml:lluvias, cons_pc),
               funs(mean(.)))

temp <- temp %>% select(cve_estado, cons_pc)
temp <- rename(temp, region = cve_estado)
temp <- rename(temp, value = cons_pc)

mxstate_choropleth(temp,
                   num_colors = 1,
                   title = "Mean per capita energy consumption by state in 2020",
                   legend = "MWh")


### Mapa de consumos máximos en 2020.

temp <- as.data.frame(p_datos)
temp <- subset(temp, anio==2020)

temp <- temp %>%
  group_by(cve_estado) %>% 
  summarise_at(vars(mean_pml:lluvias, cons_pc),
               funs(max(.)))

temp <- temp %>% select(cve_estado, cons_pc)
temp <- rename(temp, region = cve_estado)
temp <- rename(temp, value = cons_pc)

mxstate_choropleth(temp,
                   num_colors = 1,
                   title = "Maximum per capita energy consumption by state in 2020",
                   legend = "MWh")





######################################
######## Modelos de Regresión ########
######################################


### 1. OLS Pooled.

p_datos <- pdata.frame(datos, index = c("nodo", "fecha"))
is.pbalanced(p_datos)

modelo1 <- lm(log(cons_pc) ~ temp + mean_pml + lluvias, p_datos)
cov1 <- vcovCR(modelo1, type="CR1", cluster=p_datos$nodo)
robust_se1 <- sqrt(diag(cov1))


### 2. OLS Pooled con factor cuadrático.

p_datos$temp_sqr <- p_datos$temp * p_datos$temp

modelo2 <- lm(log(cons_pc) ~ temp + temp_sqr + mean_pml + lluvias, p_datos)
cov2 <- vcovCR(modelo2, type="CR1", cluster=p_datos$nodo)
robust_se2 <- sqrt(diag(cov2))


### 3. Efectos Fijos.

modelo3 <- plm(log(cons_pc) ~  temp + mean_pml + lluvias + factor(fecha), p_datos, model = "within")
cov3 <- vcovCR(modelo3, type="CR1", cluster=p_datos$nodo)
robust_se3 <- sqrt(diag(cov3))


### 4. Efectos Fijos con factor cuadrático.

modelo4 <- plm(log(cons_pc) ~ temp + temp_sqr + mean_pml + lluvias + factor(fecha), p_datos, model = "within")
cov4 <- vcovCR(modelo4, type="CR1", cluster=p_datos$nodo)
robust_se4 <- sqrt(diag(cov4))


### Tabla de Regresiones.

stargazer(modelo1, modelo2, modelo3, modelo4,
          title="Linear Regression Models",
          se=list(robust_se1, robust_se2, robust_se3, robust_se4),
          omit=c("fecha"),
          font.size="scriptsize",
          column.sep.width="0pt",
          model.names=F,
          add.lines=list(c("Errors", "Cluster by zone", "Cluster by zone", "Cluster by zone", "Cluster by zone"),
                         c("Effects", "No", "No", "Fixed: Zone and month", "Fixed: Zone and month")),
          covariate.labels = c("Temperature", "Squared Temperature", "Marginal Price", "Rainfalls"),
          dep.var.labels="Log(Energy Consumption per capita)", no.space=TRUE, header=FALSE,
          out=(paste(output, "Tabla de Regresiones.tex", sep="/")))





#########################################
######## Gráfica del Partial Out ########
#########################################


### (a) Temperatura contra controles.

p_datos <- pdata.frame(datos, index = c("nodo", "fecha"))
is.pbalanced(p_datos)

partial1 <- plm(temp ~ mean_pml + lluvias + factor(fecha), p_datos, model = "within")


### (b) Consumo de energía contra residuales.

p_datos$res <- residuals(partial1)
partial2 <- lm(log(cons_pc) ~ res, data=p_datos)
summary(partial2)


### (c) Gráfica de dispersión y ajuste lineal.

p_datos <- as.data.frame(p_datos)

p_datos$ln_cons <- log(p_datos$cons_pc)

gr <- ggplot(p_datos, aes(x=res, y=ln_cons)) +
  geom_point(colour="dodgerblue4") +
  geom_smooth(method="lm", formula= y ~ x, colour="darkred") +
  ggtitle("Partial effect of temperature on energy consumption") +
  labs(x="Temperature (Partial-out residuals)", y="Log(Energy Consumption per capita)")
gr

ggsave(paste(output, "Gráfica del efecto parcial.png", sep="/"), plot=gr, width=10, height=6)





####################################################
######## Modelos de Regresión (sin outlier) ########
####################################################


### Quitamos el "outlier" que se ve en la gráfica.

p_datos <- p_datos[which(p_datos$res<10),]


### 1. OLS Pooled.

p_datos <- pdata.frame(p_datos, index = c("nodo", "fecha"))
is.pbalanced(p_datos)
p_datos <- make.pbalanced(p_datos, balance.type = "shared.times") # Nos quedamos con el panel balanceado.
is.pbalanced(p_datos)

modelo1 <- lm(log(cons_pc) ~ temp + mean_pml + lluvias, p_datos)
cov1 <- vcovCR(modelo1, type="CR1", cluster=p_datos$nodo)
robust_se1 <- sqrt(diag(cov1))


### 2. OLS Pooled con factor cuadrático.

p_datos$temp_sqr <- p_datos$temp * p_datos$temp

modelo2 <- lm(log(cons_pc) ~ temp + temp_sqr + mean_pml + lluvias, p_datos)
cov2 <- vcovCR(modelo2, type="CR1", cluster=p_datos$nodo)
robust_se2 <- sqrt(diag(cov2))


### 3. Efectos Fijos.

modelo3 <- plm(log(cons_pc) ~  temp + mean_pml + lluvias + factor(fecha), p_datos, model = "within")
cov3 <- vcovCR(modelo3, type="CR1", cluster=p_datos$nodo)
robust_se3 <- sqrt(diag(cov3))


### 4. Efectos Fijos con factor cuadrático.

modelo4 <- plm(log(cons_pc) ~ temp + temp_sqr + mean_pml + lluvias + factor(fecha), p_datos, model = "within")
cov4 <- vcovCR(modelo4, type="CR1", cluster=p_datos$nodo)
robust_se4 <- sqrt(diag(cov4))


### Tabla de Regresiones.

stargazer(modelo1, modelo2, modelo3, modelo4,
          title="Linear Regression Models (without outlier)",
          se=list(robust_se1, robust_se2, robust_se3, robust_se4),
          omit=c("fecha"),
          font.size="scriptsize",
          column.sep.width="0pt",
          model.names=F,
          add.lines=list(c("Errors", "Cluster by zone", "Cluster by zone", "Cluster by zone", "Cluster by zone"),
                         c("Effects", "No", "No", "Fixed: Zone and month", "Fixed: Zone and month")),
          covariate.labels = c("Temperature", "Squared Temperature", "Marginal Price", "Rainfalls"),
          dep.var.labels="Energy Consumption", no.space=TRUE, header=FALSE,
          out=(paste(output, "Tabla de Regresiones (sin outlier).tex", sep="/")))





#######################################################
######## Gráfica del Partial Out (sin outlier) ########
#######################################################


### (a) Temperatura contra controles.

p_datos <- pdata.frame(p_datos, index = c("nodo", "fecha"))
is.pbalanced(p_datos)

partial1 <- plm(temp ~ mean_pml + lluvias + factor(fecha), p_datos, model = "within")


### (b) Consumo de energía contra residuales.

p_datos$res2 <- residuals(partial1)
partial2 <- lm(log(cons_pc) ~ res2, data=p_datos)
summary(partial2)


### (c) Gráfica de dispersión y ajuste lineal.

p_datos <- as.data.frame(p_datos)

p_datos$ln_cons <- log(p_datos$cons_pc)

gr <- ggplot(p_datos, aes(x=res2, y=ln_cons)) +
  geom_point(colour="dodgerblue4") +
  geom_smooth(method="lm", formula= y ~ x, colour="darkred") +
  ggtitle("Partial effect of temperature on energy consumption (without outlier)") +
  labs(x="Temperature (Partial-out residuals)", y="Log(Energy Consumption per capita)")
gr

ggsave(paste(output, "Gráfica del efecto parcial (sin outlier).png", sep="/"), plot=gr, width=10, height=6)


