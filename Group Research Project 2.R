rm(list=ls())
setwd("~")

##########################################################
#####       Maestría en Economía Aplicada, ITAM      #####
#####                                                #####
#####       Andrea Moreno, Alejandro Grimaldi,       #####
#####       Alejandro Flores y Rodrigo Salinas       #####
#####                                                #####
#####           Group Research Project 2             #####
#####                                                #####
#####       Procesamiento de la Base de Datos        #####
#####                                                #####
#####          última versión: 05/05/2021            #####
##########################################################





########################################
######## Paquetes y Direcciones ########
########################################

library(pacman)
p_load(tidyverse, haven, taRifx, dplyr, foreign, openxlsx, plm, clubSandwich, stargazer, mxmaps, lubridate, readxl, rdd)

input <- "C:/Users/agrimaldi/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 2/Bases de Datos"
output <- "C:/Users/agrimaldi/Dropbox/ITAM MEA/Cuarto Semestre/Applied Research II/Group Projects/Group Project 2/Imágenes y Tablas"

### Abrir la base de datos.

datos <- read_excel(paste(input, "Electricidad y gas por regiones.xlsx", sep="/"))

datos <- as.data.frame(datos)

datos$date <- ymd(datos$date)





############################################
######## Commodities prices and LMP ########
############################################

temp <- datos

temp$waha_s <- temp$waha * temp$waha_supply
temp$hsc_s <- temp$hsc * temp$hsc_supply

### Pooled OLS.

modelo1 <- lm(zone_price ~ waha_s + hsc_s + netexports_mexico + diesel + fueloil, temp)
cov1 <- vcovHC(modelo1, type="HC1")
robust_se1 <- sqrt(diag(cov1))

stargazer(modelo1,
          title="MLP and Commodities Prices in Mexico",
          se=list(robust_se1),
          font.size="scriptsize",
          column.sep.width="0pt",
          model.names=F,
          add.lines=list(c("Errores", "Cluster", "Cluster", "Cluster")),
          covariate.labels = c("Waha", "HSC", "Net Exports", "Diesel", "Fueil Oil"),
          dep.var.labels="Marginal Local Prices (MLP)", no.space=TRUE, header=FALSE,
          out=paste(output, "Regresión.txt", sep="/"))





######################################
######## Curva de Oferta Real ########
######################################

temp <- datos
temp$month <- month(temp$date)
temp$year <- year(temp$date)

temp$waha_s <- temp$waha * temp$waha_supply
temp$hsc_s <- temp$hsc * temp$hsc_supply

temp <- temp[which(temp$month == 2 & temp$year == 2021),] # Nos quedamos solo con febrero 2021.

temp$na_count <- apply(temp, 1, function(z) sum(is.na(z)))
temp <- temp[which(temp$na_count == 0),] # Remover filas con NAs.
temp <- subset(temp, select = -c(na_count))  

### (a) Precios contra controles.

partial1 <- lm(zone_price ~ waha_s + hsc_s + netexports_mexico + diesel + fueloil, temp)

temp$res <- residuals(partial1)

### (b) Producción de electricidad contra residuales.

partial2_r <- lm(total_power ~ res, data=temp)

summary(partial2)

### Precio sobre la curva real:

q = 779965.6576
b0 = coef(summary(partial2_r))[1,"Estimate"]
b1 = coef(summary(partial2_r))[2,"Estimate"]
p_real = (q - b0) / b1

### (c) Gráfica de oferta de electricidad.

gr <- ggplot(temp, aes(x=total_power, y=res)) +
  geom_point(colour="dodgerblue4") +
  geom_smooth(method="lm", formula= y ~ x, colour="darkred") +
  ggtitle("Supply of electricity in Mexico") +
  labs(x="Total Power Production", y="Marginal Local Prices")
gr





###############################################
######## Curva de Oferta Contrafactual ########
###############################################

temp$waha_f <- temp$waha_fom * temp$waha_supply
temp$hsc_f <- temp$hsc_fom * temp$hsc_supply

### (a) Precios contra controles.

partial1 <- lm(zone_price ~ waha_f + hsc_f + netexports_mexico + diesel + fueloil, temp)

temp$res_fom <- residuals(partial1)

### (b) Producción de electricidad contra residuales.

partial2_c <- lm(total_power ~ res_fom, data=temp)

summary(partial2)

### Precio sobre la curva contrafactual:

q = 779965.6576
b0 = coef(summary(partial2_c))[1,"Estimate"]
b1 = coef(summary(partial2_c))[2,"Estimate"]
p_cont = (q - b0) / b1

### (c) Gráfica de oferta de electricidad.

gr <- ggplot(temp, aes(x=total_power, y=res)) +
  geom_point(colour="dodgerblue4") +
  geom_smooth(aes(y=res), method="lm", formula= y ~ x, colour="darkred") +
  geom_smooth(aes(y=res_fom), method="lm", formula= y ~ x, colour="green") +
  geom_vline(xintercept = 779965.6576) +
  ggtitle("Supply of electricity in Mexico") +
  labs(x="Total Power Production (MWh)", y="Marginal Local Prices (MXN/MWh)")
gr





#################################
######## Deadweight Loss ########
#################################

### Precio sobre la curva real:

q = 779965.6576
b0 = coef(summary(partial2_r))[1,"Estimate"]
b1 = coef(summary(partial2_r))[2,"Estimate"]
p_real = (q - b0) / b1

### Precio sobre la curva contrafactual:

q = 779965.6576
b0 = coef(summary(partial2_c))[1,"Estimate"]
b1 = coef(summary(partial2_c))[2,"Estimate"]
p_cont = (q - b0) / b1

### Deadweight Loss:

change_p = p_real - p_cont

PBS = q * (change_p)





#######################################
######## Regresión Discontinua ########
#######################################

datos$trat = ifelse(datos$date >= "2021-02-12", 1, 0)

datos$cut <- "2021-02-14"
datos$cut <- ymd(datos$cut)

datos$run_var <- difftime(datos$cut, datos$date , units = c("days"))
datos$run_var <- datos$run_var * (-1)

datos$run_var <- destring(datos$run_var)
datos$zone_price <- destring(datos$zone_price)


### Primer intento:

rdd <- RDestimate(zone_price ~ run_var, data = datos, cutpoint = 0)

plot(rdd)

### Segundo intento:

bw <- with(datos, IKbandwidth(run_var, zone_price, cutpoint = 0))

rdd <- RDestimate(zone_price ~ run_var, data = datos, cutpoint = 0, bw = bw)

summary(rdd)

plot(rdd)



