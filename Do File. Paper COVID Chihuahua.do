
********************************************
*****                                  *****
*****     Investigación Aplicada I     *****
*****   Reporte de casos de COVID-19   *****
*****                                  *****
*****    Alejandro Grimaldi Ferreira   *****
*****                                  *****
********************************************

clear

global input "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\Mexico_data 1"

global graphs "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\avances-estudiantes\Alejandro Grimaldi - Chihuahua\Gráficas"

global tables "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\avances-estudiantes\Alejandro Grimaldi - Chihuahua\Tablas"

global maps "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\avances-estudiantes\Alejandro Grimaldi - Chihuahua\Mapas"

global output "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\avances-estudiantes\Alejandro Grimaldi - Chihuahua\Output"





****************************************
*****                              *****
*****   Descripción de los Datos   *****
*****     27 de agosto de 2020     *****
*****                              *****
****************************************

/*** NOTAS:

¿Qué sí cambia con el tiempo?

- Fecha de defunción.
- Resultado: Positivo o Negativo.

***/

import delimited "$input\_5_octubre\201005COVID19MEXICO.csv", encoding(UTF-8) clear



*********************************
*** Limpieza de Base de Datos ***
*********************************


*** Pacientes con COVID:

gen covid = resultado == 1
label variable covid "COVID-19 confirmado"


*** Pacientes que fallecieron:

gen deceso = fecha_def != "9999-99-99"
label variable deceso "Fecha de defunción registrada"


*** Indicadoras de características observables:

* Sexo *

gen mujer = sexo == 1
label variable mujer "Mujer"

* Lengua Indígena *

gen indigena = habla_lengua_indig == 1
label variable indigena "Habla una lengua indígena"

* Grupos de Edad *

gen menor10 = edad <= 10
gen edad11_20 = edad > 10 & edad <= 20
gen edad21_30 = edad > 20 & edad <= 30
gen edad31_40 = edad > 30 & edad <= 40
gen edad41_50 = edad > 40 & edad <= 50
gen edad51_60 = edad > 50 & edad <= 60
gen edad61_70 = edad > 60 & edad <= 70
gen edad71_80 = edad > 70 & edad <= 80
gen mayor80 = edad > 80

label variable menor10 "Menor a 11"
label variable edad11_20 "Entre 11 y 20"
label variable edad21_30 "Entre 21 y 30"
label variable edad31_40 "Entre 31 y 40"
label variable edad41_50 "Entre 41 y 50"
label variable edad51_60 "Entre 51 y 60"
label variable edad61_70 "Entre 61 y 70"
label variable edad71_80 "Entre 71 y 80"
label variable mayor80 "Mayor a 80"

rename edad cont_edad

* Comorbilidades *

gen obeso = obesidad == 1
label variable obeso "Obesidad"

gen diab = diabetes == 1
label variable diab "Diabetes"

gen fuma = tabaquismo == 1
label variable fuma "Tabaquismo"

gen enfisema = epoc == 1
label variable enfisema "EPOC"

gen asmatico = asma == 1
label variable asmatico "Asma"

gen hipert = hipertension == 1
label variable hipert "Hipertensión"

gen cardio = cardiovascular == 1
label variable cardio "Enfermedades cardiovasculares"

gen renales = renal_cronica == 1
label variable renales "Enfermedad renal crónica"

gen inmu = inmusupr == 1
label variable inmu "Inmunosupresión"

drop obesidad diabetes tabaquismo epoc asma hipertension cardiovascular renal_cronica inmusupr

gen tot_comorb = obeso + diab + fuma + enfisema + asmatico + hipert + cardio + renales + inmu
label variable tot_comorb "Número de comorbilidades"

gen varias_comorb = tot_comorb == 1
label variable varias_comorb "Más de una comorbilidad"

global observables "mujer indigena menor10 edad* mayor80 obeso diab fuma enfisema asmatico hipert cardio renales inmu varias_comorb"



*************************
*** Tablas de Balance ***
*************************


*** Intalación de "Balance Table":

* ssc install balancetable


*** Dummy de Chihuahua:

gen chihuahua = entidad_res == 8


*** Tablas de Balance: Casos entre Chihuahua y resto del país.

balancetable chihuahua $observables using "$tables\Tabla de Balance - Casos de Chihuahua vs Resto del país.xls" if covid == 1, replace varlabels ctitles("Resto del País" "Chihuahua" "Diferencia")


*** Tablas de Balance: Decesos entre Chihuahua y resto del país.

balancetable chihuahua $observables using "$tables\Tabla de Balance - Decesos de Chihuahua vs Resto del país.xls" if covid == 1 & deceso == 1, replace varlabels ctitles("Resto del País" "Chihuahua" "Diferencia")


*** Tablas de Balance: Casos y Decesos por COVID en Chihuahua.

balancetable deceso $observables using "$tables\Tabla de Balance - Casos vs Decesos en Chihuahua.xls" if covid == 1 & chihuahua == 1, replace varlabels ctitles("Casos confirmados" "Decesos confirmados" "Diferencia")


*** Tablas de Balance: Casos y Decesos por COVID en el resto del país.

balancetable deceso $observables using "$tables\Tabla de Balance - Casos vs Decesos en el resto del país.xls" if covid == 1 & chihuahua == 0, replace varlabels ctitles("Casos confirmados" "Decesos confirmados" "Diferencia")



*************************
*** Mapas: Shapefiles ***
*************************


*** Indicadora de deceso por COVID:

gen deceso_covid = covid == 1 & deceso == 1


*** Colapsar los datos por municipio:

collapse (sum) covid deceso_covid, by(entidad_res municipio_res)

drop if municipio_res == 999

gen entidad_mun = entidad_res * 1000 + municipio_res

sort entidad_mun

gen letalidad = deceso_covid / covid

replace letalidad = 0 if letalidad == .

save "$output\Casos y decesos COVID por Municipio.dta", replace


*** Creación del Mapa en STATA:

* Usamos el Shapelife que subió el profesor.

* ssc install spmap
* ssc install shp2dta

cd "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\avances-estudiantes\Alejandro Grimaldi - Chihuahua\Mapas\Shapefiles"

shp2dta using national_municipal, data(munsmex) coor(coordmuns) genid(id) replace


*** Merge de las bases de datos:

use munsmex.dta, clear

destring CVEGEO, gen(entidad_mun) force

sort entidad_mun

merge 1:1 entidad_mun using "$output\Casos y decesos COVID por Municipio.dta"
gen tasa_mort = deceso_covid / POB1 * 100000
keep if _merge == 3
drop _merge


*** Mapa de casos de Chihuahua:

sum covid if entidad_res == 8, detail /* Revisar percentiles */

spmap covid using coordmuns if entidad_res == 8, id(id) clmethod(custom) ///
clbreaks(0 3 7 27 177 1000 6000) ///
legtitle("Casos confirmados") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("Chihuahua")

graph export "$maps\Mapa de Casos - Chihuahua.png", as(png) replace


*** Mapa de casos de México:

spmap covid using coordmuns, id(id) clmethod(custom) ///
clbreaks(0 3 7 27 177 1000 6000 20200) ///
legtitle("Casos confirmados") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("México")

graph export "$maps\Mapa de Casos - Nacional.png", as(png) replace


*** Mapa de decesos de Chihuahua:

sum deceso_covid if entidad_res == 8, detail /* Revisar percentiles */

spmap deceso_covid using coordmuns if entidad_res == 8, id(id) ///
clmethod(custom) clbreaks(0 1 3 15 50 100 500 1000) ///
legtitle("Decesos confirmados") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("Chihuahua")

graph export "$maps\Mapa de Decesos - Chihuahua.png", as(png) replace


*** Mapa de decesos de México:

spmap deceso_covid using coordmuns, id(id) clmethod(custom) ///
clbreaks(0 1 3 15 50 100 500 1000 2250) ///
legtitle("Decesos confirmados") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("México")

graph export "$maps\Mapa de Decesos - Nacional.png", as(png) replace


*** Mapa de letalidad de Chihuahua:

sum letalidad if entidad_res == 8, detail /* Revisar percentiles */

spmap letalidad using coordmuns if entidad_res == 8, id(id) ///
clmethod(custom) clbreaks(0 .10 .30 .50 .80 .90 1) ///
legtitle("Tasa de letalidad") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("Chihuahua")

graph export "$maps\Mapa de Letalidad - Chihuahua.png", as(png) replace


*** Mapa de letalidad de México:

spmap letalidad using coordmuns, id(id) ///
clmethod(custom) clbreaks(0 .10 .30 .50 .80 .90 1) ///
legtitle("Tasa de letalidad") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("México")

graph export "$maps\Mapa de Letalidad - Nacional.png", as(png) replace


*** Mapa de mortalidad de Chihuahua:

sum tasa_mort if entidad_res == 8, detail /* Revisar percentiles */

spmap tasa_mort using coordmuns if entidad_res == 8, id(id) ///
clmethod(custom) clbreaks(0 5 15 25 35 50 200) ///
legtitle("Tasa de mortalidad") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("Chihuahua")

graph export "$maps\Mapa de Mortalidad - Chihuahua.png", as(png) replace


*** Mapa de mortalidad de México:

spmap tasa_mort using coordmuns, id(id) ///
clmethod(custom) clbreaks(0 5 10 20 30 50 200 600) ///
legtitle("Tasa de mortalidad") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("México")

graph export "$maps\Mapa de Mortalidad - Nacional.png", as(png) replace





*****************************************
*****                               *****
*****   Estadísticas Descriptivas   *****
*****   10 de septiembre de 2020    *****
*****                               *****
*****************************************

/*** NOTAS:

Vamos a utilizar la primera fecha de defunción registrada.

***/



*************************************
*** Merge de la Fecha de Registro ***
*************************************


import delimited "$input\_5_octubre\201005COVID19MEXICO.csv", encoding(UTF-8) clear

gen deceso = fecha_def != "9999-99-99"
keep if resultado == 1
keep if deceso == 1

sort id_registro

merge 1:1 id_registro using "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\avances-estudiantes\Alejandro Grimaldi - Chihuahua\Output\reportdate_all.dta"

keep if _merge == 3
drop _merge

gen deaddate = date(fecha_def, "YMD")

gen retraso = reportdate - deaddate
replace retraso = 0 if retraso < 0

gen retraso_30 = retraso
replace retraso_30 = 30 if retraso > 30

label variable retraso_30 "Días de retraso en reporte de decesos"



********************************
*** Histogramas de Restrasos ***
********************************


*** Retrasos entre México y Chihuahua:

twoway (histogram retraso_30 if reportdate > 22025, percent discrete bcolor(midgreen%30)) ///
(histogram retraso_30 if reportdate > 22025 & entidad_res == 8, percent discrete bcolor(navy%40)), ///
ytitle("Porcentaje") legend(order(1 "México" 2 "Chihuahua"))

graph export "$graphs\Histograma de retrasos entre México y Chihuahua.png", as(png) replace


*** Retrasos entre México y Chihuahua (mayores a 30 días):

twoway (histogram retraso if reportdate > 22025 & retraso > 30, percent discrete bcolor(midgreen%30)) ///
(histogram retraso if reportdate > 22025 & entidad_res == 8 & retraso > 30, percent discrete bcolor(navy%40)), ///
ytitle("Porcentaje") legend(order(1 "México" 2 "Chihuahua"))

graph export "$graphs\Histograma de retrasos entre México y Chihuahua (mayores a 30 días).png", as(png) replace


*** Retrasos por Institución:

twoway (kdensity retraso_30 if reportdate > 22025 & entidad_res == 8 & sector == 4, bw(1) k(gau) color(red)) ///
(kdensity retraso_30 if reportdate > 22025 & entidad_res == 8 & sector == 6, bw(1) k(gau) color(blue)) ///
(kdensity retraso_30 if reportdate > 22025 & entidad_res == 8 & sector == 12, bw(1) k(gau) color(green)) ///
(kdensity retraso_30 if reportdate > 22025 & entidad_res == 8 & sector == 9, bw(1) k(gau) color(grey)), ///
ytitle("Densidad") xtitle("Días de retraso en reporte de decesos") legend(order(1 "IMSS" 2 "ISSSTE" 3 "SSA" 4 "Privado"))

graph export "$graphs\Densidad de retrasos por institución.png", as(png) replace


*** Retrasos entre Hospitalizados y Ambulatorios:

twoway (histogram retraso_30 if reportdate > 22025 & tipo_paciente == 1 & entidad_res == 8, percent discrete bcolor(midgreen%30)) ///
(histogram retraso_30 if reportdate > 22025 & tipo_paciente == 2 & entidad_res == 8, percent discrete bcolor(navy%40)), ///
ytitle("Porcentaje") legend(order(1 "Ambulatorios" 2 "Hospitalizados"))

graph export "$graphs\Histograma de retrasos por tipo de paciente.png", as(png) replace


*** Retrasos entre Indígenas y No Indígenas:

twoway (histogram retraso_30 if reportdate > 22025 & habla_lengua_indig == 1 & entidad_res == 8, percent discrete bcolor(midgreen%30)) ///
(histogram retraso_30 if reportdate > 22025 & habla_lengua_indig == 2 & entidad_res == 8, percent discrete bcolor(navy%40)), ///
ytitle("Porcentaje") legend(order(1 "Habla lengua indígena" 2 "No habla lengua indígena"))

graph export "$graphs\Histograma de retrasos por hablar lengua indígena.png", as(png) replace


*** Retrasos por Comorbilidades:

twoway (histogram retraso_30 if reportdate > 22025 & epoc == 1 & entidad_res == 8, percent discrete bcolor(midgreen%30)) ///
(histogram retraso_30 if reportdate > 22025 & epoc == 2 & entidad_res == 8, percent discrete bcolor(navy%40)), ///
ytitle("Porcentaje") legend(order(1 "EPOC: Sí" 2 "EPOC: No"))

graph export "$graphs\Histograma de retrasos por EPOC.png", as(png) replace

twoway (histogram retraso_30 if reportdate > 22025 & asma == 1 & entidad_res == 8, percent discrete bcolor(midgreen%30)) ///
(histogram retraso_30 if reportdate > 22025 & asma == 2 & entidad_res == 8, percent discrete bcolor(navy%40)), ///
ytitle("Porcentaje") legend(order(1 "Asma: Sí" 2 "Asma: No"))

graph export "$graphs\Histograma de retrasos por Asma.png", as(png) replace



**************************************
*** Retrasos Promedio por Municipo ***
**************************************

preserve

collapse (mean) retraso retraso_30, by(entidad_res municipio_res)

gen entidad_mun = entidad_res * 1000 + municipio_res

keep entidad_mun retraso retraso_30

sort entidad_mun

save "$output\Retrasos promedio por municipio.dta", replace

restore



***********************************************
*** Decesos Ocurridos por Fecha y Municipio ***
***********************************************

preserve

gen occ_deaths = 1

gen occ_deaths_chih = entidad_res == 8

collapse (sum) occ_deaths occ_deaths_chih, by(deaddate)

keep deaddate occ_deaths occ_deaths_chih

label variable occ_deaths "Decesos por fecha de ocurrencia (México)"

label variable occ_deaths_chih "Decesos por fecha de ocurrencia (Chihuahua)"

rename deaddate date

sort date

save "$output\Decesos ocurridos por fecha y municipio.dta", replace

restore



***********************************************
*** Decesos Registrados por Fecha y Municipio ***
***********************************************

preserve

gen rep_deaths = 1

gen rep_deaths_chih = entidad_res == 8

collapse (sum) rep_deaths rep_deaths_chih, by(reportdate)

keep reportdate rep_deaths rep_deaths_chih

label variable rep_deaths "Decesos por fecha de registro (México)"

label variable rep_deaths_chih "Decesos por fecha de registro (Chihuahua)"

rename reportdate date

sort date

save "$output\Decesos registrados por fecha y municipio.dta", replace

restore



***************************************
*** Mapas de Retrasos por Municipio ***
***************************************


*** Creación del Mapa en STATA:

* Usamos el Shapelife que subió el profesor.

* ssc install spmap
* ssc install shp2dta

cd "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\avances-estudiantes\Alejandro Grimaldi - Chihuahua\Mapas\Shapefiles"

shp2dta using national_municipal, data(munsmex) coor(coordmuns) genid(id) replace


*** Merge de las bases de datos:

use munsmex.dta, clear

destring CVEGEO, gen(entidad_mun) force

sort entidad_mun

merge 1:1 entidad_mun using "$output\Retrasos promedio por Municipio.dta"
keep if _merge == 3
drop _merge


*** Mapa de retrasos en Chihuahua:

sum retraso if NOM_ENT == "Chihuahua", detail /* Revisar percentiles */

spmap retraso using coordmuns if NOM_ENT == "Chihuahua", id(id) clmethod(custom) ///
clbreaks(0 3 10 35 50) ///
legtitle("Retraso promedio de decesos") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin) ///
title("Chihuahua")

graph export "$maps\Mapa de Retrasos - Chihuahua.png", as(png) replace


*** Mapa de retrasos en México:

sum retraso, detail

spmap retraso using coordmuns, id(id) clmethod(custom) ///
clbreaks(0 3 10 35 50 100) ///
legtitle("Retraso promedio de decesos") legend(size(vsmall)) legorder(lohi) legend(position(2)) ///
osize(vvthin vvthin vvthin vvthin vvthin vvthin) ///
title("México")

graph export "$maps\Mapa de Retrasos - Nacional.png", as(png) replace



*************************
*** Curvas Epidémicas ***
*************************


*** Merge de las bases de datos:

use "$output\Decesos ocurridos por fecha y municipio.dta", clear

merge 1:1 date using "$output\Decesos registrados por fecha y municipio.dta"

keep if _merge == 3
drop _merge

sort date

for varlist occ* rep*: replace X = 0 if X == .

format date %tdnn/dd/YY


*** Curvas epidémicas en México:

twoway (bar occ_deaths date, color(midgreen%30)) (bar rep_deaths date, color(navy%40)) if date>22024, ///
title(México) xtitle(Fecha)  legend(order( 1 "Ocurrencia de decesos" 2 "Registro de decesos"))

graph export "$graphs\Curvas epidémicas - México.png", as(png) replace


*** Curvas epidémicas en México:

twoway (bar occ_deaths_chih date, color(midgreen%30)) (bar rep_deaths_chih date, color(navy%40)) if date>22024, ///
title(Chihuahua) xtitle(Fecha)  legend(order( 1 "Ocurrencia de decesos" 2 "Registro de decesos"))

graph export "$graphs\Curvas epidémicas - Chihuahua.png", as(png) replace





******************************************
*****                                *****
*****   Correlaciones con Retrasos   *****
*****         Octubre de 2020        *****
*****                                *****
******************************************



*************************************
*** Merge de la Fecha de Registro ***
*************************************


import delimited "$input\_8_septiembre\200908COVID19MEXICO.csv", encoding(UTF-8) clear

gen deceso = fecha_def != "9999-99-99"
keep if resultado == 1
keep if deceso == 1

sort id_registro

merge 1:1 id_registro using "C:\Users\ale_g\INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\EMILIO GUTIERREZ FERNANDEZ - Inv_Aplicada_2020_2\data_stata\reportdate_all.dta"

keep if _merge == 3
drop _merge

gen deaddate = date(fecha_def, "YMD")

gen retraso = reportdate - deaddate
replace retraso = 0 if retraso < 0

gen retraso_30 = retraso
replace retraso_30 = 30 if retraso > 30

label variable retraso_30 "Días de retraso en reporte de decesos"



************************************
*** Limpieza de la Base de Datos ***
************************************


*** Indicadoras de características observables:

* Sexo *

gen mujer = sexo == 1
label variable mujer "Mujer"

* Lengua Indígena *

gen indigena = habla_lengua_indig == 1
label variable indigena "Habla una lengua indígena"

* Grupos de Edad *

gen menor30 = edad <= 30
gen mayor60 = edad >= 60

label variable menor30 "Menor a 31 años"
label variable mayor60 "Mayor a 59 años"

* Comorbilidades *

gen obeso = obesidad == 1
label variable obeso "Obesidad"

gen diab = diabetes == 1
label variable diab "Diabetes"

gen fuma = tabaquismo == 1
label variable fuma "Tabaquismo"

gen enfisema = epoc == 1
label variable enfisema "EPOC"

gen asmatico = asma == 1
label variable asmatico "Asma"

gen hipert = hipertension == 1
label variable hipert "Hipertensión"

gen cardio = cardiovascular == 1
label variable cardio "Enfermedades cardiovasculares"

gen renales = renal_cronica == 1
label variable renales "Enfermedad renal crónica"

gen inmu = inmusupr == 1
label variable inmu "Inmunosupresión"

drop obesidad diabetes tabaquismo epoc asma hipertension cardiovascular renal_cronica inmusupr

gen tot_comorb = obeso + diab + fuma + enfisema + asmatico + hipert + cardio + renales + inmu
label variable tot_comorb "Número de comorbilidades"

gen varias_comorb = tot_comorb == 1
label variable varias_comorb "Más de una comorbilidad"

global observables "mujer indigena menor30 mayor60 obeso diab fuma enfisema asmatico hipert cardio renales inmu"



************************************************
*** Variables para diferencias en Modelo SIR ***
************************************************

reg retraso $observables if entidad_res == 8, robust

outreg2 using "$tables\Regresión - Retrasos y Variables para Modelo SIR.doc", replace label




