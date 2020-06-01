# ELECTIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE.
# MAGÍSTER EN CCSS UNIVERSIDAD DE CHILE - 2020 

# Actualizar o crear proyecto github
# https://github.com/feliperuizbruzzone/electivo-magister-2020

# ---- 1. CARGAR BASE DE DATOS A RSTUDIO Y PREPARAR VARIABLES (CLASE 6  ) ----

# Paquete que incluye funciones para leer bases de datos desde otros formatos
library(haven)

# Para leer desde SPSS
CEP_dic19_spss <- read_sav("datos/CEP_dic2019.sav")

#Seleccionar variables de interés
library(dplyr)
CEP_analisis <- select(CEP_dic19_spss, pond = PONDERADOR, region = REGION,
                       sexo = DS_P1, edad = DS_P2_EXACTA, O19 = ESP_32)

# RECODIFICACIÓN

# Sexo (nominal)

table(CEP_analisis$sexo) #1 = hombre, 2 = mujer
class(CEP_analisis$sexo)
CEP_analisis$sexo <- as.numeric(CEP_analisis$sexo) #convertir a vector numérico para poder transformar

CEP_analisis <- mutate(CEP_analisis, sexorec = recode(CEP_analisis$sexo, "1" = "hombre", "2" = "mujer"))
class(CEP_analisis$sexorec) #queda como objeto character
CEP_analisis$sexorec #visualizar datos concretos guardados
table(CEP_analisis$sexorec)

# Apoyo o rechazo octubre 19 (ordinal)

#1. Apoyo / 2. Apoyo parcial / 3. Neutro / 4. Rechazo parcial / 5. Rechazo
# Recodificar en apoyo, neutro y rechazo
table(CEP_analisis$O19)
class(CEP_analisis$O19)
CEP_analisis$O19 <- as.numeric(CEP_analisis$O19)

#Especificar paquete desde el cual queremos ejecutar la función 'recode' 
#para poder recodificar según tramos
CEP_analisis <- mutate(CEP_analisis, O19_rec = car::recode(CEP_analisis$O19, "1:2 = 1; 3 = 2; 
                                         4:5 = 3; else = NA"))
table(CEP_analisis$O19_rec)

#Convertir a factor para poner etiquetas
CEP_analisis$O19_rec <- factor(CEP_analisis$O19_rec, labels= c("Apruebo", "Neutro", "Rechazo"))
table(CEP_analisis$O19_rec)

# edad (intervalar)

#Recodificar en números asociados a rangos. 18-29; 30-49; 50-69; 70 o más.
summary(CEP_analisis$edad)
class(CEP_analisis$edad)
CEP_analisis$edad <- as.numeric(CEP_analisis$edad)

#Especificar paquete desde el cual queremos ejecutar la función 'recode' 
#para poder recodificar según tramos
CEP_analisis <- mutate(CEP_analisis, edad_rango = car::recode(CEP_analisis$edad, "18:29 = 1;30:49 = 2;
                                            50:69 =3; else = 4"))
table(CEP_analisis$edad_rango)

#Convertir a factor para poner etiquetas
CEP_analisis$edad_rango <- factor(CEP_analisis$edad_rango, labels= c("18-29", "30-49", "50-69", "70+"))
table(CEP_analisis$edad_rango)

saveRDS(CEP_analisis, file = "datos/CEP_dic2019_seleccion.RDS")

# ---- 2. CONSTRUIR RESULTADOS DE NIVEL MUESTRAL ----

library(summarytools)
# Tabla de frecuencias simple y de %
table(CEP_analisis$O19_rec)
table(CEP_analisis$sexorec, CEP_analisis$O19_rec)
prop.table(table(CEP_analisis$sexorec, CEP_analisis$O19_rec),1)*100

# RESULTADO 1-1: frecuencias sin ponderar con "summarytools"
# Posición sobre manifestaciones O-19
freq(CEP_analisis$O19_rec)

# RESULTADO 1-2: frecuencias ponderadas con "summarytools" (ojo como cambian datos)
# Posición sobre manifestaciones O-19
freq(CEP_analisis$O19_rec, weights = CEP_analisis$pond)

# RESULTADO 2-1:  tabla de doble entrada con "summarytools", sin ponderar
# Posición sobre manifestaciones O-19 según sexo, perfil columna
ctable(CEP_analisis$sexorec, CEP_analisis$O19_rec, prop = "c")

# RESULTADO 2-2:  tabla de doble entrada con "summarytools", ponderada
# Posición sobre manifestaciones O-19 según sexo, perfil columna
ctable(CEP_analisis$sexorec, CEP_analisis$O19_rec, prop = "c",
       weights = CEP_analisis$pond)

# Estadísticos descriptivos variable edad
summary(CEP_analisis$edad)

# RESULTADO 3-1: estadísticos de resumen con "summarytools"
# Estadísticos de resumen variable edad
descr(CEP_analisis$edad, transpose = T)
descr(CEP_analisis$edad, transpose = T, 
      stats = c("min","q1","med","mean","q3","sd","iqr","cv","n.valid"))

# RESULTADO 3-2: estadísticos de resumen con "summarytools", ponderados
# Estadísticos de resumen variable edad
# No aguanta todos (cuartiles y derivados por ejemplo)
descr(CEP_analisis$edad, transpose = T, 
      stats = "common", weights = CEP_analisis$pond)


# RESULTADO 4-1: estadísticos resumen según una segunda variable
# Estadísticos de resumen variable edad según posición manifestaciones 0-19, sin ponderar
with(CEP_analisis, stby(data= edad, INDICES = O19_rec,
                        FUN = descr, 
                        stats = c("min","q1","med","mean","q3","sd","iqr","cv","n.valid"),
                        transpose = T))

# RESULTADO 4-2: estadísticos resumen según una segunda variable, ponderados
# Estadísticos de resumen variable edad según posición manifestaciones 0-19, sin ponderar
with(CEP_analisis, stby(data= edad, INDICES = O19_rec,
                        FUN = descr, 
                        stats = "common",
                        transpose = T))



# ---- 3. CONSTRUIR RESULTADOS DE NIVEL POBLACIONAL ----

# ¿Cómo aplico el diseño muestral complejo sobre una base de datos (aka. "ponderar")?
# Este paquete tiene la ventaja de usar toda la información disponible para definir diseño muestral
# Además, entrega errores estándar de estimaciones

library(survey)
CEP_ponderada <- svydesign(data = CEP_analisis, id=~0, strata = NULL, weights = ~pond)

# ¿Como verifico que diseño muestral esté correctamente definido?
prop.table(table(CEP_analisis$region)) # sin ponderar
freq(CEP_analisis$region, weights = CEP_analisis$pond) # con ponderador (summarytools)
svytotal(~factor(region), CEP_ponderada, na.rm=T) # con diseño muestral complejo (survey)

# RESULTADO 1-2: frecuencias ponderadas (survey)
# Posición sobre manifestaciones O-19
svytotal(~O19_rec, CEP_ponderada, na.rm = T)

# RESULTADO 2-2:  tabla de doble entrada, ponderada (survey)
# Posición sobre manifestaciones O-19 según sexo
svyby(~O19_rec, ~sexorec, CEP_ponderada, svytotal, na.rm=T)

# RESULTADO 3-2: estadísticos de resumen ponderados (survey)
# Estadísticos de resumen variable edad
svymean(~edad, CEP_ponderada, na.rm = T)
svyvar(~edad, CEP_ponderada, na.rm = T)
svyquantile(~edad, CEP_ponderada, c(.25, .5, .75), ci=T, na.rm = T)

# RESULTADO 4-2: estadísticos resumen según una segunda variable
# Estadísticos de resumen variable edad según posición manifestaciones 0-19, sin ponderar
svyby(~edad, ~O19_rec, CEP_ponderada, svymean, na.rm = T)

