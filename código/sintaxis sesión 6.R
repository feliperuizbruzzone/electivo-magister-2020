# ELECTIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE.
# MAGÍSTER EN CCSS UNIVERSIDAD DE CHILE - 2020 

# Mandar paquetes a instalar previamente
# Clase y diapos: contenidos; documentación tidyverse; PSPP; CEP y taller datos propios

# ---- 0. PAQUETES A INSTALAR ---

install.packages(c("readxl", "tidyverse", "haven", "car"))

# ---- 1. IMPORTACIÓN DE DATOS A R, DESDE DIFERENTES FORMATOS ----

# Primero que todo, definir carpeta de trabajo
#Botones: Session -> Set Working Directory -> Choose directory -> Elegir carpeta
#Abreviación de botones: Ctrl + Shift + H

#Escogemos la carpeta donde tengamos nuestros archivos (sintaxis, base de datos a usar)
setwd("~/Dropbox/Felipe/Docencia/Universidad de Chile/Magister CCSS UCH/C. Electivo Metodología/clases/clase 6")

# Si estamos trabajando con un proyecto, esto está pre-definido por la ubicación del proyecto

#VEREMOS COMO IMPORTAR BASES DE DATOS DESDE 4 TIPOS DE DATOS: CSV, EXCEL, SPSS, STATA

#Ver archivo paraguay.xlsx, entender estructura de libro de datos.

#Configurar hoja correspondiente a base de datos de encuesta en archivo "paraguay" en Excel a archivo CSV 
#Esto lo hacemos desde explorador de archivos en "guardar como"

#Observar estructura interna (abrir CSV con bloc de notas)
#Notación latinoamericana (, decimales - ; separador)
#Notación EEUU/EUROPA (. decimales - , separador)

# A. Abrir archivo paraguay desde CSV (ojo con diferencias Linux/Windows)
#NOTACIÓN EEUU: decimales como punto y no coma
paraguay_csv <- read.csv("paraguay.csv")
View(paraguay_csv) # Base no se lee correctamente, no se separan bien columnas

#¿Qué pasa si usamos una función adecuada a notación latina?
#lee comas como decimales y punto y comas como separador de variables
paraguay_csv2 <- read.csv2("paraguay.csv")
View(paraguay_csv2) 
# podemos eliminar manualmente primera fila para leer correctamente nombre variable

# B. Abrir archivo paraguay desde excel.
library(readxl) 

paraguay_excel <- read_excel("paraguay.xlsx") 

head(paraguay_excel) 

#¿Cuál es el problema?

#Uso de argumentos en función read_excel

paraguay_excel <- read_excel("paraguay.xlsx", sheet = 2) # posición de la hoja

paraguay_excel <- read_excel("paraguay.xlsx", sheet = "respuestas") # nombre hoja

paraguay_excel <- read_excel("paraguay.xlsx", sheet = "respuestas", skip = 1) #saltar fila de preguntas del cuestionario

#Es preciso indicar el nombre o posición de la hoja y desde qué fila leer datos. 
#Por defecto la función lee como nombre de variable la primera fila que lee.

# Limpiar entorno de trabajo

# C. Abrir base de datos desde STATA y SPSS

# Paquete que incluye funciones para leer bases de datos desde otros formatos
library(haven)

# Para leer desde SPSS
CEP_dic19_spss <- read_sav("CEP_dic2019.sav")

#Para leer desde Stata
CEP_dic19_stata <- read_dta("CEP_dic2019.dta")

# Visualizar encuesta CEP en PSPP

# Eliminar base stata manualmente desde entorno

# ---- 2. RECODIFICACIÓN Y TRANSFORMACIÓN DE VARIABLES ----

# Análisis global de los datos
class(CEP_dic19_spss) # Tipo de objeto
dim(CEP_dic19_spss) # Dimensiones de la matriz
names(CEP_dic19_spss) # Nombres de las variables

# Selección de variables de interés 
library(dplyr) #Cargar paquete'dplyr'

# Usaremos menos en esta sesión, todas son para ejemplos posteriores

# Sexo: DS_P1
# Edad: DS_P2_EXACTA (intervalar)
# Tramo ingreso individual: DS_P29 (ORDINAL)
# identificación/simpatía partidos (nominal): MB_P8
# posición política (nominal): MB_P9 
# Apoyo o rechazo octubre 19: ESP_32
# Frecuencia acuerdo acción directa como forma de manifestación
# ESP_41_1 a ESP_41_5
# Justificación represión
# ESP_42_1 a ESP_42_3 (Carabineros), 
#ESP_43 (violación DDH carabineros, invertir), ESP_44 (violación DDHH FFAA, invertir)

CEP <- select(CEP_dic19_spss, DS_P1, DS_P2_EXACTA,MB_P8,MB_P9,
              ESP_32, ESP_41_1, ESP_41_2,ESP_41_3,ESP_41_4,ESP_41_5,
              ESP_42_1, ESP_42_2, ESP_42_3, ESP_43, ESP_44)

#Renombraremos sólo algunas variables
CEP <- rename(CEP, sexo = DS_P1, O19 = ESP_32, edad = DS_P2_EXACTA)

# RECODIFICACIÓN

# Sexo (nominal)
#---------------
table(CEP$sexo) #1 = hombre, 2 = mujer
class(CEP$sexo)
CEP$sexo <- as.numeric(CEP$sexo) #convertir a vector numérico para poder transformar

CEP <- mutate(CEP, sexorec = recode(CEP$sexo, "1" = "hombre", "2" = "mujer"))
class(CEP$sexorec) #queda como objeto character
CEP$sexorec #visualizar datos concretos guardados
table(CEP$sexorec)

# Apoyo o rechazo octubre 19 (ordinal)
#------------------------------------
#1. Apoyo / 2. Apoyo parcial / 3. Neutro / 4. Rechazo parcial / 5. Rechazo
# Recodificar en apoyo, neutro y rechazo
table(CEP$O19)
class(CEP$O19)
CEP$O19 <- as.numeric(CEP$O19)

#Especificar paquete desde el cual queremos ejecutar la función 'recode' 
#para poder recodificar según tramos
CEP <- mutate(CEP, O19_rec = car::recode(CEP$O19, "1:2 = 1; 3 = 2; 
                                         4:5 = 3; else = NA"))
table(CEP$O19_rec)

#Convertir a factor para poner etiquetas
CEP$O19_rec <- factor(CEP$O19_rec, labels= c("Apruebo", "Neutro", "Rechazo"))
table(CEP$O19_rec)

# edad (intervalar)
#------------------

#Recodificar en números asociados a rangos. 18-29; 30-49; 50-69; 70 o más.
summary(CEP$edad)
class(CEP$edad)
CEP$edad <- as.numeric(CEP$edad)

#Especificar paquete desde el cual queremos ejecutar la función 'recode' 
#para poder recodificar según tramos
CEP <- mutate(CEP, edad_rango = car::recode(CEP$edad, "18:29 = 1;30:49 = 2;
                                            50:69 =3; else = 4"))
table(CEP$edad_rango)

#Convertir a factor para poner etiquetas
CEP$edad_rango <- factor(CEP$edad_rango, labels= c("18-29", "30-49", "50-69", "70+"))
table(CEP$edad_rango)

# ---- 3. BREVE ANÁLISIS ----

table(CEP$sexorec, CEP$O19_rec)

prop.table(table(CEP$sexorec, CEP$O19_rec))*100

prop.table(table(CEP$sexorec, CEP$O19_rec),1)*100

table(CEP$edad_rango, CEP$O19_rec)

prop.table(table(CEP$edad_rango, CEP$O19_rec))*100
prop.table(table(CEP$edad_rango, CEP$O19_rec),1)*100

# Guardar base de datos con selección de variables y variables recodificadas

save(CEP, file = "CEP_dic19_seleccion.RData")

# ---- 4. TALLER PRÁCTICO: CARGAR DATOS PROPIOS, SELECCIONAR VARIABLES A UTILIAR ----

# Instalar paquetes necesarios

# Cargar paquetes a utilizar en sesión de trabajo

# Definir carpeta de trabajo

# Leer base de datos y guardarla como objeto R

# Crear nueva base con subconjunto de variables específico a analizar

# Guardar sintaxis y base de datos en formato R


save(datos, file = "base_electivo.RData") # ¿Dónde queda guardada?