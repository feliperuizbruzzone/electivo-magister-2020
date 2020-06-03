# ELECTIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE.
# MAGÍSTER EN CCSS UNIVERSIDAD DE CHILE - 2020 

# Actualizar o crear proyecto github
# https://github.com/feliperuizbruzzone/electivo-magister-2020

# ---- 1. CARGAR DATOS Y PAQUETES PARA ANÁLISIS (CLASE 6 Y 9) ----

CEP <- readRDS("datos/CEP_dic2019_seleccion.RDS")

install.packages(c("summarytools", "tidyverse"))

library(summarytools)
library(ggplot2)
library(dplyr)

# ---- 2. EXPORTAR TABLAS A CONSOLIDADO DE RESULTADOS ----

# RESULTADO 1: frecuencias ponderadas con "summarytools" (ojo como cambian datos)
# Posición sobre manifestaciones O-19
freq(CEP$O19_rec, weights = CEP$pond)

# RESULTADO 2:  tabla de doble entrada con "summarytools", ponderada
# Posición sobre manifestaciones O-19 según sexo, perfil columna
ctable(CEP$sexorec, CEP$O19_rec, prop = "c",
       weights = CEP$pond)

# RESULTADO 3: estadísticos de resumen con "summarytools", ponderados
# Estadísticos de resumen variable edad
descr(CEP$edad, transpose = T, 
      stats = "common", weights = CEP$pond)

# RESULTADO 4: estadísticos resumen según una segunda variable, ponderados
# Estadísticos de resumen variable edad según posición manifestaciones 0-19, sin ponderar
with(CEP, stby(data= edad, INDICES = O19_rec,
                        FUN = descr, 
                        stats = "common", weights = CEP$pond,
                        transpose = T))

# ¿Cómo podemos extraer estos resultados en un formato reproducible y que a la vez interactúe con otros software?
# Omitiremos por principio el expulsar resultados a planillas
# Con estos cuatro resultados podemos usar el código para construir un reporte reproducible con RMarkdown.

# ---- 3. CONSTRUIR GRÁFICOS ----

# Gráfico de barras básico
plot(CEP$O19_rec)
# Gráfico de barras básico, con algo de formato
plot(CEP$O19_rec, ylab = "Frecuencia", xlab = "Posición frente a manifestaciones",
     main = "Gráfico de barras")

# Histograma básico
hist(CEP$edad)
# Histograma básico, con algo de formato
hist(CEP$edad, ylab = "Frecuencia", xlab = "Edad",
     main = "Histograma")

# Gráfico de barras simple con ggplot (¿qué cosas podemos mejorar?)
ggplot(CEP, aes(x = O19_rec)) +
  geom_bar()

# Gráfico barras frecuencia relativa (%), posición frente a manifestaciones
# Incluye ediciones de formato y excluye NA's
CEP %>% 
  filter(!is.na(O19_rec)) %>% 
  ggplot()+
  geom_bar(mapping = aes(x=O19_rec, y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent, "%") +
  scale_x_discrete("Posición frente a manifestaciones")

# Gráfico de barras para grupos y colores según 3a variable (ir describiendo que efectúa cada línea)
CEP %>% 
  filter(!is.na(O19_rec)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = O19_rec, y = (..count..)/sum(..count..), fill = factor(sexorec)),
           position = "dodge") + # Probar sacando "position"
  scale_y_continuous(labels=scales::percent) +
  xlab("Opinión respecto a movilizaciones O19") +
  scale_fill_manual("Género",
                    values = c("#E41A1C", "#377EB8"),
                    labels = c("Hombre", "Mujer")) +
  ylab("Porcentaje")

# Histograma simple con ggplot
ggplot(CEP, aes(x = edad)) +
  geom_histogram()

# Histograma Edad según grupos (incluye NA's como grupo)
ggplot(CEP, aes(x = edad)) + 
  geom_histogram() +
  facet_grid(~O19_rec)

# Histograma Edad según grupos (excluye NA's del análisis)
CEP %>% 
  filter(!is.na(O19_rec)) %>% 
  ggplot()+
  geom_histogram(mapping = aes(x=edad)) +
  xlab("Edad") +
  ylab("Frecuencia") +
  ggtitle("Edad según posición respecto a manifestaciones") +
  facet_grid(~O19_rec)


# ---- 4. EJEMPLO SOBRE COMO UTILIZAR MANUAL PARA APRENDER SOBRE USO DE MAPAS -----
# https://arcruz0.github.io/libroadp/mapas.html