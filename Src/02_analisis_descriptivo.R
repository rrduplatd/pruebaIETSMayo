# Encabezado --------------------------------------------------------------
# R Versions: R version 4.3.2
# Platform:(64-bit)
# Running under: Windows 10 x64 (build 17763)
#
#
# Description: Analisis Descriptivo de las tablas individuales
#
# Inputs: 1. datosPreprocesados.Rdata
#
# Outputs: .Rdata
#
# File history:
#   2025/05/18: Creation
#
# Autor: Ricardo Duplat
#

# ELIMINAR ARCHIVOS DE ESPACIO DE TRABAJO PARA REPROCESAR
rm(list=ls())

# definir carpeta de trabajo si es necesario
setwd("C:/Users/rrdup/Documents/Trabajo/IETS/Src")

#################################################################################
#   librerias - directorios - Funciones
#################################################################################

inPath <- file.path("..","Input")
outPath <- file.path("..","Output")
srcPath <- file.path("..","Src")

# Libraries ------------------------------

# instalando librerias necesarias SOLO UNA VEZ
# install.packages(c("readxl", "dplyr", "ggplot2", "sf", 
#     "rnaturalearth", "rnaturalearthdata", "devtools"))
#
# instalar github para mapa de calor
# devtools::install_github("ropensci/rnaturalearthhires")


library(readxl)
library(dplyr)
#library(stringr)
#library(janitor)
library(data.table)
library(ggplot2)
library(gridExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(officer)

# Cargar funciones 
source(file.path(srcPath, "Functions" ,"gg_geoPlot.R"))
source(file.path(srcPath, "Functions" ,"descriptive.R"))

# Cargar Rdata producto de 01_preprocesamiento
load(file.path(outPath, "datosPreprocesados.Rdata"))

##################################################################################
# Analisis Descriptivo
##################################################################################

# Distribución geografica -------------------------------------------

# Obtener mapa de Colombia a nivel departamental
colombia <- ne_states(country = "Colombia", returnclass = "sf") %>%
  mutate(name_upper = toupper(name))


# Sedes 
# Mapa de calor para Sedes por departamento
ggDeptoAgr(sedDat)
# Mapa de calor para Sedes por departamento, para comparar
# municipios PDET con NO PDET
ggDeptoAgr(sedDat, aux = "municipio_pdet")
#mapa de calor solo SI PDET
ggDeptoAgr(sedDat[ municipio_pdet == "SI"])

# CapacidadInstalada
# identificar posibles combinaciones para dibujar mapa de calor 
capInsDat[, table(coca_nombre, grupo_capacidad )]
# identificar valores de otra columna (Naturaleza) para graficos desagregados
capInsDat[, table(naju_nombre)]

ggDeptoSerEl(df = capInsDat, coca_nam = "Pediátrica" , grupo_cap = "CAMAS")
ggDeptoSerEl(df = capInsDat, coca_nam = "Consulta Externa" , grupo_cap = "CONSULTORIOS")

# realizar el grafico con los 3 tipos de naturaleza 
gg1 = ggDeptoSerEl(df = capInsDat[naju_nombre == "Pública"], 
        coca_nam = "Consulta Externa" , grupo_cap = "CONSULTORIOS", 
        desagre = "- Pública")

gg2 = ggDeptoSerEl(df = capInsDat[naju_nombre == "Privada"], 
        coca_nam = "Consulta Externa" , grupo_cap = "CONSULTORIOS",
        desagre = "- Privada")

gg3 = ggDeptoSerEl(df = capInsDat[naju_nombre == "Mixta"], 
        coca_nam = "Consulta Externa" , grupo_cap = "CONSULTORIOS",
        desagre = "- Mixta")

grid.arrange(gg1, gg2, gg3, ncol = 1)

################################################

# Estadisticas descriptivas -----------------------------------


#Arreglar columnas de identificador de municipio y departamento
munDat[ , dp := sprintf("%02d", dp) ] 
munDat[ , mpio := sprintf("%05d", mpio) ]

#Crear columnas de identificador de municipio y departamento 
# a partir de codigo_prestador
sedDat[ , dp := substr(codigo_prestador,1,2)]
sedDat[ , mpio := substr(codigo_prestador,1,5)]

# unir las 2 tablas
sedDatAll <- merge( sedDat, munDat[, .(dp, mpio, superficie, pop_tot,
  rural, region)], by= c("dp" , "mpio"))

# El equivalente a la anterior linea en SQL es:
#SELECT 
#  sed.*,
#  mun.superficie,
#  mun.pop_tot,
#  mun.rural,
#  mun.region
#FROM sedDat AS sed
#INNER JOIN (
#  SELECT dp, mpio, superficie, pop_tot, rural, region
#  FROM munDat
#) AS mun
#ON sed.dp = mun.dp AND sed.mpio = mun.mpio;

# Agrupar y calcular número de sedes por departamento
sedes_por_depto <- sedDatAll %>%
  group_by(departamento) %>%
  summarise(n_Sedes = n())

# Crear histograma de n_Sedes
ggplot(sedes_por_depto, aes(x = n_Sedes)) +
  geom_histogram( fill = "steelblue", color = "white") +
  labs(title = "Distribución de número de sedes por departamento",
       x = "Número de sedes",
       y = "Número de departamentos") +
  theme_minimal()

# ejecutar función que crea resumen descriptivo y grafico por departamento
prueba  = ResumenSedDeo(sedDatAll, depart_input = "Cundinamarca")
prueba$ggViolin
prueba$ggCajas

# Ejecutar función que crea presentación ppt por departamento de interes
crearPptSedes(sedDatAll, "Cundinamarca", salida = file.path(outPath, "Cundinamarca_sedes.pptx"))
crearPptSedes(sedDatAll, "Magdalena", salida = file.path(outPath, "Magdalena_sedes.pptx"))
crearPptSedes(sedDatAll, "Vichada", salida = file.path(outPath, "Vichada_sedes.pptx"))

