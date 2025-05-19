# Encabezado --------------------------------------------------------------
# R Versions: R version 4.3.2
# Platform:(64-bit)
# Running under: Windows 10 x64 (build 17763)
#
#
# Description: Cargue y preprocesamiento de imputs
#
# Inputs: 1. Municipios.xslx
#         2. Tablas descargadas y previamente ajustadas de 
#            https://prestadores.minsalud.gov.co/habilitacion/
#
# Outputs: datosPreprocesados.Rdata
#
# File history:
#   2025/05/16: Creation
#
# Autor: Ricardo Duplat
#

# ELIMINAR ARCHIVOS DE ESPACIO DE TRABAJO PARA REPROCESAR
rm(list=ls())

# definir carpeta de trabajo si es necesario
#setwd("C:/Users/rrdup/Documents/Trabajo/IETS/Src")

#################################################################################
#   librerias - directorios - Funciones
#################################################################################

inPath <- file.path("..","Input")
outPath <- file.path("..","Output")
srcPath <- file.path("..","Src")

# Libraries ------------------------------
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(data.table)

# Definiciones Globales ------------------------------

muniPath    <- file.path(inPath, "Municipios.xlsx")
sedePath    <- file.path(inPath, "Sedes.xlsx")
presPath    <- file.path(inPath, "Prestadores.xlsx")
servPath    <- file.path(inPath, "Servicios.xlsx") 
capInsPath  <- file.path(inPath, "CapacidadInstalada.xlsx")
medSegPath  <- file.path(inPath, "MedidasSeguridad.xlsx")
sancPath    <- file.path(inPath, "MedidasSeguridad-sanciones.xlsx")

# Funciones --------------------------------

# Función para limpiar nombres con errores comunes
limpiar_texto <- function(texto) {
  texto %>%
    str_replace_all("[^[:alnum:]áéíóúÁÉÍÓÚñÑ ]", "") %>%  # elimina caracteres especiales
    str_squish() %>%                                      # elimina espacios extra
    str_to_title()                                        # pone en formato título
}

##################################################################################
# Lectura Inputs - Procesamiento
##################################################################################


# Leer inputs

# Municipios
munDat <- read_excel(munPath) %>% 
  data.table() %>%
  clean_names()  # estandariza nombres de columnas

# Sedes
sedDat <- read_excel(sedePath) %>%
  data.table() %>%
  clean_names()

# Prestadores
presDat <- read_excel(presPath) %>%
  data.table() %>%
  clean_names()

# Servicios
servDat <- read_excel(servPath) %>%
  data.table() %>%
  clean_names()

# Capacidad Instalada
capInsDat <- read_excel(capInsPath) %>%
  data.table() %>%
  clean_names()

# Medidas de seguridad
medSegDat <- read_excel(medSegPath) %>%
  data.table() %>%
  clean_names()

# Sanciones
sancDat <- read_excel(sancPath) %>%
  data.table() %>%
  clean_names()

# Preprocesamiento ---------------------------

# Aplicar la limpieza a Departamento y Municipio
munDat <- munDat %>%
  mutate(
    departamento = limpiar_texto(departamento),
    municipio = limpiar_texto(municipio)
  )

# definir rural en formato numerico ( 0 a 1 )
munDat[, rural := rural/100]

# depuración de columnas de tablas

# Columnas a borrar
borrar_columnas <- c("gerente", "direccion", "barrio", "telefono", "fax", 
  "email", "habilitado", "horario_lunes", "horario_martes", "horario_miercoles", 
  "horario_jueves", "horario_viernes", "horario_sabado", "horario_domingo", 
  "fecha_corte_reps", "telefono_adicional", "email_adicional" )
sedDat[, (borrar_columnas) := NULL ]

borrar_columnas <- c("nombre_prestador", "tido_codigo", "razon_social", "direccion", 
  "fax", "email", "telefono", "habilitado", "gerente", "nivel","caracter", 
  "fecha_cierre", "fecha_corte_reps", "telefono_adicional", "email_adicional", 
  "rep_legal")
presDat[, (borrar_columnas) := NULL ]

borrar_columnas <- c("direccion", "email", "telefono", "habilitado", 
  "gerente", "nivel","caracter",   "fecha_cierre", 
  "observaciones_serv_res3100_2019", "fecha_corte_reps", "horario_lunes", 
  "horario_martes", "horario_miercoles", "horario_jueves", "horario_viernes", 
  "horario_sabado", "horario_domingo", "telefono_adicional", "email_adicional" )
servDat[, (borrar_columnas) := NULL ]

borrar_columnas <- c("fecha_corte_reps", "telefono", "email", "direccion")
capInsDat[, (borrar_columnas) := NULL ]

borrar_columnas <- c("direccion", "email", "telefono", 
  "fecha_corte_reps", "nombre_prestador" )
medSegDat[, (borrar_columnas) := NULL ]
sancDat[, (borrar_columnas) := NULL ]

save.image(file = file.path(outPath, "datosPreprocesados.Rdata") )