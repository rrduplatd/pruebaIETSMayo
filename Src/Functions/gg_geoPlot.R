library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(data.table)
library(ggplot2)
library(gridExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Funciones
formatear_variable <- function(nombre_var) {
  partes <- strsplit(nombre_var, "_")[[1]]
  primera <- stringr::str_to_title(partes[1])
  segunda <- toupper(partes[2])
  paste(primera, segunda)
}

ggDeptoAgr <- function(df = sedDat, aux = FALSE) {
  if (is.logical(aux)) {
    sedesDep <- df %>%
      group_by(departamento) %>%
      summarise(n_Sedes = n()) %>%
      mutate(depa_nombre = toupper(departamento))

    mapa <- colombia %>%
      left_join(sedesDep, by = c("name_upper" = "depa_nombre")) %>%
      mutate(n_Sedes = ifelse(is.na(n_Sedes), 0, n_Sedes))

    gg <- ggplot(mapa) +
      geom_sf(aes(fill = n_Sedes), color = "white") +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      theme_minimal() +
      labs(title = "Mapa de Calor: Número de Sedes por Departamento",
           fill = "Sedes") +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
    
  } else {
    aux_sym <- rlang::sym(aux)

    # Obtener todos los valores únicos de la variable dicotómica
    valores_aux <- df %>%
      filter(!is.na(!!aux_sym)) %>%
      distinct(!!aux_sym) %>%
      pull()

    # Crear una copia del mapa para cada valor de aux
    mapa_expandido <- purrr::map_dfr(valores_aux, function(val) {
      colombia %>%
        mutate(aux_val = val)
    })

    # Agrupar los datos reales
    sedesDep <- df %>%
      filter(!is.na(!!aux_sym)) %>%
      group_by(departamento, !!aux_sym) %>%
      summarise(n_Sedes = n(), .groups = "drop") %>%
      mutate(depa_nombre = toupper(departamento))

    # Unir shapefile expandido con los datos
    mapa <- mapa_expandido %>%
      left_join(sedesDep, by = c("name_upper" = "depa_nombre", "aux_val" = aux)) %>%
      mutate(n_Sedes = ifelse(is.na(n_Sedes), 0, n_Sedes))

    gg <- ggplot(mapa) +
      geom_sf(aes(fill = n_Sedes), color = "white") +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      facet_wrap(~aux_val) +
      theme_minimal() +
      labs(title = paste0("Mapa de Calor: Número de Sedes por Departamento \n" , formatear_variable(aux)),
           fill = "Prestadores") +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  }

  return(gg)
}


ggDeptoSerEl = function(df = capInsDat, coca_nam = "Pediátrica" , 
  grupo_cap = "CAMAS", desagre = NULL){
  #Funcion que dibuja el mapa de calor por departamento por servicio (coca_nombre)
  # y elemento (grupo_capacidad)
  #
  #Parametros 
  #df = data.table con la tabla de interes para mapa de calor
  #coca_nam = valor especifico de coca_nombre/servicios
  #grupo_cap = elemento especifico del servicio
  #desagre = mensaje para el titulo por si se desagrega la tabla antes de la función
  
  df_filtrado <- df[ coca_nombre == coca_nam & grupo_capacidad == grupo_cap]
  
  # Agrupar por departamento y sumar cantidad
  df_agregado <- df_filtrado %>%
    group_by(depa_nombre) %>%
    summarise(total_elemento = sum(as.numeric(cantidad), na.rm = TRUE)) %>%
    mutate(depa_nombre = toupper(depa_nombre))
  
  # Unir datos de prestadores con shapefile
  mapa <- colombia %>%
    left_join(df_agregado, by = c("name_upper" = "depa_nombre"))
  
  
  gg = ggplot(mapa) +
         geom_sf(aes(fill = total_elemento), color = "white") +
         scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
         theme_minimal() +
         labs(title = paste( grupo_cap , "para servicio de", 
                             coca_nam , "\n"," Departamento", desagre , sep = " "),
              fill = paste("Total", grupo_cap, sep = " ")) +
         theme(axis.text = element_blank(),
               axis.ticks = element_blank())
  
  return(gg)
}

