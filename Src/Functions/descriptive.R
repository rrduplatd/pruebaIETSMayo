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

ResumenSedDeo <- function(df, depart_input) {
  # Filtrar por departamento
  df_filtrado <- df %>%
    filter(departamento == depart_input)
  
  # Contar sedes por municipio
  sedes_mpio <- df_filtrado %>%
    group_by(mpio) %>%
    summarise(n_sedes = n()) %>% data.table()
  
  # Imprimir resumen estadístico
  print(paste("Estadísticas descriptivas para el departamento:", depart_input))
  print(summary(sedes_mpio$n_sedes))
  
  # Dibujar histograma
  gg1 = ggplot(sedes_mpio, aes(x = n_sedes)) +
      geom_histogram(fill = "darkorange", color = "white") +
      labs(title = paste("Histograma de número de sedes por municipio en", depart_input),
           x = "Número de sedes por municipio",
           y = "Frecuencia (n° de municipios)") +
      theme_minimal()

    # Diagrama de violín
  gg2 = ggplot(sedes_mpio, aes(x = "", y = n_sedes)) +
      geom_violin(fill = "skyblue", color = "black") +
      geom_jitter(width = 0.1, alpha = 0.6) +  # puntos individuales
      labs(title = paste("Distribución de sedes por municipio en", depart_input),
           x = "",
           y = "Número de sedes por municipio") +
      theme_minimal()

    # Diagrama de cajas
  gg3 = ggplot(sedes_mpio, aes(x = "", y = n_sedes)) +
      geom_boxplot() + 
      labs(title = paste("Distribución de sedes por municipio en", depart_input),
           x = "",
           y = "Número de sedes por municipio") +
      theme_minimal()

  # salida de estadisticas descriptivas
  tablaSalida = sedes_mpio[, c(min = min(n_sedes), 
    Cuartil_1 = quantile(n_sedes, probs = 0.25, names = F), 
    Mediana = quantile(n_sedes, probs = 0.5, names = F),
    Cuartil_3 = quantile(n_sedes, probs = 0.75, names = F),
    max = max(n_sedes), 
    sd = sd(n_sedes))]

  return(list( Descriptivas = tablaSalida, 
    ggHist = gg1, ggViolin = gg2, ggCajas = gg3))
}

# Función para crear la presentación
crearPptSedes <- function(df = sedDatAll , depart_input = "Cundinamarca", salida = "reporte_sedes_sin_rvg.pptx") {
  # Ejecutar la función original
  resultados <- ResumenSedDeo(df, depart_input)
  temp = data.frame( Estadisticas = names(resultados$Descriptivas), 
                     Valor = round(resultados$Descriptivas, 2))


  # Crear nuevo pptx
  ppt <- read_pptx()
  
  # Guardar gráficos como archivos PNG temporales
  archivos_temp <- list(
    hist = tempfile(fileext = ".png"),
    violin = tempfile(fileext = ".png"),
    caja = tempfile(fileext = ".png")
  )
  
  ggsave(archivos_temp$hist, plot = resultados$ggHist, width = 8, height = 5, dpi = 300)
  ggsave(archivos_temp$violin, plot = resultados$ggViolin, width = 8, height = 5, dpi = 300)
  ggsave(archivos_temp$caja, plot = resultados$ggCajas, width = 8, height = 5, dpi = 300)

  # Resumen del departamento, segun archivo de municipios
  tablaGeneral <- unique(df[ departamento == depart_input , .(departamento,superficie, pop_tot, rural, region) ])
  tablaGeneral <- tablaGeneral[, .( sum(superficie), 
                                    sum(pop_tot), 
                                    sum(rural * superficie) , 
                                    unique(region) )]
  tablaGeneral[, V3 := V3 / V1]

  tablaGeneral <- data.table( 
    Caracteristica = 
      c("Superficie", "Poblacion Total", "Prc Superficie Rural", "Region"),
    Valor = 
      c(tablaGeneral[1]))

  # Slide 1: Resumen general del departamento
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  # Agregar título a la diapositiva
  ppt <- ph_with(ppt, depart_input, location = ph_location_type(type = "title"))
  # Insertar la tabla directamente como una tabla simple de PowerPoint
  ppt <- ph_with(ppt, value =  tablaGeneral, location = ph_location_type(type = "body"))

  # Slide 2: Estadisticas descriptivas
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  ppt <- ph_with(ppt, paste("Estadisticas descriptivas de -", depart_input), location = ph_location_type(type = "title"))
  ppt <- ph_with(ppt, value =  temp, location = ph_location_type(type = "body"))

  # Slide 3: Histograma
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = paste("Histograma -", depart_input), location = ph_location_type(type = "title")) %>%
  ph_with(external_img(archivos_temp$hist, width = 8, height = 5), location = ph_location_type(type = "body"))


  # Slide 4: Violín
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = paste("Violín -", depart_input), location = ph_location_type(type = "title")) %>%
    ph_with(external_img(archivos_temp$violin, width = 8, height = 5), location = ph_location_type(type = "body"))

  # Slide 5: Cajas
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = paste("Cajas -", depart_input), location = ph_location_type(type = "title")) %>%
    ph_with(external_img(archivos_temp$caja, width = 8, height = 5), location = ph_location_type(type = "body"))

  # Guardar el ppt
  print(ppt, target = salida)
  cat("Presentación creada en:", salida, "\n")
}


