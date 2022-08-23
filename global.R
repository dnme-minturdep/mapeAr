# Cargo librerías
library(shiny) # Web Application Framework for R, CRAN v1.7.1
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.1
library(sf) # Simple Features for R, CRAN v1.0-0
library(geoAr) # Argentina's Spatial Data Toolbox, [github::politicaargentina/geoAr] v0.0.1.2
library(shinyjs) # Easily Improve the User Experience of Your Shiny Apps in Seconds, CRAN v2.0.0
library(shinyvalidate) # Input Validation for Shiny Apps, CRAN v0.1.2
library(shinyWidgets) # Custom Inputs Widgets for Shiny, CRAN v0.7.0
library(colourpicker) # A Colour Picker Tool for Shiny and for Selecting Colours in Plots, CRAN v1.1.1
library(waiter) # Loading screens for Shiny
library(shinyBS) # Twitter Bootstrap Components for Shiny, CRAN v0.61.1

# Cargo mapa de Argentina
mapa_arg <- get_geo("ARGENTINA", level = "provincia") %>% 
  add_geo_codes() %>% 
  mutate(name_iso = case_when(name_iso == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
                              TRUE ~ name_iso))

rutas_naturales_base <- st_read("/srv/DataDNMYE/capas_sig/rutas_naturales.gpkg", "rutas_naturales")

# Cargo módulo de capas
source("modules/add_layer.R", encoding = "UTF-8")

loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)
