# Cargo librerías
library(shiny) # Web Application Framework for R, CRAN v1.7.1
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.1
library(sf) # Simple Features for R, CRAN v1.0-0
library(geoAr) # Argentina's Spatial Data Toolbox, [github::politicaargentina/geoAr] v0.0.1.2
library(shinyjs) # Easily Improve the User Experience of Your Shiny Apps in Seconds, CRAN v2.0.0
library(shinyvalidate) # Input Validation for Shiny Apps, CRAN v0.1.2
library(shinyWidgets) # Custom Inputs Widgets for Shiny, CRAN v0.7.0
library(colourpicker) # A Colour Picker Tool for Shiny and for Selecting Colours in Plots, CRAN v1.1.1

# Cargo mapa de Argentina
mapa_arg <- get_geo("ARGENTINA", level = "provincia") %>% 
  add_geo_codes() %>% 
  mutate(name_iso = case_when(name_iso == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
                              TRUE ~ name_iso))

# Cargo módulo de capas
source("modules/add_layer.R", encoding = "UTF-8")

