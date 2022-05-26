library(shiny)
library(tidyverse)
library(sf)
library(geoAr)
library(shinyjs)
library(shinyvalidate)
library(shinyWidgets)
library(colourpicker)

# ARGENTINA
mapa_arg <- get_geo("ARGENTINA", level = "provincia") %>% 
  add_geo_codes() %>% 
  mutate(name_iso = case_when(name_iso == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
                              TRUE ~ name_iso))


source("modules/add_layer.R", encoding = "UTF-8")
