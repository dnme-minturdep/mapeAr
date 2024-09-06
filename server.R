
shinyServer(function(input, output, session) {


    # Creo objeto para guardar capas
    plot.dat <- reactiveValues()
    
    
    # Filtro mapa de Argentina/provincias
    mapa_base <- reactive({
        req(input$provincia)
        if (input$provincia == "País") {
            mapa_arg <- mapa_arg    
        } else {
            mapa_arg <- mapa_arg %>% filter(name_iso %in% input$provincia)
        }
    })
    
    # Armo capa base
    plot.dat$main <- reactive(ggplot() +
                                  geom_sf(data = mapa_base(), fill = input$fill_arg, color = input$color_arg) +
                                  theme_void() #+
                                  #theme(legend.position = "none")
                              )
    
    # Cargo capa continente Sudamérica
    observeEvent(input$sudamerica,{
        if (input$sudamerica == T) {
            sudamerica <- read_sf("/srv/DataDNMYE/capas_sig/sudamerica.geojson")
            plot.dat$layerSud <- geom_sf(data =sudamerica, fill = "transparent")
        } else {
            plot.dat$layerSud <- NULL
        }
    })
    
    # Cargo capa de departamentos
    observeEvent(input$deptos,{
        if (input$deptos == T & input$provincia == "País") {
            deptos <- get_geo("ARGENTINA", level = "departamento")
            plot.dat$layerDep <- geom_sf(data =deptos, fill = "transparent")
        } else if (input$deptos == T & !input$provincia == "País") {
            deptos <- get_geo("ARGENTINA", level = "departamento") %>% 
                add_geo_codes() %>% 
                mutate(name_iso = case_when(name_iso == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
                                            TRUE ~ name_iso))
            plot.dat$layerDep <- geom_sf(data =deptos %>% filter(name_iso %in% input$provincia), fill = "transparent")
        } else {
            plot.dat$layerDep <- NULL
        }
    })
    
    # Cargo etiquetas de provincias
    observeEvent(list(input$refProv,input$provincia, input$refProvSize, input$color_deptos),{
        req(input$provincia)
        if (input$refProv == T & input$provincia == "País") {
            plot.dat$refMain <- geom_sf_text(data = mapa_arg, aes(label = name_iso), 
                                             size = as.numeric(input$refProvSize),
                                             color = input$color_deptos) 
        } else if (input$refProv == T & !input$provincia == "País") {
            plot.dat$refMain <- geom_sf_text(data = mapa_arg %>% filter(name_iso %in% input$provincia), 
                                             aes(label = name_iso), size = as.numeric(input$refProvSize),
                                             color = input$color_deptos)
        } else {
            plot.dat$refMain <- NULL
        }
    })
    
    # Capa de rutas naturales
    capa_rutas <- reactive({
        req(input$ruta_natural)
        if(input$ruta_natural == "Todas" & input$provincia == "País") {
            
            rutas_naturales_base <- rutas_naturales_base 
            
        } else if(input$ruta_natural == "Todas" & input$provincia != "País") {
            
            rutas_naturales_base <- rutas_naturales_base %>% 
                st_intersection(mapa_base() %>% summarise(geometry = st_union(geometry)))
            
        } else if(input$ruta_natural != "Todas" & input$provincia == "País") {
            
            rutas_naturales_base <- rutas_naturales_base %>% 
                filter(ruta_natural %in% input$ruta_natural) 
            
        } else {
            rutas_naturales_base <- rutas_naturales_base %>% 
                filter(ruta_natural %in% input$ruta_natural) %>% 
                st_intersection(mapa_base() %>% summarise(geometry = st_union(geometry)))
        }
        
    })
 
    plot.dat$layerRuta <- reactive(geom_sf(data = capa_rutas(), 
                                           fill = capa_rutas()$color_hex, 
                                           alpha = input$alpha_rn,
                                           linewidth = input$size_rn))
    
    
    # Rutas naturales grises de fondo
    rutas_naturales_fondo <- reactive({
        req(input$ruta_natural)
    if (input$rutas_gris == T & input$provincia != "País" & input$ruta_natural != "Todas") {
        rutas_naturales <- rutas_naturales_base %>%
           st_intersection(mapa_base()) %>% 
            filter(!ruta_natural %in% input$ruta_natural)
    } else if (input$rutas_gris == T & input$provincia == "País" & input$ruta_natural != "Todas") {
        rutas_naturales <- rutas_naturales_base %>%
            st_intersection(mapa_base() %>% summarise(geometry = st_union(geometry))) %>% 
            filter(!ruta_natural %in% input$ruta_natural)
    } else {
        rutas_naturales <- NULL
    } 
    })
    
    plot.dat$layerPre0 <- reactive(
        geom_sf(data = rutas_naturales_fondo(), 
                                           fill = "#E7E7E7",
                                           alpha = input$alpha_rn,
                linewidth = input$size_rn)
        )
    
    # Control de opciones de capas definidas
    
    options <- c("Ninguna","Regiones","Circuitos", "Circuitos subidos a la web", "Rutas Escénicas",
                 "Áreas Protegidas", "Vías Nacionales","Capitales")
    
    observeEvent(input$preCapas,{
        updateSelectInput(session=session, "preCapas2",
                          choices = c("Ninguna",options[options!=input$preCapas]))
    })
    
    # Cargo capa predefinida 1
    observeEvent(list(input$provincia,input$preCapas, input$size_pre, input$alpha_pre, mapa_base()),{
        if (input$preCapas == "Ninguna") {
            plot.dat$layerPre <- NULL
        } else if (input$preCapas == "Regiones") {
            regiones <- st_read("/srv/DataDNMYE/capas_sig/regiones_turisticas.gpkg", layer = "regiones_turisticas") %>% st_filter(mapa_base())
            plot.dat$layerPre <- geom_sf(data =regiones, aes(fill = region), linewidth = input$size_pre, alpha = input$alpha_pre)
        } else if (input$preCapas == "Circuitos") {
            circuitos <- st_read("/srv/DataDNMYE/capas_sig/circuitos.gpkg", "circuitos") %>% 
                    st_filter(mapa_base())
            plot.dat$layerPre <- geom_sf(data =circuitos, color = "#87222b", linewidth = input$size_pre, alpha = input$alpha_pre)
        } else if (input$preCapas == "Circuitos subidos a la web") {
            circuitos <- st_read("/srv/DataDNMYE/capas_sig/circuitos_web.gpkg", "circuitos_web") %>% 
                st_filter(mapa_base())
            plot.dat$layerPre <- geom_sf(data =circuitos, color = "#632711", linewidth = input$size_pre, alpha = input$alpha_pre)
        } else if (input$preCapas == "Rutas Escénicas") {
            rutas_escenicas <- st_read("/srv/DataDNMYE/capas_sig/rutas_escenicas.gpkg", "rutas_escenicas") %>% 
                st_intersection(mapa_base())
            plot.dat$layerPre <- geom_sf(data =rutas_escenicas, color = "#333057", linewidth = input$size_pre, alpha = input$alpha_pre)   
        } else if (input$preCapas == "Áreas Protegidas") {
            areas_protegidas <- st_read("/srv/DataDNMYE/capas_sig/areas_protegidas_ign.gpkg", "areas_protegidas_ign") %>% st_intersection(mapa_base())
            plot.dat$layerPre <- geom_sf(data =areas_protegidas, fill = "#23a623", linewidth = input$size_pre, alpha = input$alpha_pre)
        } else if (input$preCapas == "Vías Nacionales") {
            vias_nacionales <- st_read("/srv/DataDNMYE/capas_sig/vias_nacionales_ign.gpkg", layer = "vias_nacionales_ign") %>% st_intersection(mapa_base())
            plot.dat$layerPre <- geom_sf(data =vias_nacionales, color = "#356296", linewidth = input$size_pre, alpha = input$alpha_pre)
        } else if (input$preCapas == "Capitales") {
            capitales <- st_read("/srv/DataDNMYE/capas_sig/capitales.gpkg", layer = "capitales") %>% st_filter(mapa_base())
            plot.dat$layerPre <- geom_sf(data =capitales, color = "black", fill = "#45261a", shape = 21, size = input$size_pre, alpha = input$alpha_pre)
        }
    })


    # Cargo capa predefinida 2
    observeEvent(list(input$provincia,input$preCapas2, input$size_pre2, input$alpha_pre2, mapa_base()),{
        if (input$preCapas2 == "Ninguna") {
            plot.dat$layerPre2 <- NULL
        } else if (input$preCapas2 == "Regiones") {
            regiones <- st_read("/srv/DataDNMYE/capas_sig/regiones_turisticas.gpkg", layer = "regiones_turisticas") %>% st_filter(mapa_base())
            plot.dat$layerPre2 <- geom_sf(data =regiones, aes(fill = region), linewidth = input$size_pre2, alpha = input$alpha_pre2)
        } else if (input$preCapas2 == "Circuitos") {
            circuitos <- st_read("/srv/DataDNMYE/capas_sig/circuitos.gpkg", "circuitos") %>% 
                st_filter(mapa_base())
            plot.dat$layerPre2 <- geom_sf(data =circuitos, color = "#87222b", linewidth = input$size_pre2, alpha = input$alpha_pre2)
        } else if (input$preCapas2 == "Circuitos subidos a la web") {
            circuitos <- st_read("/srv/DataDNMYE/capas_sig/circuitos_web.gpkg", "circuitos_web") %>% 
                st_filter(mapa_base())
            plot.dat$layerPre2 <- geom_sf(data =circuitos, color = "#632711", linewidth = input$size_pre2, alpha = input$alpha_pre2)
        } else if (input$preCapas2 == "Rutas Escénicas") {
            rutas_escenicas <- st_read("/srv/DataDNMYE/capas_sig/rutas_escenicas.gpkg", "rutas_escenicas") %>% 
                st_intersection(mapa_base())
            plot.dat$layerPre2 <- geom_sf(data =rutas_escenicas, color = "#333057", linewidth = input$size_pre2, alpha = input$alpha_pre2)   
        } else if (input$preCapas2 == "Áreas Protegidas") {
            areas_protegidas <- st_read("/srv/DataDNMYE/capas_sig/areas_protegidas_ign.gpkg", "areas_protegidas_ign") %>% 
                st_intersection(mapa_base())
            plot.dat$layerPre2 <- geom_sf(data =areas_protegidas, fill = "#23a623", linewidth = input$size_pre2, alpha = input$alpha_pre2)
        } else if (input$preCapas2 == "Vías Nacionales") {
            vias_nacionales <- st_read("/srv/DataDNMYE/capas_sig/vias_nacionales_ign.gpkg", layer = "vias_nacionales_ign") %>% 
                st_intersection(mapa_base())
            plot.dat$layerPre2 <- geom_sf(data =vias_nacionales, color = "#356296", linewidth = input$size_pre2, alpha = input$alpha_pre2)
        } else if (input$preCapas2 == "Capitales") {
            capitales <- st_read("/srv/DataDNMYE/capas_sig/capitales.gpkg", layer = "capitales") %>% 
                st_filter(mapa_base())
            plot.dat$layerPre2 <- geom_sf(data =capitales, color = "black", fill = "#45261a", shape = 21, size = input$size_pre2, alpha = input$alpha_pre2)
        }
    })
    
    # Validadores de valores de formato del mapa descargable
    ancho_mapa <- InputValidator$new()
    ancho_mapa$add_rule("widthMap", sv_between(3, 12, message_fmt = "Inserte un valor entre {left} y {right}"))
    ancho_mapa$enable()
    
    alto_mapa <- InputValidator$new()
    alto_mapa$add_rule("heightMap", sv_between(3, 12, message_fmt = "Inserte un valor entre {left} y {right}"))
    alto_mapa$enable()
    
    dpi_mapa <- InputValidator$new()
    dpi_mapa$add_rule("dpiMap", sv_between(50, 1000, message_fmt = "Inserte un valor entre {left} y {right}"))
    dpi_mapa$enable()
    
    # Llamo módulo de capas
    value1 <- capasServer("layer1")
    value2 <- capasServer("layer2")
    value3 <- capasServer("layer3")
    
    # Guardo capas generadas por el módulo
    observe({
        
        if (!is.null(value1$d())) {
            plot.dat$layer1 <- reactive({value1$a()})
            plot.dat$refLayer1 <- reactive({value1$c()})
        } else {
            plot.dat$layer1 <- reactive(NULL)
            plot.dat$refLayer1 <- reactive(NULL)
        }
       
        if (!is.null(value2$d())) {
            plot.dat$layer2 <- reactive({value2$a()})
            plot.dat$refLayer2 <- reactive({value2$c()})
        } else {
            plot.dat$layer2 <- reactive(NULL)
            plot.dat$refLayer2 <- reactive(NULL)
        }
        
        if (!is.null(value3$d())) {
            plot.dat$layer3 <- reactive({value3$a()})
            plot.dat$refLayer3 <- reactive({value3$c()})
        } else {
            plot.dat$layer3 <- reactive(NULL)
            plot.dat$refLayer3 <- reactive(NULL)
        }
        
    })
        
    plot.dat$layerLegend <- reactive({
        if (input$legendSwitch) {
            
        theme(legend.position = "right")
            
        } else {
            
            theme(legend.position = "none")
            
        }
        
        })

    # Genero mapa con todas las capas
        mapa <- reactive({
                
                plot.dat$main() + plot.dat$refMain + 
                    plot.dat$layerRuta() +
                    plot.dat$layerPre0() + 
                    plot.dat$layerPre + plot.dat$layerPre2 +
                    plot.dat$layerSud + plot.dat$layerDep + 
                    plot.dat$layer1() + plot.dat$refLayer1() + 
                    plot.dat$layer2() + plot.dat$refLayer2() +
                    plot.dat$layer3() + plot.dat$refLayer3()  +
                plot.dat$layerLegend()
                
            
        })
        
        output$mapa <- renderPlot({
            mapa() 
        })
        
    legend <- reactive({
        ggpubr::get_legend(mapa())
    })    
        
        
    output$downloadLegend <- downloadHandler(
        filename = function() {paste("leyenda", input$formatoMapa, sep =".")},
        content = function(file) {
            ggsave(file, plot = legend(), device = input$formatoMapa, bg = "transparent",
                   dpi = as.numeric(input$dpiMap))
        })
    
    # Renderizo mapa

    #Oculto waiter    
    waiter_hide()
        
    # Control del botón de descargar mapa
    output$downloadMap <- downloadHandler(
        filename = function() {paste("mapear", input$formatoMapa, sep =".")},
        content = function(file) {
            ggsave(file, plot = mapa(), device = input$formatoMapa, bg = "transparent",
                   width = as.numeric(input$widthMap), height = as.numeric(input$heightMap), 
                   dpi = as.numeric(input$dpiMap))
        }) 
    
    
    # Control del botón de descargar data modelo
    output$downloadData <- downloadHandler(
        filename = "aeropuertos_modelo.xlsx",
        content = function(file) {
            file.copy("data_modelo/aeropuertos_modelo.xlsx", file)
        })
    
})
