
## Segmento del UI
capasUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(ns("vector"),label = "Datos espaciales:", choices = c("Puntos","Líneas","Polígonos"), inline = T),
    
    fileInput(ns("file"), "Cargar archivo", 
              buttonLabel = "Buscar", placeholder = "Sin archivo",
              multiple = FALSE
    ),
    
    
    fluidRow(
      column(3,wellPanel(selectInput(ns("opcionColor"),label = "Color", choices=c("Único","Según variable","Personalizado")),
                         uiOutput(ns("colorear")))),
      
      column(3,wellPanel(selectInput(ns("opcionShape"),label = "Forma", choices=c("Única","Según variable","Ícono")),
                         selectInput(ns("shape"), label = NULL, choices = c("Círculos"=16,"Triangulos"=17,"Rombos"=18,"Cuadrados"=15))
                        )),
      
      column(3,wellPanel(selectInput(ns("opcionSize"),label = "Tamaño", choices=c("Único","Según variable")),
                         selectInput(ns("size"), label = NULL, choices = c(0.1,0.5,1:7)))),
      
      column(3,wellPanel(noUiSliderInput(ns("alpha"), label = "Opacidad", min = -0.1, max = 1.1, 
                                         value = 1, step = 0.1, orientation = "vertical", padding = 0.1,
                                         height = 80, direction = "rtl", color = "#37BBED")))
      
    ),

    conditionalPanel(
      condition = 'input.opcionShape == "Ícono"', ns = ns,
      wellPanel(
      sliderInput(ns("iconAsp"), label = "Relación de aspecto", 
                  value = 1, min = 0, max = 1, step = 0.1)
    )),
    
    wellPanel(
      fluidRow(
      column(6,selectInput(ns("referencias"), "Referencias", choices = c("Ninguna"))),
      column(6,selectInput(ns("labelRef"), "Tipo", choices = c("Texto","Etiqueta")))),
      
      fluidRow(
      column(6,sliderInput(ns("sizeRef"), "Tamaño", min = 0.5, max = 5, step = 0.5, value = 2)),
      column(6,sliderInput(ns("labelDist"), "Distancia", min = 0.1, max = 5, step = 0.5, value = 1)))
    )
   
  )
}

## Segmento del server
capasServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    layer <- reactiveValues(capa = NULL)
    
    # Chequeo tipo de archivo cargado
    ext <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
    })
    
    # Levanto datos
    capa <- eventReactive(list(input$file,ext()),{
      req(input$file)
      if (input$vector == "Puntos" & ext() %in% c("xlsx","xls")) {
        test <- readxl::read_excel(input$file$datapath) %>% 
          janitor::clean_names()
        if ("latitud" %in% colnames(test)) {
          data <- readxl::read_excel(input$file$datapath) %>% 
            janitor::clean_names() %>%
            mutate(latitud = str_replace(latitud, ",", "."),
                   longitud = str_replace(longitud, ",", "."),
                   longitud = as.numeric(longitud),
                   latitud = as.numeric(latitud)) %>%
            st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
        } else {
          data <- read_sf(input$file$datapath) %>% 
            janitor::clean_names()
        }
        rm(test)
      } else if (input$vector == "Puntos" & !ext() %in% c("xlsx","xls")) {
        test <- read_sf(input$file$datapath) %>% 
          janitor::clean_names()
        if ("latitud" %in% colnames(test)) {
          data <- data.table::fread(input$file$datapath) %>% 
            janitor::clean_names() %>% 
            mutate(latitud = str_replace(latitud, ",", "."),
                   longitud = str_replace(longitud, ",", "."),
                   longitud = as.numeric(longitud),
                   latitud = as.numeric(latitud)) %>%
            st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
        } else {
          data <- read_sf(input$file$datapath) %>% 
            janitor::clean_names() %>% 
            st_as_sf(wkt = "geometry", crs = 4326)
        }
        rm(test)
      } else {
        data <- read_sf(input$file$datapath) %>% 
          janitor::clean_names()
      }
      
      
      if ("icono" %in% colnames(data)) {
        
        temp_icon <- data %>% 
          distinct(icono) %>% 
          group_by(icono) %>% 
          mutate(temp = tempfile())
        
        data <- data %>% 
          left_join(temp_icon) %>% 
          mutate(icono = paste0(icono,".svg"),
                 icono = ifelse(str_detect(icono, "poncho"),
                                str_replace(icono, "poncho-", "https://raw.githubusercontent.com/argob/iconos/41863b8903574925f8398daa8d98283c1eaa3cd3/src/_icons/"),
                                str_replace(icono, "fa-", "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/d3a7818c253fcbafff9ebd1d4abb2866c192e1d7/svgs/solid/")))
        
        
        for (i in unique(data$icono)) {
          
          o <- data %>% 
            filter(icono == i) %>% 
            pull(temp) %>% 
            unique()
          
          download.file(i, o)
          
          bitmap <- rsvg::rsvg(o, width = 100, height = 100)
          
          png::writePNG(bitmap, o, dpi = 200)
          }
          
      }
      
      data
    })
    
    
    # Actualizo controles (selects)
    observeEvent(capa(),{
      updateSelectInput(inputId = "referencias",choices = c("Ninguna",colnames(capa() %>% st_set_geometry(NULL))))
    })
    
    observeEvent(input$vector,{
      if (input$vector=="Polígonos") {
        updateSelectInput(inputId = "opcionShape", label = "Contorno", choices = c("Único","Según variable"))
        
      } else if (input$vector %in% c("Puntos","Líneas")) {
        updateSelectInput(inputId = "opcionShape", label = "Forma", choices = c("Única", "Según variable", "Ícono"))
      }
    })
    
    
    output$colorear <- renderUI({
      if (input$opcionColor == "Único") {
        colourInput(inputId = ns("color"), label = NULL, value = "purple")
      } else if (input$opcionColor == "Según variable"){
        selectInput(inputId = ns("color"), label = NULL, choices = colnames(capa() %>% select_if(is.character) %>% st_set_geometry(NULL)))
      } else if (input$opcionColor == "Personalizado") {
        selectInput(inputId = ns("color"), label = NULL, choices = "color_hex")
      }
    })     
    
    
    observeEvent(list(input$opcionShape, input$vector),{
      if (input$vector =="Puntos" & input$opcionShape == "Única") {
        updateSelectInput(inputId = "shape", choices = c("Círculos"=16,"Triangulos"=17,"Rombos"=18,"Cuadrados"=15))
      } else if (input$vector =="Puntos"& input$opcionShape == "Según variable") {
        updateSelectInput(inputId = "shape", choices = colnames(capa() %>% select_if(is.character)%>% st_set_geometry(NULL)))
      } else if (input$vector =="Puntos" & input$opcionShape == "Ícono") {
        updateSelectInput(inputId = "shape", choices = "icono")
        updateSelectInput(inputId = "size", choices = c(0.05,0.1,0.15,0.2,0.25,0.3))
      } else if (input$vector =="Líneas" & input$opcionShape == "Única") {
        updateSelectInput(inputId = "shape", choices = c("Sólida"="solid","Punteada 1"="dotted","Punteada 2"="dashed"))
      } else if (input$vector =="Líneas" & input$opcionShape == "Según variable") {
        updateSelectInput(inputId = "shape", choices = colnames(capa() %>% select_if(is.character)%>% st_set_geometry(NULL)))
      } else if (input$vector =="Polígonos" & input$opcionShape == "Único") {
        updateSelectInput(inputId = "shape", choices = c("Negro"="black",
                                                         "Gris"="grey",
                                                         "Blanco"="white",
                                                         "Rojo"="red",
                                                         "Azul"="blue",
                                                         "Amarillo"="yellow",
                                                         "Verde"="green"))
      } else if (input$vector =="Polígonos" & input$opcionShape == "Según variable") {
        updateSelectInput(inputId = "shape", choices = colnames(capa() %>% select_if(is.character)%>% st_set_geometry(NULL)))
      }
    })
    
    observeEvent(list(input$opcionSize, input$opcionShape),{
      if (input$opcionSize == "Único" & input$opcionShape != "Ícono") {
        updateSelectInput(inputId = "size", choices = c(0.1,0.2,0.5,1:7), selected = 3)
      } else  if (input$opcionSize == "Único" & input$opcionShape == "Ícono") {
        updateSelectInput(inputId = "size", choices = c(0.05,0.1,0.15,0.2,0.25,0.3), selected = 0.1)
      } else if (input$opcionSize == "Según variable") {
        updateSelectInput(inputId = "size", choices = colnames(capa() %>% select_if(is.numeric) %>% st_set_geometry(NULL)))
      }
    })
    

    # Armo capa según opciones del usuario
    layer$capa <- eventReactive(list(input$file,capa(),input$vector,input$color,input$shape,input$size, input$alpha, input$iconAsp),{
      if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), color = input$color, shape = as.numeric(input$shape), size = as.numeric(input$size), alpha =input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = input$color, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(shape = input$shape, size = input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(shape = input$shape),size = as.numeric(input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color, size = input$size), shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color), size = as.numeric(input$size), shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color,size = input$size, shape = input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color, shape = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = capa()$color_hex, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(color = input$color, size = input$size,shape = input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(color = input$color,shape = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        list(geom_sf(data = capa(), aes_string(shape = input$shape, size = input$size), color = capa()$color_hex, alpha = input$alpha),
             scale_color_manual(values = levels(factor(capa()$color_hex))))
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        list(geom_sf(data = capa(), aes_string(shape = input$shape, color = input$color),size = as.numeric(input$size)),
             scale_color_manual(values = levels(factor(capa()$color_hex))))
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), size = as.numeric(input$size), color = capa()$color_hex, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), color = input$color, linetype = input$shape, linewidth = as.numeric(input$size), alpha =input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(linewidth = input$size), color = input$color, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(linetype = as.character(input$shape), linewidth = input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(linetype = input$shape),linewidth = as.numeric(input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color,linewidth = input$size), linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color), linewidth = as.numeric(input$size), linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color,linewidth = input$size, linetype = input$shape), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color, linetype = input$shape),linewidth = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(linewidth = input$size), color = capa()$color_hex, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), color = capa()$color_hex,linewidth = input$size, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(color = input$color, linewidth = input$size, linetype = input$shape), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(color = input$color,linetype = input$shape),linewidth = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(linetype = input$shape, linewidth = input$size), color = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(linetype = input$shape),linewidth = as.numeric(input$size), color = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(linewidth = input$size), color = capa()$color_hex, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), linewidth = as.numeric(input$size), color = capa()$color_hex, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Único" & input$opcionSize == "Único"){
        geom_sf(data = capa(), linewidth = as.numeric(input$size), color = input$shape, fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Único" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(linewidth = input$size), color = input$shape, fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = as.character(input$shape), linewidth = input$size), fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$shape),linewidth = as.numeric(input$size),fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Único" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(fill = input$color,linewidth = input$size), color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Único" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(fill = input$color), linewidth = as.numeric(input$size), color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(fill = input$color,linewidth = input$size, color = input$shape), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(fill = input$color, color = input$shape),linewidth = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(linewidth = input$size), fill = capa()$color_hex, color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Único") {
        geom_sf(data = capa(), fill = capa()$color_hex,linewidth = input$size, color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(fill = capa(), linewidth = input$size, color = input$shape), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(fill = capa(),color = input$shape),linewidth = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(color = input$shape, linewidth = input$size), fill = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(color = input$shape),linewidth = as.numeric(input$size), fill = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(linewidth = input$size), fill = capa()$color_hex, color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Único") {
        geom_sf(data = capa(), linewidth = as.numeric(input$size), fill = capa()$color_hex, color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Ícono" & input$opcionSize == "Único") {
        geom_image(data = capa(), aes(geometry = geometry, image = temp), stat = "sf_coordinates", asp = as.numeric(input$iconAsp), size = as.numeric(input$size), 
                           color = input$color, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Ícono" & input$opcionSize == "Según variable") {
        list(geom_image(data = capa(), aes_string(geometry = "geometry", image = "temp", size = input$size), stat = "sf_coordinates", asp = as.numeric(input$iconAsp), 
                   color = input$color, alpha = input$alpha),
             scale_size(range = c(0.05,0.2)))
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Ícono" & input$opcionSize == "Único") {
        geom_image(data = capa(), aes_string(geometry = "geometry", image = "temp", color = input$color), stat = "sf_coordinates", asp = as.numeric(input$iconAsp), 
                   alpha = input$alpha, size = as.numeric(input$size))
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Ícono" & input$opcionSize == "Según variable") {
        list(geom_image(data = capa(), aes_string(geometry = "geometry", image = "temp", color = input$color, size = input$size), stat = "sf_coordinates", asp = as.numeric(input$iconAsp), 
                   alpha = input$alpha),
             scale_size(range = c(0.05,0.2)))
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Ícono" & input$opcionSize == "Según variable") {
        list(geom_image(data = capa(), aes_string(geometry = "geometry", image = "temp", size = input$size), stat = "sf_coordinates", asp = as.numeric(input$iconAsp), 
                        color = capa()$color_hex, alpha = input$alpha),
             scale_color_manual(values = levels(factor(capa()$color_hex))),
             scale_size(range = c(0.05,0.2)))
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Ícono" & input$opcionSize == "Único") {
        list(geom_image(data = capa(), aes(geometry = geometry, image = temp), stat = "sf_coordinates", asp = as.numeric(input$iconAsp), 
                        color = capa()$color_hex, alpha = input$alpha, size = as.numeric(input$size)),
             scale_color_manual(values = levels(factor(capa()$color_hex))))
      } else {
        NULL
      }
    })
    
    # Genero capa con referencias
    refs <- eventReactive(list(input$referencias, input$sizeRef, input$labelRef, input$labelDist),{
      if (input$referencias=="Ninguna") {
        return(NULL)
      } else if (input$referencias!="Ninguna" & input$labelRef=="Texto") {
        ggrepel::geom_text_repel(data=capa(), aes_string(label= input$referencias, geometry="geometry"), 
                                 stat = "sf_coordinates", size = as.numeric(input$sizeRef),
                                 box.padding = input$labelDist, min.segment.length = 2, max.overlaps = 200)
      } else if (input$referencias!="Ninguna" & input$labelRef=="Etiqueta") {
        ggrepel::geom_label_repel(data=capa(), aes_string(label= input$referencias, geometry="geometry"), 
                                  stat = "sf_coordinates", size = as.numeric(input$sizeRef),
                                  box.padding = input$labelDist, min.segment.length = 2, max.overlaps = 200)
      }
      
    })

    
    # Envío capa y valores al server
    return(list(a = reactive(layer$capa()),
                c = reactive(refs()),
                d = reactive(input$file)
                ))
  })
}