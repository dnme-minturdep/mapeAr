
## Segmento del UI
capasUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    radioButtons(ns("vector"),label = "Datos espaciales:", choices = c("Puntos","Líneas","Polígonos"), inline = T),
    
    fileInput(ns("file"), "Cargar archivo", 
            buttonLabel = "Buscar", placeholder = "Sin archivo",
            multiple = FALSE, #accept = c(".csv",".geojson",".xlsx",".kml")
            ),
  
  fluidRow(
    column(3,wellPanel(selectInput(ns("opcionColor"),label = "Color", choices=c("Único","Según variable","Personalizado")),
                       uiOutput(ns("colorear")))),
    
    column(3,wellPanel(selectInput(ns("opcionShape"),label = "Forma", choices=c("Única","Según variable")),
                       selectInput(ns("shape"), label = NULL, choices = c("Círculos"=16,"Triangulos"=17,"Rombos"=18,"Cuadrados"=15)))),
    
    column(3,wellPanel(selectInput(ns("opcionSize"),label = "Tamaño", choices=c("Único","Según variable")),
                       selectInput(ns("size"), label = NULL, choices = c(1:7)))),
    
    column(3,wellPanel(noUiSliderInput(ns("alpha"), label = "Opacidad", min = -0.1, max = 1.1, 
                                         value = 1, step = 0.1, orientation = "vertical", padding = 0.1,
                                       height = 80, direction = "rtl", color = "#37BBED")))

  ),
  
  wellPanel(fluidRow(
    column(4,selectInput(ns("referencias"), "Referencias", choices = c("Ninguna"))),
    column(4,selectInput(ns("labelRef"), "Referencias", choices = c("Texto","Etiqueta"))),
    column(4,sliderInput(ns("sizeRef"), "Tamaño", min = 0.5, max = 5, step = 0.5, value = 2)))
    ),
  
  actionButton(ns("btnCapa"),label = "Agregar capa", icon = icon("plus", lib = "font-awesome"))
 ) 
}

## Segmento del server
capasServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    capa <- eventReactive(input$file,{
      req(input$file)
      if (input$vector == "Puntos") {
        test <- data.table::fread(input$file$datapath)
        if ("latitud" %in% colnames(test)) {
          data <- data.table::fread(input$file$datapath) %>% 
            janitor::clean_names() %>% 
            st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
        } else {
          data <- read_sf(input$file$datapath) %>% 
            janitor::clean_names()
        }
        } else {
        data <- read_sf(input$file$datapath) %>% 
          janitor::clean_names()
        }
      rm(test)
      data
    })
   
    observeEvent(capa(),{
        updateSelectInput(inputId = "referencias",choices = c("Ninguna",colnames(capa() %>% st_set_geometry(NULL))))
    })
    
    observeEvent(input$vector,{
      if (input$vector=="Polígonos") {
        updateSelectInput(inputId = "opcionShape", label = "Contorno",choices = c("Único","Según variable"))
        
      } else if (input$vector %in% c("Puntos","Líneas")) {
        updateSelectInput(inputId = "opcionShape", label = "Forma",choices = c("Única","Según variable"))
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

    observeEvent(list(input$opcionSize, input$vector),{
      if (input$opcionSize == "Único") {
        updateSelectInput(inputId = "size", choices = c(1:7))
      } else if (input$opcionSize == "Según variable") {
        updateSelectInput(inputId = "size", choices = colnames(capa() %>% select_if(is.numeric) %>% st_set_geometry(NULL)))
      }
    })
    
    layer <- eventReactive(list(capa(),input$vector,input$color,input$shape,input$size, input$alpha),{
      if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), color = input$color, shape = as.numeric(input$shape), size = as.numeric(input$size), alpha =input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = input$color, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(shape = input$shape, size = input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(shape = input$shape),size = as.numeric(input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color,size = input$size), shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color), size = as.numeric(input$size), shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color,size = input$size, shape = input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color, shape = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = capa()$color_hex, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), color = capa()$color_hex,size = input$size, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(color = capa(), size = input$size,shape = input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(color = capa(),shape = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(shape = input$shape, size = input$size), color = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(shape = input$shape),size = as.numeric(input$size), color = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = capa()$color_hex, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Puntos" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), size = as.numeric(input$size), color = capa()$color_hex, shape = as.numeric(input$shape), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), color = input$color, linetype = input$shape, size = as.numeric(input$size), alpha =input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = input$color, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(linetype = as.character(input$shape), size = input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(linetype = input$shape),size = as.numeric(input$size),color = input$color, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color,size = input$size), linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Única" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color), size = as.numeric(input$size), linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = input$color,size = input$size, linetype = input$shape), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$color, linetype = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = capa()$color_hex, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), color = capa()$color_hex,size = input$size, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(color = capa(), size = input$size, linetype = input$shape), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(color = capa(),linetype = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(linetype = input$shape, size = input$size), color = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(linetype = input$shape),size = as.numeric(input$size), color = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), color = capa()$color_hex, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Líneas" & input$opcionColor == "Personalizado" & input$opcionShape == "Única" & input$opcionSize == "Único") {
        geom_sf(data = capa(), size = as.numeric(input$size), color = capa()$color_hex, linetype = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Único" & input$opcionSize == "Único"){
        geom_sf(data = capa(), size = as.numeric(input$size), color = input$shape, fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Único" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(size = input$size), color = input$shape, fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(color = as.character(input$shape), size = input$size), fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Único" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(color = input$shape),size = as.numeric(input$size),fill = input$color, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Único" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(fill = input$color,size = input$size), color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Único" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(fill = input$color), size = as.numeric(input$size), color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable"){
        geom_sf(data = capa(), aes_string(fill = input$color,size = input$size, color = input$shape), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único"){
        geom_sf(data = capa(), aes_string(fill = input$color, color = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), fill = capa()$color_hex, color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Único") {
        geom_sf(data = capa(), fill = capa()$color_hex,size = input$size, color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(fill = capa(), size = input$size, color = input$shape), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Según variable" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(fill = capa(),color = input$shape),size = as.numeric(input$size), alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(color = input$shape, size = input$size), fill = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Según variable" & input$opcionSize == "Único") {
        geom_sf(data = capa(), aes_string(color = input$shape),size = as.numeric(input$size), fill = capa()$color_hex, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Según variable") {
        geom_sf(data = capa(), aes_string(size = input$size), fill = capa()$color_hex, color = input$shape, alpha = input$alpha)
      } else if (input$vector == "Polígonos" & input$opcionColor == "Personalizado" & input$opcionShape == "Único" & input$opcionSize == "Único") {
        geom_sf(data = capa(), size = as.numeric(input$size), fill = capa()$color_hex, color = input$shape, alpha = input$alpha)
      }
    })
    
    refs <- eventReactive(list(input$referencias, input$sizeRef, input$labelRef),{
      if (input$referencias=="Ninguna") {
        return(NULL)
      } else if (input$referencias!="Ninguna" & input$labelRef=="Texto") {
        ggrepel::geom_text_repel(data=capa(), aes_string(label= input$referencias, geometry="geometry"), 
                                 stat = "sf_coordinates", size = as.numeric(input$sizeRef),
                                 box.padding = 0.1, min.segment.length = 1, max.overlaps = 200)
      } else if (input$referencias!="Ninguna" & input$labelRef=="Etiqueta") {
        ggrepel::geom_label_repel(data=capa(), aes_string(label= input$referencias, geometry="geometry"), 
                                  stat = "sf_coordinates", size = as.numeric(input$sizeRef),
                                  box.padding = 0.1, min.segment.length = 1, max.overlaps = 200)
      }
      
    })
    
    click <- eventReactive(input$btnCapa, {
      if(input$btnCapa){
        "Click"
      } else {
        "No click"
      }
    }, ignoreNULL = FALSE)
    
    return(list(a = reactive(layer()),
                b = reactive(click()),
                c = reactive(refs()),
                d= reactive(input$file)))
    
  })
}

