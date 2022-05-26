shinyUI(
    navbarPage("MapeAr",
               tabPanel("MAPA",
                        div(
                          tags$head(
                            includeCSS("style.css")
                          )),
                        sidebarLayout(
                        sidebarPanel(width = 6, style="overflow: auto;
                                     max-height: 750px;",
                                     useShinyjs(),
                                     br(),
                            wellPanel(h3("CAPA BASE"),
                            fluidRow(
                              column(selectInput("provincia",
                                             label = "Filtrar provincias", 
                                             choices = c("País",unique(mapa_arg$name_iso)),
                                             multiple = T, selected = "País"), width = 6),
                            column(3, colourInput("fill_arg", "Relleno", value = "#FFFFFF", returnName = F)),
                            column(3, selectInput("color_arg", "Líneas", selected = "black", 
                                                  choices = c("Negro"="black",
                                                              "Gris"="grey",
                                                              "Blanco"="white",
                                                              "Rojo"="red",
                                                              "Azul"="blue",
                                                              "Amarillo"="yellow",
                                                              "Verde"="green")))),
                            
                            fluidRow(
                            column(6,checkboxInput("sudamerica", 
                                                 label = "Agregar continente", value = F)),
                            column(6,checkboxInput("deptos", 
                                                   label = "Agregar departamentos", value = F))),
                            fluidRow(
                            column(4,checkboxInput("refProv", 
                                          label = "Agregar etiqueta de provincia", value = F)),
                            
                            column(4, selectInput("color_deptos", "Color", selected = "black", 
                                                  choices = c("Negro"="black",
                                                              "Gris"="grey",
                                                              "Blanco"="white",
                                                              "Rojo"="red",
                                                              "Azul"="blue",
                                                              "Amarillo"="yellow",
                                                              "Verde"="green"))),
                            column(4,sliderInput("refProvSize", label = "Tamaño", 
                                                 value = 1, min = 0.5, max = 7, step = 0.5))
                            )), 
                            
                            fluidRow(
                            column(4,selectInput("preCapas", label = "Capas predefinidas", #multiple = T, 
                                        choices = c("Ninguna","Regiones","Rutas Naturales","Circuitos",
                                                    "Áreas Protegidas", "Vías Nacionales"))),
                            column(4,sliderInput("size_pre", label = "Grosor", 
                                                 value = 1, min = 0, max = 3, step = 0.2)),
                            column(4,sliderInput("alpha_pre", label = "Opacidad", 
                                                 value = 1, min = 0, max = 1, step = 0.1))),
                            
                            wellPanel(id = "panel1", h3("CAPA 1"), capasUI("layer1")),
                            wellPanel(id = "panel2", h3("CAPA 2"), capasUI("layer2")),
                            wellPanel(id = "panel3", h3("CAPA 3"), capasUI("layer3")),
                          
                            fluidRow(
                              column(6,radioButtons("formatoMapa", label = "Exportar en", 
                                         choices = c("png","jpg","svg","pdf"), inline = T)),
                              column(2,numericInput("widthMap", label = "Ancho", min = 3, max = 12, value = 6)),
                              column(2,numericInput("heightMap", label = "Alto", min = 3, max = 12, value = 8)),
                              column(2,numericInput("dpiMap", label = "Dpi", min = 50, max = 600, value = 200)),
                            ),
                            
                            downloadButton(outputId = 'downloadMap', label = 'Descargar mapa')),
                        
                        mainPanel(width = 6,
                                  plotOutput("mapa", width = 600, height = 700))
                        )
                        ),
               tabPanel("¿CÓMO USAR?")
               )
    )
