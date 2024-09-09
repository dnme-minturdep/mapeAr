shinyUI(
  
  navbarPage(title = div(div(
    id = "img-id",
    tags$a(img(src = "https://tableros.yvera.tur.ar/recursos/logo_sinta.png",
               width = 100),href="https://www.yvera.tur.ar/sinta/",target = '_blank'
    )),
    icon("map-location-dot"), "MapeAr", id = "title", class = "navbar1"),
    id="navbar",
    position = "fixed-top",
    windowTitle = "MapeAr", 
    collapsible = TRUE,
    header = includeCSS("style.css"),
    
    tabPanel("MAPA", 
             
             tags$head(includeHTML("/srv/DataDNMYE/login_shiny/mapear.html")),
             
             useWaiter(),
             waiter_show_on_load(html = loading_screen, color = "white"),
            
             br(),
             sidebarLayout(
               sidebarPanel(width = 6, style="overflow: auto;
                                     max-height: 750px;",
                            useShinyjs(),
                            br(),
                            
                            # Panel de capa base
                            bsCollapse(open = h3("CAPA BASE"),
                                       bsCollapsePanel(h3("CAPA BASE"), 
                                                       fluidRow(
                                                         column(selectInput("provincia",
                                                                            label = "Filtrar provincias", 
                                                                            choices = c("País",sort(unique(mapa_arg$name_iso))),
                                                                            multiple = T, selected = "País"), width = 6),
                                                         column(3, colourInput("fill_arg", "Relleno", value = "#FFFFFF", returnName = F)),
                                                         column(3, colourInput("color_arg", "Líneas", value = "#000000", returnName = F))),
                                                       
                                                       fluidRow(
                                                         column(6,checkboxInput("sudamerica", 
                                                                                label = "Agregar continente", value = F)),
                                                         column(6,checkboxInput("deptos", 
                                                                                label = "Agregar departamentos", value = F))),
                                                       wellPanel("Etiquetas de provincias",
                                                                 fluidRow(
                                                                   column(4,checkboxInput("refProv", 
                                                                                          label = "Agregar etiqueta", value = F)),
                                                                   
                                                                   column(4, colourInput("color_deptos", "Color", value = "#000000", returnName = F)),
                                                                   column(4,sliderInput("refProvSize", label = "Tamaño", 
                                                                                        value = 1, min = 0.5, max = 7, step = 0.5))
                                                                 )))), 
                            
                            # Panel de capa Rutas naturales
                            bsCollapsePanel(h4("CAPA REGIONES NATURALES"),
                                            selectInput("ruta_natural",
                                                        label = "Agregar regiones", 
                                                        choices = c("Ninguna","Todas",sort(unique(rutas_naturales_base$ruta_natural))),
                                                        multiple = T, selected = "Ninguna"),
                                            
                                            fluidRow(  
                                              column(6,sliderInput("alpha_rn", label = "Opacidad", 
                                                                   value = 1, min = 0, max = 1, step = 0.1)),
                                              column(6,  
                                                     sliderInput("size_rn", label = "Grosor", 
                                                                 value = 1, min = 0, max = 3, step = 0.2))
                                            ),
                                            
                                            checkboxInput("rutas_gris", 
                                                          label = "Completar regiones en gris", value = F)
                            ),
                            
                            # Panel de capas predefinidas
                            bsCollapsePanel(h4("CAPAS PREDEFINIDAS"),
                                            fluidRow(
                                              column(4,selectInput("preCapas", label = "Opciones de capa", #multiple = T, 
                                                                   choices = c("Ninguna","Regiones","Circuitos", "Circuitos subidos a la web",
                                                                               "Rutas Escénicas", "Áreas Protegidas", "Vías Nacionales","Capitales"))),
                                              column(4,sliderInput("size_pre", label = "Tamaño", 
                                                                   value = 1, min = 0, max = 7, step = 0.2)),
                                              column(4,sliderInput("alpha_pre", label = "Opacidad", 
                                                                   value = 1, min = 0, max = 1, step = 0.1))),
                                            fluidRow(
                                              column(4,selectInput("preCapas2", label = "Opciones de capa", #multiple = T, 
                                                                   choices = c("Ninguna","Regiones","Circuitos", "Circuitos subidos a la web",
                                                                               "Rutas Escénicas", "Áreas Protegidas", "Vías Nacionales","Capitales"))),
                                              column(4,sliderInput("size_pre2", label = "Tamaño", 
                                                                   value = 1, min = 0, max = 7, step = 0.2)),
                                              column(4,sliderInput("alpha_pre2", label = "Opacidad", 
                                                                   value = 1, min = 0, max = 1, step = 0.1)))),
                            
                            HTML("Antes de cargar un archivo, consulte la sección <b>¿CÓMO USAR?</b> para verificar que su base de datos cumple los requerimientos y recomendaciones."),
                            br(),br(),
                            
                            # Paneles de capas personalizadas
                            wellPanel(h4("CAPAS PERSONALIZADAS"),
                                      bsCollapsePanel(h4("CAPA 1"), capasUI("layer1")),
                                      bsCollapsePanel(h4("CAPA 2"), capasUI("layer2")),
                                      bsCollapsePanel(h4("CAPA 3"), capasUI("layer3"))
                            ),
                            
                            # Panel de descarga
                            fluidRow(
                              column(6,radioButtons("formatoMapa", label = "Exportar en", 
                                                    choices = c("png","jpg","svg","pdf"), inline = T)),
                              column(2,numericInput("widthMap", label = "Ancho", min = 3, max = 12, value = 6)),
                              column(2,numericInput("heightMap", label = "Alto", min = 3, max = 12, value = 8)),
                              column(2,numericInput("dpiMap", label = "Dpi", min = 50, max = 600, value = 200)),
                            ),
                            
                            checkboxInput("legendSwitch", label = "Mostrar leyenda"),
                            downloadButton(outputId = 'downloadMap', label = 'Descargar mapa'),
                            downloadButton(outputId = 'downloadLegend', label = 'Descargar leyenda')),
               
               # Panel de previsualización
               mainPanel(width = 6,
                         plotOutput("mapa", width = "100%", height = 700))
             )
    ),
    
    tabPanel(title = "¿CÓMO USAR?", 
             
             div(id="container-info",
                 
                 h3("USANDO DATOS ESPACIALES"),
                 h5("Para poder visualizar información en el mapa y descargarlos, se necesita contar con una base de datos", tags$b("espaciales")),
                 h5("Existen distintos tipos de datos que se pueden mapear en un plano:"),
                 
                 h5(tags$p(tags$b("      • Puntos"),": coordenadas geográficas, refieren a la latitud y longitud, donde se geolocaliza el dato. Por ejemplo, un aeropuerto o todos los museos de una ciudad."),
                    tags$p(tags$b("      • Líneas"),": una sucesión de puntos que forma una geometría con continuidad sobre el espacio. Un ejemplo lo conforman las vías terrestres, como la Ruta Nacional 40."), 
                    tags$p(tags$b("      • Polígonos"),": son geometrías cerradas, es decir, tienen límites específicos como un país, una provincia o las parques nacionales.")
                 ),
                 h5("Para crear tus bases de datos espaciales visitá el siguiente recurso:",  tags$a(href="https://geojson.io/", "geojson.io"),". Podés marcar geometrías en el territorio de Argentina, descargarlas y subirlas en la plataforma."),
                 
                 br(),
                 h3("MAPEANDO DATOS"),
                 h5("Antes de cargar la base de datos a visualizar, tenga en cuenta las siguientes recomendaciones:"),
                 h5(tags$ul(tags$p("  1. Para mapear puntos que están guardados en una base plana (del tipo .csv, .xlsx, .txt, etc.) deben existir dos columnas, una con la",tags$b("latitud"), "y otra con la", tags$b("longitud"), "del punto (o los puntos). En el caso de que se trabaje con una base de datos espacial (formatos .geojson, .kml, etc.), la misma debe tener la columna", tags$b("geometry"), "con la información geográfica (sean puntos, líneas o polígonos)."))),
                 
                 h5(tags$ul(tags$p("  2. En caso de querer asignar un color particular a una capa de datos, se debe definir una columna en la base de datos que especifique el código hexadecimal del color de cada registro, denominada", tags$b("color_hex"),". Por ejemplo, el negro se representa como #000000. Para consultar el código de los colores consulte este",  tags$a(href="https://htmlcolorcodes.com/es/", "recurso"),"."))),
                 
                 h5(tags$ul(tags$p("  3. Para asignar un tamaño a los puntos en función de una variable, la misma debe ser númerica."))),
                 
                 h5(tags$ul(tags$p("  4. La opción de agregar referencias a los puntos le permite utilizar una variable de texto de la base. Si desea que las referencias sean números, agregue una columna con los números de cada registro en formato texto."))),
                 
                 h5(tags$ul(tags$p("  5. Asegúrese de que la base no tengo registros faltantes (NA o missing values) en las variables que utilizará para mapear, por ejemplo en las coordenadas o la variable de color personalizado."))),
                 
                 h5(tags$ul(tags$p("  6. Las columnas de latitud y longitud deben estar escritas con un punto y sin comas. Por ejemplo: '-34.657852'"))),
                 
                 h5(tags$ul(tags$p("  7. La plataforma permite cargar hasta seis capas de datos, además de la capa base del país. Una de rutas naturales, dos predefinidas (como las áreas protegidas) y tres personalizadas. Tener en cuenta a la hora de armar el mapa, que cada capa que se suma se suporpone a la anterior. Así, la CAPA 1 va a tapar la CAPA BASE, y a su vez va a quedar por debajo de la CAPA 2."))),
                 
                 h5(tags$ul(tags$p("  8. Se pueden utilizar íconos para representar puntos en el mapa. En este caso, debe existir una columna denominada ", tags$b("icono"), " que contenga el nombre del mismo, con una referencia al repositorio correspondiente. Las opciones son ", tags$a(href="https://argob.github.io/poncho/identidad/iconos/", "Poncho"), " o ", tags$a(href="https://fontawesome.com/icons", "Font Awesome")," (visitar links para buscar los nombres de los íconos). Se debe escribir 'poncho' o 'fa' para referir a cada librería, seguido de un guión medio y el nombre del ícono. Ejemplo: 'poncho-aeropuerto' o 'fa-location-dot'. La resolución de las imágenes puede no ser óptima debido a configuraciones del sistema."))),
                 
                 h5(tags$ul(tags$p("  9. Se incorporó la opción de incluir una leyenda con las referencias correspondientes a las variables de mapeo utilizadas en las capas personalizadas. La misma se genera automáticamente de acuerdo a parámetros internos del sistema, en caso de querer personalizarla se sugiere descargar la misma por separado en formato svg, para editarla luego en un programa de diseño."))),
                 
                 h5(tags$ul(tags$p("A continuación puede descargar una base modelo, con algunos aeropuertos de Argentina, para tener de referencia a la hora de estructurar los datos a mapear."))),
                 
                 downloadButton(outputId = 'downloadData', label = 'Descargar base'),br(),br(),
                 
                 h5("Nota: las capas de áreas protegidas y vías nacionales se elaboraron en base a información del ", tags$a(href="https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG", "Instituto Geográfico Nacional.")," Visitar el sitio para conocer más sobre las distintas capas disponibles a nivel país.")
             )
    ),
    
    footer = includeHTML("/srv/shiny-server/recursos/shiny_footer.html")
  )
)
