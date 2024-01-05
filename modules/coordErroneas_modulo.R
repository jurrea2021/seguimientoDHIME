Sys.setlocale(locale = "Spanish")

coordErroneasUI = function(id) {
  ns <- NS(id)
  
  tabPanel(strong("Coordenadas Erróneas"),value = "panel7",
           fluidRow(
             column(2,
                    botonAyuda(ns("ayuda28")),
                    br(),
                    filtroV1AreasOperativas(ns('AOCoordsErroneas'), "AREA OPERATIVA")
             ),
             column(3,
                    p(strong("Cantidad Estaciones:")),
                    verbatimTextOutput(ns("TableText3"))
             ),
             column(7,
                    p(strong("La tabla enlista las estaciones con diferencias de coordenadas en los catálogos de los años 2014 y 2021. Además de la información básica de cada estación en estas versiones de 
                               catálogos, los últimos campos describen para cada estación la distancia en metros que hay entre sus distintas versiones de ubicaciones así como las orientaciones entre las mismas.") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:15px;border-radius:10px")
             )
           ),
           tabsetPanel(
             tabPanel(strong("Catálogo estaciones con coordenadas erróneas"),
                      tabPanel(
                        strong("Tabla"),
                        box(div(dataTableOutput(ns("tablaCoordsMalas")),style = "font-size:70%"),width = 12),
                        downloadButton(ns("downloadTabla27"))
                      )
             ),
             tabPanel(strong("MAPA"),
                      # fluidRow(
                      #   column(7),
                      #   column(3,
                      #          selectInput(inputId = "selEstacionErroneas",
                      #                      label = "Seleccione código de la estación de interés",
                      #                      choices = NULL)
                      #          ),
                      #   column(2)
                      # ),
                      fluidRow(
                        column(5,
                               br(),
                               p(strong("A partir del filtro de códigos de estaciones que está encima del mapa, el mapa se actualiza con la estación seleccionada. Aparecerán dos ubicaciones asociadas a la estación 
                                   para el Catálogo del 2014 (",shiny::span(strong("AZUL"), style = "color:blue"),") y el Catálogo del 2021 (",shiny::span(strong("ROJO"), style = "color:red"),"). Al pasar el cursor del mouse sobre 
                                   el lienzo del mapa, aparecerá en la esquina superior derecha del mapa las coordenadas del mapa en donde se encuentra el mouse sin embargo, con Ctrl + Click  también obtendrá
                                   las coordenadas en el portapapeles para que pegue el texto de las coordenadas en otro lado.") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:10px;border-radius:10px"),
                               p(strong("El mapa también presenta herramientas relacionadas con la medición de distancias (Esquina superior Derecha), y dibujo de polígonos en el mapa (Izquierda) junto con un Menú 
                                          para prender o apagar las capas y los mapas base (Google Maps e Imágenes de Satélite) que están en la Derecha del mapa. ¡¡ Ayudará para la toma de decisiones al momento de 
                                          escoger unas adecuadas coordenadas!!") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:10px;border-radius:10px"),
                               p(strong("Finalmente, puede informar al grupo de Planeación Operativa o por Mesa de Ayuda la propuesta de nuevas coordenadas para la estación de interés, así se evaluará desde
                                          el grupo y se actualizará el Catálogo de Estaciones del IDEAM del 2021 con esas nuevas coordenadas.") ,style="text-align:justify;color:black;background-color:#C7C9E8;padding:10px;border-radius:10px")
                        ),
                        column(7,
                               fluidRow(
                                 column(4),
                                 column(4,
                                        selectInput(inputId = ns("selEstacionErroneas"),
                                                    label = "Seleccione estación",
                                                    choices = NULL)
                                 ),
                                 column(2),
                                 column(2,
                                        botonAyuda(ns("ayuda29"))
                                 )
                               ),
                               leafletOutput(ns("mapCoordsErroneas"))
                        )
                      )
             )
           )
  )
  
}

coordErroneas = function(input, output, session) {
  
  observeEvent(input$ayuda28, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","Con el filtro de 'Área Operativa' se puede depurar la tabla con las estaciones con probables coordenadas erróneas del Área Operativa de interés. A la derecha se encuentra un contador de estaciones activas, suspendidas y en mantenimiento",type = "info")
  })
  
  observeEvent(input$ayuda29, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","El filtro de 'Código' se actualiza de acuerdo al filtro 'Área Operativa' que se encuentra arriba.",type = "info")
  })
  
  coordsMalas = reactive({
    CNE_UNION_A = CNE_UNION %>% filter(AREA_OPERATIVA == input$AOCoordsErroneas)
    return(CNE_UNION_A)
  })
  
  coordsMalasTabla = reactive({
    dd = coordsMalas()
    dd$DIVIPOLA = NULL
    dd$MUNICIPIO_SHAPE = NULL
    dd$DEPTO_SHAPE = NULL
    return(dd)
  })
  
  output$tablaCoordsMalas = ({
    renderDataTable({datatable(coordsMalasTabla(),options = list(scrollX = T,pageLength = 10),rownames = F)})
  })
  output$downloadTabla27 <- downloadButtonTable(coordsMalasTabla())
  
  observe({
    updateSelectInput(session, inputId = "selEstacionErroneas",label = "Seleccione código", 
                      choices = c(coordsMalas()$codigo))
  })
  
  output$TableText3 = renderPrint({
    coordsMalasZ = coordsMalas()
    table(coordsMalasZ$estadoCNE_2021)
  })
  
  output$mapCoordsErroneas = renderLeaflet({
    
    COORDSMALAS_A = coordsMalas()[,c("codigo","nombreCNE_2014","longitudCNE_2014","latitudCNE_2014","entidadCNE2014")]
    COORDSMALAS_A = COORDSMALAS_A %>% filter(codigo == input$selEstacionErroneas)
    # COORDSMALAS_B = coordsMalas()[,c("codigo","nombreCNE_2021","longitudCNE_2021","latitudCNE_2021","entidadCNE2021","municipioCNE_2021","departamentoCNE_2021","estadoCNE_2021","DIVIPOLA","MUNICIPIO_SHAPE","DEPTO_SHAPE")]
    COORDSMALAS_B = coordsMalas()[,c("codigo","nombreCNE_2021","longitudCNE_2021","latitudCNE_2021","entidadCNE2021","municipioCNE_2021","departamentoCNE_2021","estadoCNE_2021","DIVIPOLA","MUNICIPIO_SHAPE","DEPTO_SHAPE")]
    COORDSMALAS_B = COORDSMALAS_B %>% filter(codigo == input$selEstacionErroneas)
    
    Muni1 = Muni[Muni$MPIO_CCNCT == stringr::str_pad(COORDSMALAS_B$DIVIPOLA,5,pad = "0"),]
    Muni2 = Muni[which(Muni$MPIO_CNMBR == COORDSMALAS_B$MUNICIPIO_SHAPE & Muni$DPTO_CNMBR == COORDSMALAS_B$DEPTO_SHAPE),]
    
    if (is.na(stringr::str_pad(COORDSMALAS_B$DIVIPOLA,5,pad = "0")) & dim(Muni2)[1] == 0) {
      leaflet() %>%
        addTiles(group = "Google Maps") %>%
        addProviderTiles("Esri.WorldImagery",group = "WorldImagery") %>% addProviderTiles(providers$Stadia.StamenTonerLines,group = "WorldImagery") %>% addProviderTiles(providers$Stadia.StamenTonerLabels,group = "WorldImagery") %>% 
        addCircleMarkers(data = COORDSMALAS_A,group = "Catalogo 2014",lng=COORDSMALAS_A$longitudCNE_2014,lat=COORDSMALAS_A$latitudCNE_2014,color = "#0917F1",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_A$codigo, "<br>",
                                                                                                                                                                                                    "nombre:", COORDSMALAS_A$nombreCNE_2014, "<br>",
                                                                                                                                                                                                    "longitud:", COORDSMALAS_A$longitudCNE_2014, "<br>",
                                                                                                                                                                                                    "latitud:", COORDSMALAS_A$latitudCNE_2014, "<br>",
                                                                                                                                                                                                    "entidad:", COORDSMALAS_A$entidadCNE2014, "<br>"
        ) %>%
          lapply(htmltools::HTML)
        ) %>% 
        addCircleMarkers(data = COORDSMALAS_B,group = "Catalogo 2021",lng=COORDSMALAS_B$longitudCNE_2021,lat=COORDSMALAS_B$latitudCNE_2021,color = "#F1093A",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_B$codigo, "<br>",
                                                                                                                                                                                                    "nombre:", COORDSMALAS_B$nombreCNE_2021, "<br>",
                                                                                                                                                                                                    "longitud:", COORDSMALAS_B$longitudCNE_2021, "<br>",
                                                                                                                                                                                                    "latitud:", COORDSMALAS_B$latitudCNE_2021, "<br>",
                                                                                                                                                                                                    "entidad:", COORDSMALAS_B$entidadCNE2021, "<br>",
                                                                                                                                                                                                    "municipio:", COORDSMALAS_B$municipioCNE_2021, "<br>",
                                                                                                                                                                                                    "departamento:", COORDSMALAS_B$departamentoCNE_2021, "<br>",
                                                                                                                                                                                                    "estado:", COORDSMALAS_B$estadoCNE_2021, "<br>"
        ) %>%
          lapply(htmltools::HTML)
        ) %>%
        addDrawToolbar(
          editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
        ) %>% 
        addMeasure(primaryLengthUnit="kilometers",activeColor = "#F41A0C",completedColor = "#F1A709",primaryAreaUnit = "sqmeters",
                   secondaryAreaUnit = "hectares") %>%
        addLayersControl(
          baseGroups = c("Google Maps","WorldImagery"),
          overlayGroups = c("Catalogo 2014", "Catalogo 2021"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend("topright", 
                           colors =c("#0917F1","#F1093A"),
                           labels= c("Catalogo 2014","Catalogo 2021"),
                           opacity = 1) %>%
        leafem::addMouseCoordinates() %>%
        # leafem::clip2sfc()
        addScaleBar()
      
    } else if (isTRUE(is.na(COORDSMALAS_B$MUNICIPIO_SHAPE))) {
      leaflet() %>%
        addTiles(group = "Google Maps") %>%
        addProviderTiles("Esri.WorldImagery",group = "WorldImagery") %>% addProviderTiles(providers$Stadia.StamenTonerLines,group = "WorldImagery") %>% addProviderTiles(providers$Stadia.StamenTonerLabels,group = "WorldImagery") %>%
        addPolygons(data = Muni1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "green", fillOpacity = 0.3,group = "Municipio VERDADERO",label = ~ MPIO_CNMBR) %>%
        addCircleMarkers(data = COORDSMALAS_A,group = "Catalogo 2014",lng=COORDSMALAS_A$longitudCNE_2014,lat=COORDSMALAS_A$latitudCNE_2014,color = "#0917F1",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_A$codigo, "<br>",
                                                                                                                                                                                                    "nombre:", COORDSMALAS_A$nombreCNE_2014, "<br>",
                                                                                                                                                                                                    "longitud:", COORDSMALAS_A$longitudCNE_2014, "<br>",
                                                                                                                                                                                                    "latitud:", COORDSMALAS_A$latitudCNE_2014, "<br>",
                                                                                                                                                                                                    "entidad:", COORDSMALAS_A$entidadCNE2014, "<br>"
        ) %>%
          lapply(htmltools::HTML)
        ) %>% 
        addCircleMarkers(data = COORDSMALAS_B,group = "Catalogo 2021",lng=COORDSMALAS_B$longitudCNE_2021,lat=COORDSMALAS_B$latitudCNE_2021,color = "#F1093A",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_B$codigo, "<br>",
                                                                                                                                                                                                    "nombre:", COORDSMALAS_B$nombreCNE_2021, "<br>",
                                                                                                                                                                                                    "longitud:", COORDSMALAS_B$longitudCNE_2021, "<br>",
                                                                                                                                                                                                    "latitud:", COORDSMALAS_B$latitudCNE_2021, "<br>",
                                                                                                                                                                                                    "entidad:", COORDSMALAS_B$entidadCNE2021, "<br>",
                                                                                                                                                                                                    "municipio:", COORDSMALAS_B$municipioCNE_2021, "<br>",
                                                                                                                                                                                                    "departamento:", COORDSMALAS_B$departamentoCNE_2021, "<br>",
                                                                                                                                                                                                    "estado:", COORDSMALAS_B$estadoCNE_2021, "<br>"
        ) %>%
          lapply(htmltools::HTML)
        ) %>%
        addDrawToolbar(
          editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
        ) %>% 
        addMeasure(primaryLengthUnit="kilometers",activeColor = "#F41A0C",completedColor = "#F1A709",primaryAreaUnit = "sqmeters",
                   secondaryAreaUnit = "hectares") %>%
        addLayersControl(
          baseGroups = c("Google Maps","WorldImagery"),
          overlayGroups = c("Catalogo 2014", "Catalogo 2021","Municipio VERDADERO"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend("topright", 
                           colors =c("#0917F1","#F1093A","green"),
                           labels= c("Catalogo 2014","Catalogo 2021","Municipio VERDADERO"),
                           opacity = 1) %>%
        leafem::addMouseCoordinates() %>%
        # leafem::clip2sfc()
        addScaleBar()

    } else {
      leaflet() %>%
        addTiles(group = "Google Maps") %>%
        addProviderTiles("Esri.WorldImagery",group = "WorldImagery") %>% addProviderTiles(providers$Stadia.StamenTonerLines,group = "WorldImagery") %>% addProviderTiles(providers$Stadia.StamenTonerLabels,group = "WorldImagery") %>%
        addPolygons(data = Muni1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "green", fillOpacity = 0.3,group = "Municipio VERDADERO",label = ~ MPIO_CNMBR) %>%
        addPolygons(data = Muni2, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "orange", fillOpacity = 0.3,group = "Municipio FALSO",label = ~ MPIO_CNMBR) %>%
        addCircleMarkers(data = COORDSMALAS_A,group = "Catalogo 2014",lng=COORDSMALAS_A$longitudCNE_2014,lat=COORDSMALAS_A$latitudCNE_2014,color = "#0917F1",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_A$codigo, "<br>",
                                                                                                                                                                                                    "nombre:", COORDSMALAS_A$nombreCNE_2014, "<br>",
                                                                                                                                                                                                    "longitud:", COORDSMALAS_A$longitudCNE_2014, "<br>",
                                                                                                                                                                                                    "latitud:", COORDSMALAS_A$latitudCNE_2014, "<br>",
                                                                                                                                                                                                    "entidad:", COORDSMALAS_A$entidadCNE2014, "<br>"
        ) %>%
          lapply(htmltools::HTML)
        ) %>% 
        addCircleMarkers(data = COORDSMALAS_B,group = "Catalogo 2021",lng=COORDSMALAS_B$longitudCNE_2021,lat=COORDSMALAS_B$latitudCNE_2021,color = "#F1093A",opacity = 0.8, radius = 6,label =paste("codigo:", COORDSMALAS_B$codigo, "<br>",
                                                                                                                                                                                                    "nombre:", COORDSMALAS_B$nombreCNE_2021, "<br>",
                                                                                                                                                                                                    "longitud:", COORDSMALAS_B$longitudCNE_2021, "<br>",
                                                                                                                                                                                                    "latitud:", COORDSMALAS_B$latitudCNE_2021, "<br>",
                                                                                                                                                                                                    "entidad:", COORDSMALAS_B$entidadCNE2021, "<br>",
                                                                                                                                                                                                    "municipio:", COORDSMALAS_B$municipioCNE_2021, "<br>",
                                                                                                                                                                                                    "departamento:", COORDSMALAS_B$departamentoCNE_2021, "<br>",
                                                                                                                                                                                                    "estado:", COORDSMALAS_B$estadoCNE_2021, "<br>"
        ) %>%
          lapply(htmltools::HTML)
        ) %>%
        addDrawToolbar(
          editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
        ) %>% 
        addMeasure(primaryLengthUnit="kilometers",activeColor = "#F41A0C",completedColor = "#F1A709",primaryAreaUnit = "sqmeters",
                   secondaryAreaUnit = "hectares") %>%
        addLayersControl(
          baseGroups = c("Google Maps","WorldImagery"),
          overlayGroups = c("Catalogo 2014", "Catalogo 2021","Municipio VERDADERO","Municipio FALSO"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend("topright", 
                           colors =c("#0917F1","#F1093A","green","orange"),
                           labels= c("Catalogo 2014","Catalogo 2021","Municipio VERDADERO","Municipio FALSO"),
                           opacity = 1) %>%
        leafem::addMouseCoordinates() %>%
        # leafem::clip2sfc()
        addScaleBar()
      
    }
    
    
  })
}
