Sys.setlocale(locale = "Spanish")

AOSensoresConvencionalesUI = function(id) {
  
  ns <- NS(id)
  
  tabPanel(strong(" Sensores Convencionales "),value = "panel4",
           tabsetPanel(
             tabPanel(strong("Consulta por Estación"),
                      fluidRow(
                        column(4,
                               downloadButton(ns("report"), "Generate report"),
                               fluidRow(
                                 column(9,
                                        filtroV1AreasOperativas(ns("AOSCoompleto1"),"ÁREA OPERATIVA"),
                                        selectInput(inputId = ns("selEstacion"),
                                                    label = "Seleccione código de la estación de interés",
                                                    choices = NULL)
                                 ),
                                 column(1,
                                        botonAyuda(ns("ayuda8"))
                                 )
                               ),
                               h4(strong("Ubicación de la estación"), align = "center"),
                               leafletOutput(ns("map"))
                        ),
                        column(8,
                               h3(strong(textOutput(ns("data2"))), align = "center"),
                               br(),
                               tabsetPanel(
                                 tabPanel(strong("Tabla Anual"),
                                          br(),
                                          tabsetPanel(
                                            tabPanel(strong("Conteos"),
                                                     setDownTable("Cantidad de datos o días de la variable de interés en DHIME por año",ns("tabla4"),ns("downloadTabla4"),titleStatic = TRUE)
                                            ),
                                            tabPanel(strong("Porcentajes"),
                                                     setDownTable("Cantidad de porcentajes de datos o días de la variable de interés en DHIME por año",ns("tabla45"),ns("downloadTabla45"),titleStatic = TRUE)
                                            )
                                          )                                 ),
                                 tabPanel(strong("Tabla Mensual"),
                                          filtroAnios(ns('anio4'), 'AÑO'),
                                          setDownTable("Cantidad de datos o días de la variable de interés en DHIME por mes",ns("tablaY"),ns("downloadTablaY"),titleStatic = TRUE)
                                 )
                               )
                               # plotOutput('graficoB')
                               # dygraphOutput("dygraph1")
                               
                        )
                      )
             ),
             tabPanel(strong("Consulta General"),
                      fluidRow(
                        column(2,
                               br(),
                               filtroV1AreasOperativas(ns('AO_ModGeneral'), "AREA OPERATIVA")
                        ),
                        column(2,
                               br(),
                               filtroEtiquetasDHIME(ns('variable_ModGeneral'),'ETIQUETA DHIME')
                        ),
                        column(2,
                               br(),
                               filtroAnios(ns('anio_ModGeneral'), 'AÑO')
                        ),
                        column(3,
                               p(strong("Cantidad Estaciones:")),
                               verbatimTextOutput(ns("TableTextMesOrg"))
                        ),
                        
                        column(1,
                               fluidRow(downloadButton(ns("downloadTablaMesOrig"),label = "DownloadTable1")),
                               br(),
                               fluidRow(downloadButton(ns("downloadTablaMesAlte"),label = "DownloadTable2"))
                        ),
                        column(1),
                        column(1,
                               br(),
                               br(),
                               botonAyuda(ns("ayuda30"))
                        )
                      ),
                      fluidRow(
                        tabsetPanel(
                          tabPanel(strong("Porcentaje datos por mes"),
                                   div(dataTableOutput(ns("tablaMesOrigPorc")),style = "font-size:60%")
                          ),
                          tabPanel(strong("Cantidad datos por mes"),
                                   div(dataTableOutput(ns("tablaMesOrig")),style = "font-size:60%")
                          ),
                          tabPanel(strong("Cantidad dias con datos por mes"),
                                   div(dataTableOutput(ns("tablaMesAlte")),style = "font-size:60%")
                          )
                        )
                      )
             )
           ),
           
           
  )
}



AOSensoresConvencionales = function(input, output, session) {
  observeEvent(input$ayuda8, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir del filtro de Estación, se selecciona el código de la estación de interés y se ajusta el mapa de la izquierda con la ubicación de la estación y la Tabla Anual de la derecha con la cantidad de registros por año para cada etiqueta de DHIME disponible en la estacion.\n La tabla mensual que se encuentra en la segunda pestaña a la derecha, muestra la misma dinámica que la primera tabla pero por mes y se corresponde con el filtro de Mes.",type = "info")
  })
  
  
  observeEvent(input$ayuda30, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","La tabla contiene información de la cantidad de datos por mes para cada etiqueta de DHIME y para cada año desde el año 2010. Con los filtros de 'Área Operativa', 'Etiqueta DHIME', y 'Año' se ajusta el contenido de la tabla al momento de su visualización y de su descarga con el botón 'Download'.",type = "info")
  })
  
  listadoEstaciones = reactive({
    est_AO = CNE_IDEAMA %>% filter(AREA_OPERATIVA == input$AOSCoompleto1)
    listado = unique(conteoEtiquetas$codigo)
    listadoFiltrado = data.frame("estacion" = listado[which(listado %in% est_AO$CODIGO)],stringsAsFactors = F)
    return(listadoFiltrado)
  })
  
  observe({
    updateSelectInput(session, inputId = "selEstacion",label = "Seleccione código de la estación de interés", 
                      choices = c(listadoEstaciones()$estacion))
  })
  
  output$data2 = renderText({
    print(paste(CNE$nombre[CNE$CODIGO == input$selEstacion],"|",CNE$MUNICIPIO[CNE$CODIGO == input$selEstacion],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$selEstacion],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$selEstacion]))
  })
  
  TABLA_D = reactive({
    TABLA_D = TABLA_4 %>% filter(CodigoEstacion == input$selEstacion)
    TABLA_D$anio = NULL
    TABLA_D$AREA_OPERATIVA = NULL
    TABLA_D$CodigoEstacion = NULL
    TABLA_D
  })
  output$tabla4 = renderDataTable({datatable(TABLA_D(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla4 <- downloadButtonTable(TABLA_D())
  
  TABLA_LL = reactive({
    TABLA_LL = TABLA_45 %>% filter(CodigoEstacion == input$selEstacion)
    TABLA_LL$anio = NULL
    TABLA_LL$AREA_OPERATIVA = NULL
    TABLA_LL$CodigoEstacion = NULL
    TABLA_LL
  })
  output$tabla45 = renderDataTable({datatable(TABLA_LL(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla45 <- downloadButtonTable(TABLA_LL())
  
  TABLA_F = reactive({
    TABLA_F = TABLA_Y %>% filter(CodigoEstacion == input$selEstacion & anio == input$anio4)
    TABLA_F$anio = NULL
    TABLA_F$AREA_OPERATIVA = NULL
    TABLA_F$CodigoEstacion = NULL
    TABLA_F
  })
  output$tablaY = renderDataTable({datatable(TABLA_F(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaY <- downloadButtonTable(TABLA_F())
  
  TABLA_MES_ORG = reactive({
    TABLA_YY = CONTEO_DATOS_MES_ORG %>% filter(AREA_OPERATIVA == input$AO_ModGeneral & etiqueta == input$variable_ModGeneral & anio == input$anio_ModGeneral)
    TABLA_YY$AREA_OPERATIVA = NULL
    TABLA_YY$etiqueta = NULL
    TABLA_YY
  })
  
  TABLA_MES_ORG_PORC = reactive({
    TABLA_II = CONTEO_DATOS_MES_ORG_PORC %>% filter(AREA_OPERATIVA == input$AO_ModGeneral & etiqueta == input$variable_ModGeneral & anio == input$anio_ModGeneral)
    TABLA_II$AREA_OPERATIVA = NULL
    TABLA_II$etiqueta = NULL
    TABLA_II
  })
  
  
  TABLA_MES_ALT = reactive({
    TABLA_XX = CONTEO_DATOS_MES_ALT %>% filter(AREA_OPERATIVA == input$AO_ModGeneral & etiqueta == input$variable_ModGeneral & anio == input$anio_ModGeneral)
    TABLA_XX$AREA_OPERATIVA = NULL
    TABLA_XX$etiqueta = NULL
    TABLA_XX
  })
  
  output$TableTextMesOrg = renderPrint({
    summMesOrg = TABLA_MES_ORG()
    table(summMesOrg$ESTADO)
  })
  
  output$tablaMesOrigPorc = renderDataTable({datatable(TABLA_MES_ORG_PORC(),options = list(scrollX = T,pageLength = 250),rownames = F) %>%
      formatStyle(c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"),
                  backgroundColor = styleInterval(c(0,10,20,30,40,50,60,70,80,90,100,10000),
                                                  c("rgb(136,6,9)",
                                                    "rgb(201,9,14)",
                                                    "rgb(248,111,108)",
                                                    "rgb(249,127,111)",
                                                    "rgb(251,166,118)",
                                                    "rgb(253,258,43)",
                                                    "rgb(254,227,130)",
                                                    "rgb(217,224,129)",
                                                    "rgb(164,208,126)",
                                                    "rgb(125,197,124)",
                                                    "rgb(100,190,123)",
                                                    "rgb(51,63,79)",
                                                    # "rgb(9,109,11)",
                                                    "rgb(51,63,79)")))
      })
  
  
  # output$downloadTablaMesOrig <- downloadButtonTable(TABLA_MES_ORG_PORC())
  
  output$tablaMesOrig = renderDataTable({datatable(TABLA_MES_ORG(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMesOrig <- downloadButtonTable(TABLA_MES_ORG())
  
  output$tablaMesAlte = renderDataTable({datatable(TABLA_MES_ALT(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  # output$tablaMesAlte = renderDataTable({datatable(TABLA_MES_ALT(),options = list(scrollX = T,pageLength = 25),rownames = F) %>% 
  #     formatStyle(c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"),
  #                 backgroundColor = styleInterval(c(0,10,20,30,40,50,60,70,80,90,100,10000),
  #                                                 c("rgb(248,105,107)","rgb(248,111,108)","rgb(249,127,111)","rgb(249,127,111)","rgb(251,166,118)",
  #                                                   "rgb(254,227,130)","rgb(217,224,129)","rgb(164,208,126)","rgb(125,197,124)","rgb(100,190,123)",
  #                                                   "rgb(51,63,79)")))})
  output$downloadTablaMesAlte <- downloadButtonTable(TABLA_MES_ALT())
  
  # Se incluye la vista de leaflet con la capa de las estaciones
  output$map = renderLeaflet({
    tre<-subset(CNE_IDEAM,CNE_IDEAM$CODIGO==input$selEstacion)
    # Graficamos el mapa resultante
    leaflet() %>%
      # addProviderTiles("Stamen.Toner") %>%
      addTiles() %>%
      # addPolygons() %>%
      addCircleMarkers(data=tre,lng=tre$longitud,lat=tre$latitud,label = tre$nombre,color="#0C00FF",weight = 3,radius=10,layerId = tre$OBJECTID) 
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report2.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report2.rmd")
      file.copy("./rmd/report2.rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = TABLA_F())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  
}