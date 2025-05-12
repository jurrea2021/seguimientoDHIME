Sys.setlocale(locale = "Spanish")

AOSAUT_NV_AUT_60_UI = function(id) {
  ns <- NS(id)
  
  tabPanel(strong("NV_AUT_60"),
           fluidRow(
             column(4,
                    fluidRow(
                      p("A traves de este modulo, se pueden realizar consultas de informacion de ubicacion, cantidad de datos mensual y anual de la etiqueta NV_AUT_60
                                     para una estacion automatica en particular" ,style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")
                    ),
                    selectInput(inputId = ns("selecEstacionNV"),
                                label = "Seleccione codigo de la estacion de interes",
                                choices = NULL),
                    h4(strong("Ubicacion de la estacion"), align = "center"),
                    leafletOutput(ns("mapAutNV_AUT_60")),
                    filtroAnios(ns('anioNV'), 'AÑO'),
                    h4("Analisis de cantidad de estaciones por rangos de dias de digitacion de datos anual", align = "center"),
                    plotlyOutput(ns('graficoNVAut'))
             ),
             column(8,
                    h5(textOutput(ns("dataNVAut")), align = "center"),
                    setDownTable("Cantidad de dias con valores de NV_AUT_60 en DHIME por anio",ns("tabla8NV_AUT_60"),ns("downloadTabla8NV_AUT_60"),titleStatic = TRUE),
                    filtroAnios(ns('anio5NV_AUT_60'), 'AÑO'),
                    setDownTable("Cantidad de dias con valores de NV_AUT_60 en DHIME por mes",ns("tabla9NV_AUT_60"),ns("downloadTabla9NV_AUT_60"),titleStatic = TRUE)
             )
           )
  )
  
  
}

AOSAUT_NV_AUT_60 = function(input, output, session) {
  
  observe({
    updateSelectInput(session, inputId = "selecEstacionNV",label = "Seleccione codigo de la estacion de interes", 
                      choices = c(LISTADOESTACIONESNV_AUT_60$codigoEstacion))
  })
  
  TABLA_V = reactive({
    TABLA_V = TABLA8NV_AUT_60 %>% filter(Codigo == input$selecEstacionNV)
    TABLA_V$Codigo = NULL
    TABLA_V
  })
  output$tabla8NV_AUT_60 = renderDataTable({datatable(TABLA_V(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla8NV_AUT_60 <- downloadButtonTable(TABLA_V())
  
  output$mapAutNV_AUT_60 = renderLeaflet({
    treNV<-subset(CNE,CNE$CODIGO==input$selecEstacionNV)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data=treNV,lng=treNV$longitud,lat=treNV$latitud,label = treNV$nombre,color="#FF0017",weight = 3,radius=10,layerId = treNV$OBJECTID)
  })
  
  output$dataNVAut = renderText({
    print(paste(CNE$nombre[CNE$CODIGO == input$selecEstacionNV],"|",CNE$MUNICIPIO[CNE$CODIGO == input$selecEstacionNV],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$selecEstacionNV],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$selecEstacionNV]))
  })
  
  RESUMEN_ORG_NV_AUT = reactive({
    RESUMEN_ORG_D = RESUMEN_ORG_NV_AUT_60 %>% filter(anio == input$anioNV)
    RESUMEN_ORG_D$AREA_OPERATIVA = paste0("AO ",substr(RESUMEN_ORG_D$AREA_OPERATIVA,16,17))
    return(RESUMEN_ORG_D)
  })
  
  output$graficoNVAut = renderPlotly({
    fig <- plot_ly(RESUMEN_ORG_NV_AUT(), x = ~AREA_OPERATIVA, y = ~COMPLETO, type = 'bar', name = 'COMPLETO',text = ~paste0("Porcentaje: ",round((COMPLETO/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig <- fig %>% add_trace(y = ~periodoA, name = '10 meses - 1 anio',text = ~paste0("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig <- fig %>% add_trace(y = ~periodoB, name = '8 meses - 10 meses',text = ~paste0("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig <- fig %>% add_trace(y = ~periodoC, name = '6 meses - 8 meses',text = ~paste0("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig <- fig %>% add_trace(y = ~periodoD, name = '4 meses - 6 meses',text = ~paste0("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig <- fig %>% add_trace(y = ~periodoE, name = '< 4 meses',text = ~paste0("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
  })
  
  TABLA_KK = reactive({
    TABLA_KK = TABLA_9NV_AUT_60 %>% filter(Codigo == input$selecEstacionNV & anio == input$anio5NV_AUT_60)
    TABLA_KK$Codigo = NULL
    TABLA_KK$anio = NULL
    TABLA_KK
  })
  output$tabla9NV_AUT_60 = renderDataTable({datatable(TABLA_KK(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla9NV_AUT_60 <- downloadButtonTable(TABLA_KK())

}
