Sys.setlocale(locale = "Spanish")

AOSAUT_PT_AUT_10_UI = function(id) {
  ns <- NS(id)
  
  tabPanel(strong("PT_AUT_10"),value = "panel5",
           fluidRow(
             column(4,
                    filtroV1AreasOperativas(ns("AOSCoompleto2"),"AREA OPERATIVA"),
                    selectInput(inputId = ns("selecEstacion"),
                                label = "Seleccione codigo de la estacion de interes",
                                choices = NULL),
                    h4(strong("Ubicacion de la estacion"), align = "center"),
                    leafletOutput(ns("mapAut")),
                    filtroAnios(ns('anio3'), 'ANIO'),
                    h4("Analisis de cantidad de estaciones por rangos de dias de digitacion de datos anual", align = "center"),
                    plotlyOutput(ns('graficoPrecAut'))
             ),
             column(8,
                    tabsetPanel(
                      tabPanel(strong("TABLA ANUAL"),
                               br(),
                               tabsetPanel(
                                 tabPanel(strong("Conteos"),
                                          h5(textOutput(ns("data3")), align = "center"),
                                          setDownTable("Cantidad de dias con valores de PT_AUT_10 en DHIME por anio",ns("tabla8"),ns("downloadTabla8"),titleStatic = TRUE)
                                 ),
                                 tabPanel(strong("Porcentajes"),
                                          setDownTable("Porcentaje de dias con valores de PT_AUT_10 en DHIME por anio",ns("tabla88"),ns("downloadTabla88"),titleStatic = TRUE)
                                 )
                               )
                      ),
                      tabPanel(strong("TABLA MENSUAL"),
                               filtroAnios(ns('anio5'), 'AÑO'),
                               setDownTable("Cantidad de dias con valores de PT_AUT_10 en DHIME por mes",ns("tabla9"),ns("downloadTabla9"),titleStatic = TRUE)
                      )
                    )             )
           )
  )
}

AOSAUT_PT_AUT_10 = function(input, output, session) {
  
  listadoEstacionesB = reactive({
    est_AO = TABLA_88_CODS %>% filter(AREA_OPERATIVA == input$AOSCoompleto2)
    listadoFiltrado = data.frame("estacion" = est_AO$codigo,stringsAsFactors = F)
    return(listadoFiltrado)
  })
  
  observe({
    updateSelectInput(session, inputId = "selecEstacion",label = "Seleccione codigo de la estacion de interes", 
                      choices = c(listadoEstacionesB()$estacion))
  })
  
  output$data3 = renderText({
    print(paste(CNE$nombre[CNE$CODIGO == input$selecEstacion],"|",CNE$MUNICIPIO[CNE$CODIGO == input$selecEstacion],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$selecEstacion],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$selecEstacion]))
  })
  
  TABLA_I = reactive({
    TABLA_I = TABLA_9 %>% filter(Codigo == input$selecEstacion & anio == input$anio5)
    TABLA_I$Codigo = NULL
    TABLA_I$anio = NULL
    TABLA_I
  })
  
  output$mapAut = renderLeaflet({
    tre<-subset(CNE,CNE$CODIGO==input$selecEstacion)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data=tre,lng=tre$longitud,lat=tre$latitud,label = tre$nombre,color="#FF0017",weight = 3,radius=10,layerId = tre$OBJECTID)
  })
  
  TABLA_H = reactive({
    TABLA_H = TABLA_8 %>% filter(Codigo == input$selecEstacion)
    TABLA_H$Codigo = NULL
    TABLA_H
  })
  output$tabla8 = renderDataTable({datatable(TABLA_H(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla8 <- downloadButtonTable(TABLA_H())
  
  TABLA_HH = reactive({
    TABLA_HH = TABLA_88 %>% filter(Codigo == input$selecEstacion)
    TABLA_HH$Codigo = NULL
    TABLA_HH
  })
  output$tabla88 = renderDataTable({datatable(TABLA_HH(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla88 <- downloadButtonTable(TABLA_HH())
  
  output$tabla9 = renderDataTable({datatable(TABLA_I(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla9 <- downloadButtonTable(TABLA_I())
  
  RESUMEN_ORG_PREC_AUT = reactive({
    RESUMEN_ORG_H = RESUMEN_ORG_PT_AUT10 %>% filter(anio == input$anio3)
    RESUMEN_ORG_H$AREA_OPERATIVA = paste0("AO ",substr(RESUMEN_ORG_H$AREA_OPERATIVA,16,17))
    return(RESUMEN_ORG_H)
  })
  
  # Se genera la grafica del resumen de cantidad de estaciones de la etiqueta PT_AUT_10 por Areas Operativas
  output$graficoPrecAut = renderPlotly({
    fig <- plot_ly(RESUMEN_ORG_PREC_AUT(), x = ~AREA_OPERATIVA, y = ~COMPLETO, type = 'bar', name = 'COMPLETO',text = ~paste0("Porcentaje: ",round((COMPLETO/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig <- fig %>% add_trace(y = ~periodoA, name = '10 meses - 1 anio',text = ~paste0("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig <- fig %>% add_trace(y = ~periodoB, name = '8 meses - 10 meses',text = ~paste0("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig <- fig %>% add_trace(y = ~periodoC, name = '6 meses - 8 meses',text = ~paste0("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig <- fig %>% add_trace(y = ~periodoD, name = '4 meses - 6 meses',text = ~paste0("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig <- fig %>% add_trace(y = ~periodoE, name = '< 4 meses',text = ~paste0("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
  })
}
