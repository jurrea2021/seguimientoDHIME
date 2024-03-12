Sys.setlocale(locale = "Spanish")

AOPorcentajesUI = function(id) {
  ns <- NS(id)
  
  tabPanel(strong("Porcentaje digitación"),value = "panel2",
           fluidRow(
             column(6,
                    fluidRow(
                      botonAyuda(ns("ayuda3"))
                    ),
                    fluidRow(
                      column(2),
                      column(4,
                             filtroEtiquetasDHIME(ns('variablePorc'),'ETIQUETA DHIME')
                      ),
                      column(4,
                             filtroAnios(ns('anioPorc'), 'AÑO')
                      ),
                      column(2)
                    ),
                    h4(strong(textOutput(ns("text12"))), align = "center"),
                    # h4("Porcentaje de digitación de datos por Área Operativa", align = "center"),
                    plotlyOutput(ns('graficoPorc')),
                    br(),
                    fluidRow(
                      column(2),
                      column(3,
                             botonAyuda(ns("ayuda5"))
                      )
                    )
             ),
             column(6,
                    fluidRow(
                      column(10),
                      column(1,
                             botonAyuda(ns("ayuda4"))
                      )
                    ),
                    tabsetPanel(
                      tabPanel("Tabla 1",
                               h4(strong(textOutput(ns("text13"))), align = "center"),
                               # h4("Metas anuales por Área Operativa con las estaciones que tuvieron registros en el año", align = "center"),
                               div(dataTableOutput(ns("tablaMeta")),style = "font-size:70%")
                      ),
                      tabPanel("Tabla 2",
                               h4(strong(textOutput(ns("text14"))), align = "center"),
                               # h4("Metas anuales por Área Operativa con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10)", align = "center"),
                               div(dataTableOutput(ns("tablaMetaBeta")),style = "font-size:70%")
                      )                      
                    )
             )
             
           ),
           fluidRow(
             column(1),
             column(10,
                    tabsetPanel(
                      tabPanel("Tabla 1",
                               h4(strong(textOutput(ns("text15"))), align = "center"),
                               # h4("Meses procesados por Área Operativa a escala mensual con las estaciones que tuvieron registros en el año", align = "center"),
                               div(dataTableOutput(ns("tablaMetaMes")),style = "font-size:70%")
                      ),
                      tabPanel("Tabla 2",
                               h4(strong(textOutput(ns("text16"))), align = "center"),
                               # h4("Meses procesados por Área Operativa a escala mensual con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10)", align = "center"),
                               div(dataTableOutput(ns("tablaMetaBetaMes")),style = "font-size:70%")
                      )                      
                    )
             ),
             column(1)
           )
  )
  

}



AOPorcentajes = function(input, output, session) {
  observeEvent(input$ayuda3, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año y etiqueta DHIME, se ajusta la gráfica correspondiente a los porcentajes y datos digitados para la etiqueta seleccionada, por cada mes del año seleccionado y por Área Operativa. Toma la cantidad de estaciones con registros de la etiqueta en el mes del Área Operativa.",type = "info")
  })
  
  observeEvent(input$ayuda4, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año y etiqueta DHIME, las tablas 1 y 2 describen las metas anuales por Área Operativa obtenidas con base al listado de las estaciones que las Áreas Operativas usan para enviar sus reportes por Intranet y el listado de las estaciones que reportaron datos de la etiqueta seleccionada en el Año seleccionado.",type = "info")
  })
  
  observeEvent(input$ayuda5, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año y etiqueta DHIME, las tablas 1 y 2 describen el total de meses procesados del listado de estaciones que las Áreas Operativas usan para enviar sus reportes por Intranet y el listado de estaciones que reportan datos de la etiqueta seleccionada en el Año seleccionado respectivamente.",type = "info")
  })
  
  output$text12 <- renderText({
    paste("Porcentaje de digitación de datos de ",input$variablePorc," por Área Operativa en el ",input$anioPorc)
  })
  
  output$text13 <- renderText({
    paste("Metas anuales por área Operativa con las estaciones que tuvieron registros de ",input$variablePorc,"  en el año ",input$anioPorc)
  })
  output$text14 <- renderText({
    paste("Metas anuales por Área Operativa con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10) para la etiqueta ",input$variablePorc,"  en el año ",input$anioPorc)
  })
  output$text15 <- renderText({
    paste("Meses procesados por Área Operativa a escala mensual con las estaciones que tuvieron registros de ",input$variablePorc," en el año ",input$anioPorc)
  })
  output$text16 <- renderText({
    paste("Meses procesados por Área Operativa a escala mensual con las estaciones reportadas en ORFEO (AO01,AO02,AO03,AO04,AO09,AO10) para la etiqueta ",input$variablePorc," en el año ",input$anioPorc)
  })
  
  PORCENTAJES = reactive({
    RR = TABLA_2S
    RR = RR %>% filter(VARIABLE == input$variablePorc & anio == input$anioPorc)
    return(RR)
  })
  
  TABLA_W = reactive({
    TABLA_R = TABLA_META_2 %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_R$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_R
  })
  output$tablaMetaBeta = renderDataTable({datatable(TABLA_W(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMetaBeta <- downloadButtonTable(TABLA_W())
  
  TABLA_P = reactive({
    TABLA_I = TABLA_META %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_I$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_I
  }) 
  output$tablaMeta = renderDataTable({datatable(TABLA_P(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMeta <- downloadButtonTable(TABLA_P())
  
  TABLA_GG = reactive({
    TABLA_U = TABLA_META_MES_2 %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_U$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_U
  })
  output$tablaMetaBetaMes = renderDataTable({datatable(TABLA_GG(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMetaBetaMes <- downloadButtonTable(TABLA_GG())
  
  TABLA_OK = reactive({
    TABLA_H = TABLA_META_MES %>% filter(anio == input$anioPorc & VARIABLE == input$variablePorc)
    TABLA_H$VARIABLE = NULL
    # colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_H
  })
  output$tablaMetaMes = renderDataTable({datatable(TABLA_OK(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaMetaMes <- downloadButtonTable(TABLA_OK())
  
  output$graficoPorc = renderPlotly({
    graph = plot_ly(PORCENTAJES(),x = ~AO,y = ~ENE,name = 'ENE',text = ~paste0("Datos digitados:",ENEconteo),type = 'scatter',mode = 'lines+markers') %>%
      add_trace(y = ~FEB,name = 'FEB',text = ~paste0("Datos digitados:",FEBconteo)) %>%
      add_trace(y = ~MAR,name = 'MAR',text = ~paste0("Datos digitados:",MARconteo)) %>%
      add_trace(y = ~ABR,name = 'ABR',text = ~paste0("Datos digitados:",ABRconteo)) %>%
      add_trace(y = ~MAY,name = 'MAY',text = ~paste0("Datos digitados:",MAYconteo)) %>%
      add_trace(y = ~JUN,name = 'JUN',text = ~paste0("Datos digitados:",JUNconteo)) %>%
      add_trace(y = ~JUL,name = 'JUL',text = ~paste0("Datos digitados:",JULconteo)) %>%
      add_trace(y = ~AGO,name = 'AGO',text = ~paste0("Datos digitados:",AGOconteo)) %>%
      add_trace(y = ~SEP,name = 'SEP',text = ~paste0("Datos digitados:",SEPconteo)) %>%
      add_trace(y = ~OCT,name = 'OCT',text = ~paste0("Datos digitados:",OCTconteo)) %>%
      add_trace(y = ~NOV,name = 'NOV',text = ~paste0("Datos digitados:",NOVconteo)) %>%
      add_trace(y = ~DIC,name = 'DIC',text = ~paste0("Datos digitados:",DICconteo)) %>%
      layout(yaxis = list(title = 'Porcentajes de digitación (%)'))
    
  })
}

