Sys.setlocale(locale = "Spanish")

AOAvancesUI = function(id) {
  
  ns <- NS(id)
  
  tabPanel(strong("Avances"),value = "panel3",
           fluidRow(
             column(5,
                    fluidRow(
                      column(2),
                      column(4,
                             filtroEtiquetasDHIME(ns('variableX'),'ETIQUETA DHIME')
                      ),
                      column(4,
                             filtroAnios(ns('anio2'), 'ANIO')
                      ),
                      column(2)
                    ),
                    fluidRow(
                      br(),
                      br(),
                      plotlyOutput(ns('grafico3'),height = "330px")
                    )
             ),
             column(7,
                    fluidRow(
                      column(10),
                      column(2,
                             botonAyuda(ns("ayuda6"))                                   )
                    ),
                    setDownTable("Comparación de porcentajes de digitación de Areas Operativas con descargas anteriores",ns("tabla5"),ns("downloadTabla5"),titleStatic = TRUE)
             ),
             fluidRow(
               column(5,
                      fluidRow(
                        column(9,
                               selectInput(ns('mesB'),
                                           label = 'mes',
                                           choices = c("Enero" = "01","Febrero" = "02",
                                                       "Marzo" = "03","Abril" = "04",
                                                       "Mayo" = "05","Junio" = "06",
                                                       "Julio" = "07","Agosto" = "08",
                                                       "Septiembre" = "09","Octubre" = "10",
                                                       "Noviembre" = "11","Diciembre" = "12"))
                        ),
                        column(1,
                               botonAyuda(ns("ayuda7"))
                        )
                      ),
                      fluidRow(
                        h4(strong(textOutput(ns("textNN"))), align = "center"),
                        plotlyOutput(ns('grafico4'),height = "330px")
                      )
                      
               ),
               column(7,
                      fluidRow(
                        column(7,
                               filtroV1AreasOperativas(ns('AO_ModAvances'), "AREA OPERATIVA")
                        ),
                        column(5,
                               botonAyuda(ns("ayuda24"))
                        )
                        
                      ),
                      fluidRow(
                        tabsetPanel(
                          tabPanel(strong("Mapa"),
                                   leafletOutput(ns("mapAvances"))
                          ),
                          tabPanel(strong("Tabla"),
                                   div(dataTableOutput(ns("tablaAvancesEst")),style = "font-size:70%"),
                                   downloadButton(ns("downloadTablaAvancesEst"))
                          )
                        )
                        
                      )
               )
             )
           )
  )
}


AOAvances = function(input, output, session) {
  mesesB = c("Enero" = "01","Febrero" = "02","Marzo" = "03","Abril" = "04",
             "Mayo" = "05","Junio" = "06","Julio" = "07","Agosto" = "08",
             "Septiembre" = "09","Octubre" = "10","Noviembre" = "11","Diciembre" = "12")
  
  observeEvent(input$ayuda24, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","La tabla contiene informacion de las estaciones filtradas por etiqueta DHIME, Anio, Mes y Area Operativa con contenido relacionado con la cantidad y porcentaje de datos por descarga para cada estacion, asi como un campo con el calculo del Avance que es el calculo del porcentaje diferencial de ambos porcentajes de datos ya mencionados.",type = "info")
  })
  
  observeEvent(input$ayuda7, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Anio, Mes y etiqueta DHIME, la grafica representa la cantidad de registros de la etiqueta seleccionadas que fueron analizados en el proceso del calculo de los avances.\n Los registros de las descargas usadas y se encuentran agrupados por Area Operativa.",type = "info")
  })
  
  observeEvent(input$ayuda6, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Anio y etiqueta DHIME, la gráfica (izquierda) y la tabla (derecha) resumen por Area Operativa los avances de digitacion en DHIME, representado como la diferencia de los porcentajes de digitacion por mes de dos descargas realizadas a la etiqueta seleccionada y para el anio seleccionado.",type = "info")
  })
  
  output$textNN <- renderText({
    paste("Cantidad de registros en el mes de ",names(mesesB)[mesesB == input$mesB]," del anio ",input$anio2," para las ultimas dos descargas de la etiqueta ",input$variableX)
  })
  
  
  TABLA_E = reactive({
    TABLA_E = TABLA_5 %>% filter(anio == input$anio2 & variable == input$variableX)
    TABLA_E$variable = NULL
    TABLA_E$anio = NULL
    TABLA_E$AO = substr(TABLA_E$AO,16,17)
    TABLA_E
  })
  output$tabla5 = renderDataTable({datatable(TABLA_E(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla5 <- downloadButtonTable(TABLA_E())

  VARIACIONES_1 = reactive({
    MM = GRAF_1_AVANCE
    unionTemp = MM %>% filter(variable == input$variableX & anio == input$anio2)
    unionTemp = unionTemp[order(unionTemp$mes),]
    return(unionTemp)
  })
  
  VARIACIONES_2 = reactive({
    NN = GRAF_2_AVANCE
    
    consolidadoConteoTemp = NN %>% filter(variable == input$variableX & anio == input$anio2 & mes == input$mesB)
    consolidadoConteoTemp$AO = paste0("AO_",substr(consolidadoConteoTemp$AO,16,17))
    
    return(consolidadoConteoTemp)
    
  })
  
  output$grafico3 = renderPlotly({
    graph = plot_ly(VARIACIONES_1(),x = ~mes,y = ~AO01_dif,name = 'AO <b>01</b>',text = ~paste0("Conteo datos penúltima descarga:",AO01_conteoDatosMesAnt,"\n Conteo datos ultima descarga:",AO01_conteoDatosMesAct),type = 'scatter',mode = 'lines+markers') %>%
      add_trace(y = ~AO02_dif,name = 'AO <b>02</b>',text = ~paste0("Conteo datos descarga anterior:",AO02_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO02_conteoDatosMesAct)) %>%
      add_trace(y = ~AO03_dif,name = 'AO <b>03</b>',text = ~paste0("Conteo datos descarga anterior:",AO03_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO03_conteoDatosMesAct)) %>%
      add_trace(y = ~AO04_dif,name = 'AO <b>04</b>',text = ~paste0("Conteo datos descarga anterior:",AO04_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO04_conteoDatosMesAct)) %>%
      add_trace(y = ~AO05_dif,name = 'AO <b>05</b>',text = ~paste0("Conteo datos descarga anterior:",AO05_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO05_conteoDatosMesAct)) %>%
      add_trace(y = ~AO06_dif,name = 'AO <b>06</b>',text = ~paste0("Conteo datos descarga anterior:",AO06_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO06_conteoDatosMesAct)) %>%
      add_trace(y = ~AO07_dif,name = 'AO <b>07</b>',text = ~paste0("Conteo datos descarga anterior:",AO07_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO07_conteoDatosMesAct)) %>%
      add_trace(y = ~AO08_dif,name = 'AO <b>08</b>',text = ~paste0("Conteo datos descarga anterior:",AO08_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO08_conteoDatosMesAct)) %>%
      add_trace(y = ~AO09_dif,name = 'AO <b>09</b>',text = ~paste0("Conteo datos descarga anterior:",AO09_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO09_conteoDatosMesAct)) %>%
      add_trace(y = ~AO10_dif,name = 'AO <b>10</b>',text = ~paste0("Conteo datos descarga anterior:",AO10_conteoDatosMesAnt,"\n Conteo datos descarga actual:",AO10_conteoDatosMesAct)) %>%
      add_trace(y = ~AO11_dif,name = 'AO <b>11</b>',text = ~paste0("Conteo datos descarga anterior:",AO11_conteoDatosMesAnt,"\n Conteo año anterior:",AO11_conteoDatosMesACt)) %>%
      layout(yaxis = list(title = 'Diferencia de porcentajes (%)'))
  })
  
  output$grafico4 = renderPlotly({
    graph2 =  plot_ly(VARIACIONES_2(),x = ~AO,y = ~conteoDatosMesAnt, type = 'bar', textposition = 'auto',name = "Cantidad datos descarga anterior", text = ~paste0("% Diferencia: ",avanceV2," %")) %>%
      add_trace(y = ~conteoDatosMesAct, textposition = 'auto',name = "Cantidad datos descarga actual", text = ~paste0("% Diferencia: ",avanceV2," %")) %>%
      layout(yaxis = list(title = 'Cantidad de datos'),legend = list(orientation = 'h',y = 1.13))
  })
  
  AVANCES_EST = reactive({
    AVANC = AVANCE_ESTACION %>% filter(etiqueta == input$variableX & anio == input$anio2 & mes == names(mesesB)[mesesB == input$mesB])
    AVANC = merge(AVANC,CNE_IDEAM[,c("CODIGO","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","ESTADO")],by.x = "Codigo",by.y = "CODIGO",all.x = T)
    AVANC$AREA_OPERATIVA = substr(AVANC$AREA_OPERATIVA,1,17)
    AVANC = AVANC %>% filter(AREA_OPERATIVA == input$AO_ModAvances)
    AVANC = AVANC[,c("Codigo","anio","mes","etiqueta","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","ESTADO","conteoDatosAnt","conteoDatosAct","PorcAnt(%)","PorcAct(%)","Avance(%)")]
    return(AVANC)
  })
  
  output$tablaAvancesEst = renderDataTable({datatable(AVANCES_EST(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaAvancesEst <- downloadButtonTable(AVANCES_EST())
  
  
  output$mapAvances = renderLeaflet({
    
    AVANCS = AVANCES_EST()
    AVANCS = merge(AVANCS,CNE_IDEAM[,c("CODIGO","latitud","longitud")],by.x = "Codigo",by.y = "CODIGO",all.x = T)
    colnames(AVANCS) = c("Codigo","anio","mes","etiqueta","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","ESTADO","conteoDatosAnt",
                         "conteoDatosAct","PorcAnt","PorcAct","Avance","latitud","longitud")
    AVANCS$RANGO = NA
    AVANCS$RANGO[which(AVANCS$Avance == 0)] = "0%"
    AVANCS$RANGO[which(AVANCS$Avance > 0 & AVANCS$Avance <= 5)] = "0% a 5%"
    AVANCS$RANGO[which(AVANCS$Avance < 0 & AVANCS$Avance >= -5)] = "-5% a 0%"
    AVANCS$RANGO[which(AVANCS$Avance > 5 & AVANCS$Avance <= 10)] = "5% a 10%"
    AVANCS$RANGO[which(AVANCS$Avance < -5 & AVANCS$Avance >= -10)] = "-10% a -5%"
    AVANCS$RANGO[which(AVANCS$Avance > 10)] = "> 10%"
    AVANCS$RANGO[which(AVANCS$Avance < -10)] = "< -10%"
    
    pal = colorFactor(palette = c("#ff5e04","#fbac05","#ffe38b","#000000","#c8f4ff","#0479ff","#0505ff"),
                      levels = c("< -10%","-10% a -5%","-5% a 0%","0%","0% a 5%","5% a 10%","> 10%"))
    
    leaflet(AVANCS) %>%
      addTiles() %>%
      addCircleMarkers(lng=AVANCS$longitud,lat=AVANCS$latitud,color = ~pal(RANGO),opacity = 0.8, radius = 6,label =paste("codigo:", AVANCS$Codigo, "<br>",
                                                                                                                         "nombre:", AVANCS$nombre, "<br>",
                                                                                                                         "municipio:", AVANCS$MUNICIPIO, "<br>",
                                                                                                                         "departamento:", AVANCS$DEPARTAMENTO, "<br>",
                                                                                                                         "estado", AVANCS$ESTADO , "<br>",
                                                                                                                         "cantidad_datos_anterior_descarga:", AVANCS$conteoDatosAnt , " datos <br>",
                                                                                                                         "porcentaje_datos_anterior_descarga:", AVANCS$PorcAnt  , " % <br>",
                                                                                                                         "cantidad_datos_ultima_descarga:", AVANCS$conteoDatosAct, " datos <br>",
                                                                                                                         "porcentaje_datos_ultima_descarga:", AVANCS$PorcAct  , " % <br>",
                                                                                                                         "Avance:", AVANCS$Avance, " % <br>") %>%
                         lapply(htmltools::HTML)
      ) %>% 
      
      leaflet::addLegend(pal = pal, values = AVANCS$RANGO, position = "topleft")
    
  })
}
