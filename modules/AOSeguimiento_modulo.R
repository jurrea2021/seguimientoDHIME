Sys.setlocale(locale = "Spanish")

AOSeguimientoUI = function(id) {
  ns <- NS(id)

    tabPanel(strong(" Seguimiento "),value = "panel1",
             fluidRow(
               column(7,
                      
                      fluidRow(
                        column(1,
                               botonAyuda(ns("preview"))                                   ),
                        column(1),
                        column(4,
                               filtroEtiquetasDHIME(ns('variable'), 'ETIQUETA DHIME')
                        ),
                        column(6,
                               filtroAnios(ns('anio'), 'AÑO')
                        )
                      ),
                      fluidRow(
                        
                        h4(strong(textOutput(ns("text"))), align = "center"),
                        # h4("Análisis de cantidad de estaciones por rangos de días de digitación de datos anual para la etiqueta PTPM_CON", align = "center"),
                        plotlyOutput(ns('grafico'),height = "330px")
                      )
                      
               ),
               column(5,
                      selectInput(ns('mes'),
                                  label = "MES",
                                  choices = c("Enero" = 1,"Febrero" = 2,"Marzo" = 3,"Abril" = 4,
                                              "Mayo" = 5,"Junio" = 6,"Julio" = 7,"Agosto" = 8,
                                              "Septiembre" = 9,"Octubre" = 10,"Noviembre" = 11,"Diciembre" = 12)),
                      h4(strong(textOutput(ns("text2"))), align = "center"),
                      # h4("Análisis de cantidad de estaciones por rangos de días de digitación de datos mensual", align = "center"),
                      plotlyOutput(ns('grafico2'),height = "330px")
               )
             ),
             
             fluidRow(
               column(1,
                      botonAyuda(ns("ayuda2"))
               ),
               #column(1),
               column(4,
                      filtroV1AreasOperativas(ns('AO'), "AREA OPERATIVA"),
                      br()
               )
             ),
             fluidRow(
               column(7,
                      tabsetPanel(
                        tabPanel(strong("Mapa Anual"),
                                 h4(strong(textOutput(ns("text3"))), align = "center"),
                                 # div(tags$a(img(src="SimbologiaAnual.png", height=55,width= 800)),style = "position: relative; top: -4px",align ="center"),
                                 leafletOutput(ns("mapAnual"))
                        ),
                        tabPanel(strong("RESUMEN"),
                                 tabsetPanel(
                                   tabPanel(strong("Tabla Resumen"),
                                            div(style = "width:650px",verbatimTextOutput(ns("Text")))
                                   ),
                                   tabPanel(strong("Grafica cantidad"),
                                            plotlyOutput(ns('graficoCantidad'),height = "330px")
                                            )
                                 )
                        ),
                        tabPanel(strong("Tabla Estaciones con Registros en el Año"),
                                 setDownTable(ns("text5"),ns("tablaAnual"),ns("downloadTablaAnual"))
                        ),
                        tabPanel(strong("Tabla Estaciones Activas sin Registros en el Año"),
                                 setDownTable(ns("textNN"),ns("tablaFaltantesAnual"),ns("DownTablaFaltantesAnual"))
                        ),
                        tabPanel(strong("Tabla Anual 2"),
                                 setDownTable(ns("text8"),ns("RESUMEN_ORG_PORC_MES"),ns("downloadRESUMEN_ORG_PORC_MES"))
                        ),
                        tabPanel(strong("Tabla Anual 3"),
                                 setDownTable(ns("text10"),ns("ConteoMes"),ns("downloadConteoMes"))
                        ),
                        tabPanel(strong("Tabla Anual 4"),
                                 setDownTable(ns("text7"),ns("tablaQ"),ns("downloadTablaQ"))
                        ),
                        tabPanel(strong("Tabla Anual 5"),
                                 setDownTable(ns("text9"),ns("tablaT"),ns("downloadTablaT"))
                        ),
                        tabPanel(strong("Tabla Anual 6"),
                                 setDownTable(ns("text11"),ns("tablaS"),ns("downloadTablaS"))
                        )
                      )
               ),
               column(5,
                      tabsetPanel(
                        tabPanel(strong("Mapa Mensual"),
                                 h4(strong(textOutput(ns("text4"))), align = "center"),
                                 # div(tags$a(img(src="SimbologiaMes.png", height=55,width= 350)),style = "position: relative; top: -4px",align ="center"),
                                 leafletOutput(ns("mapMensual"))
                        ),
                        tabPanel(strong("Tabla Mensual 1"),
                                 setDownTable(ns("text6"),ns("tabla3"),ns("downloadTabla3"))
                        )
                      )
               )
             )
             
    )
  
}


AOSeguimiento = function(input, output, session) {
  #ns <- session$ns
  #######################################################################################################################
  # Areas Operativas ---> Seguimiento
  #######################################################################################################################
  
  meses = c("Enero" = 1,"Febrero" = 2,"Marzo" = 3,"Abril" = 4,
            "Mayo" = 5,"Junio" = 6,"Julio" = 7,"Agosto" = 8,
            "Septiembre" = 9,"Octubre" = 10,"Noviembre" = 11,"Diciembre" = 12)
  
  AreasOpAlt = c("01 - Medellin" = "Area Operativa 01","02 - Barranquilla" = "Area Operativa 02","03 - Villavicencio" = "Area Operativa 03","04 - Neiva" = "Area Operativa 04",
                 "05 - Santa Marta" = "Area Operativa 05","06 - Duitama" = "Area Operativa 06","07 - Pasto" = "Area Operativa 07","08 - Bucaramanga" = "Area Operativa 08",
                 "09 - Cali" = "Area Operativa 09","10 - Ibague" = "Area Operativa 10","11 - Bogota" = "Area Operativa 11")
  
  
  
  output$text <- renderText({
    paste("Cantidad de estaciones por rangos de días de digitación en el ",input$anio," para la etiqueta ", input$variable)
  })
  
  output$text2 <- renderText({
    paste("Cantidad de estaciones por rangos de días de digitación en ",names(meses)[meses == input$mes]," de ",input$anio," para la etiqueta", input$variable)
  })
  
  output$text3 <- renderText({
    paste("Estaciones del ",input$AO," con cantidad de datos de la etiqueta ",input$variable,"en el ",input$anio)
  })
  
  output$text4 <- renderText({
    paste("Estaciones del ",input$AO," con cantidad de datos de la etiqueta ",input$variable," en ",names(meses)[meses == input$mes]," de ",input$anio)
  })
  
  output$text5 <- renderText({
    paste("Cantidad de datos de la etiqueta ",input$variable," y rangos por estación para el ",input$AO," en el ",input$anio)
  })
  
  output$textNN <- renderText({
    paste("Estaciones Activas sin registros para la etiqueta ",input$variable," en el Área OPerativa ",input$AO," para el ",input$anio)
  })
  
  output$text6 <- renderText({
    paste("Cantidad de datos de la etiqueta ",input$variable," y rangos por estación para el ",input$AO," en ",names(meses)[meses == input$mes]," de ",input$anio)
  })
  
  output$text7 <- renderText({
    paste("Porcentaje de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," para el ",input$anio," en el DHIME.")
  })
  output$text8 <- renderText({
    paste("Porcentaje de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," en cada mes de ",input$anio," en el DHIME, en el ",input$AO)
  })
  output$text9 <- renderText({
    paste("Cantidad de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," para el ",input$anio," en el DHIME.")
  })
  output$text10 <- renderText({
    paste("Cantidad de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," en cada mes de ",input$anio," en el DHIME, en el ",input$AO)
  })
  output$text11 <- renderText({
    paste("Porcentaje de estaciones por cada periodo de digitación de datos de la etiqueta ",input$variable," por año en el DHIME para el ",input$AO)
  })
  
  
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "Las gráficas de barras apiladas indican por año (gráfica izquierda) y por mes (gráfica derecha) la cantidad de estaciones en cada rango de periodo de digitación de la etiqueta seleccionada, agrupados por Área Operativa.\n Están acondicionadas por los filtros de Año, Mes y Etiqueta DHIME.",type = "info")
  })
  
  observeEvent(input$ayuda2, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "A partir de los filtros de Año, Mes y Etiqueta DHIME, junto con el filtro de Área Operativa, se asocian los mapas y tablas de cantidades y porcentajes correspondientes a las estaciones que hacen parte de los rangos de digitación.\n Estos productos se encuentran a escala Anual (columna izquierda) y Mensual (columna derecha) organizados en pestañas.",type = "info")
  })
  
  ## Se filtra la tabla con la cantidad de estaciones por rangos y por años del 2010 al 2020
  RESUMEN_ORG_1 = reactive({
    RESUMEN_ORG_A = RESUMEN_ORG %>% filter(anio == input$anio & VARIABLE == input$variable)
    RESUMEN_ORG_A$AREA_OPERATIVA = paste0("AO ",substr(RESUMEN_ORG_A$AREA_OPERATIVA,16,17))
    return(RESUMEN_ORG_A)
  })
  
  RESUMEN_ORG_2 = reactive({
    TAB_GRAF_B = TAB_GRAF_2 %>% filter(anio == input$anio & VARIABLE == input$variable & mes == input$mes)
    TAB_GRAF_B$AREA_OPERATIVA = paste0("AO ",substr(TAB_GRAF_B$AREA_OPERATIVA,16,17))
    return(TAB_GRAF_B)
  })
  
  # Se genera la grafica del resumen de cantidad de estaciones con valores digitados de las variables por Areas Operativas
  
  output$grafico = renderPlotly({
    fig <- plot_ly(RESUMEN_ORG_1(), x = ~AREA_OPERATIVA, y = ~COMPLETO, type = 'bar', name = 'COMPLETO',text = ~paste0("Porcentaje: ",round((COMPLETO/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig <- fig %>% add_trace(y = ~periodoA, name = '10 meses - 1 año',text = ~paste0("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig <- fig %>% add_trace(y = ~periodoB, name = '8 meses - 10 meses',text = ~paste0("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig <- fig %>% add_trace(y = ~periodoC, name = '6 meses - 8 meses',text = ~paste0("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig <- fig %>% add_trace(y = ~periodoD, name = '4 meses - 6 meses',text = ~paste0("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig <- fig %>% add_trace(y = ~periodoE, name = '< 4 meses',text = ~paste0("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
  })
  
  output$grafico2 = renderPlotly({
    fig2 <- plot_ly(RESUMEN_ORG_2(), x = ~AREA_OPERATIVA, y = ~periodoA, type = 'bar', name = 'Mes Completo',text = ~paste("Porcentaje: ",round((periodoA/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(0, 177, 75)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoB, name = '24 días - Mes Completo',text = ~paste("Porcentaje: ",round((periodoB/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL), marker = list(color = "rgb(105, 36, 211)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoC, name = '15 días - 24 días',text = ~paste("Porcentaje: ",round((periodoC/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(0, 119, 190)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoD, name = '10 días - 15 días',text = ~paste("Porcentaje: ",round((periodoD/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(235, 189, 16)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoE, name = '5 días - 10 días',text = ~paste("Porcentaje: ",round((periodoE/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(115, 94, 0)"))
    fig2 <- fig2 %>% add_trace(y = ~periodoF, name = '< 5 días',text = ~paste("Porcentaje: ",round((periodoF/TOTAL)*100,1),"% \n Total estaciones AO:",TOTAL),marker = list(color = "rgb(197, 11, 9)"))
    fig2 <- fig2 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',legend = list(orientation = 'h',y = 1.13))
    fig2
  })
  
  output$mapAnual = renderLeaflet({
    dataAnualAAW = TABLA_Z_PRE()
    
    # AreasOpPoly1 = AreasOpPoly %>% filter(SEDE == names(AreasOpAlt)[AreasOpAlt == input$AO])
    AreasOpPoly1 = AreasOpPoly[AreasOpPoly$SEDE %in% names(AreasOpAlt)[AreasOpAlt == input$AO],]
    
    
    pal = colorFactor(palette = c("#00B14B", "#6924D3", "#0077BE", "#EBBD10","#756000","#C50B09"),levels = c("01 - COMPLETO","02 - 10 meses - 1 anio","03 - 8 meses - 10 meses","04 - 6 meses - 8 meses","05 - 4 meses - 6 meses","06 - < 4 meses"))
    
    leaflet(dataAnualAAW) %>%
      # addProviderTiles("HERE.normalNightMobile") %>%
      addTiles() %>%
      addPolygons(data = AreasOpPoly1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "#C8C9D7", fillOpacity = 0.8,group = "AreaOperativa",label = ~ SEDE) %>%
      addCircleMarkers(lng=dataAnualAAW$longitud,lat=dataAnualAAW$latitud,group = "estacion",color = ~pal(RANGO),radius = 6,label =paste("rango:", dataAnualAAW$RANGO,"<br>","nombre:", dataAnualAAW$nombre, "<br>","conteo datos:", dataAnualAAW$CONTEO) %>%
                         lapply(htmltools::HTML)
      ) %>% 
      leaflet::addLegend(pal = pal, values = dataAnualAAW$RANGO, position = "bottomleft") %>% 
      addLayersControl(
        overlayGroups = c("AreaOperativa", "estacion"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  output$mapMensual = renderLeaflet({
    y = TABLA_C_PRE()
    AreasOpPoly1 = AreasOpPoly[AreasOpPoly$SEDE %in% names(AreasOpAlt)[AreasOpAlt == input$AO],]
    pal = colorFactor(palette = c("#00B14B", "#6924D3", "#0077BE", "#EBBD10","#756000","#C50B09"),levels = c("mesCompleto","24Dias-mesCompleto","15Dias-24Dias","10Dias-15Dias","5Dias-10Dias","<5Dias"))
    # colorRampPalette(c('red', 'green'))(length(t$RANGO))
    leaflet() %>%
      # addProviderTiles("HERE.normalNightMobile") %>%
      addTiles() %>%
      addPolygons(data = AreasOpPoly1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "#C8C9D7", fillOpacity = 0.8,group = "AreaOperativa",label = ~ SEDE) %>%
      addCircleMarkers(data = y,lng=y$longitud,lat=y$latitud,group = "estacion",color = ~pal(RANGO),radius = 6,label =paste("rango:", y$RANGO, "<br>",
                                                                                                                            "nombre:", y$nombre, "<br>",
                                                                                                                            "conteo datos:", y$CONTEO) %>% lapply(htmltools::HTML)
                       
      ) %>%
      leaflet::addLegend(pal = pal, values = y$RANGO, position = "bottomleft") %>%
      addLayersControl(
        overlayGroups = c("AreaOperativa", "estacion"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  TABLA_Z_PRE = reactive({
    TABLA_Z_PRE = conteoConsolidado %>% filter(VARIABLE == input$variable & anio == input$anio)
    TABLA_Z_PRE = merge(TABLA_Z_PRE,CNE_IDEAM[,c("CODIGO","latitud","longitud","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA")],by.x = "CodigoEstacion",by.y = "CODIGO",all.x = T)
    TABLA_Z_PRE$AREA_OPERATIVA = substr(TABLA_Z_PRE$AREA_OPERATIVA,1,17)
    TABLA_Z_PRE = TABLA_Z_PRE %>% filter(AREA_OPERATIVA == input$AO)
    return(TABLA_Z_PRE)
  })
  
  TABLA_Z = reactive({  
    TABLA_Z = TABLA_Z_PRE()
    TABLA_Z$VARIABLE = NULL
    TABLA_Z$AREA_OPERATIVA = NULL
    TABLA_Z$anio = NULL
    TABLA_Z$latitud = NULL
    TABLA_Z$longitud = NULL
    TABLA_Z$CodigoEstacion = NULL
    TABLA_Z = TABLA_Z[,c("nombre","MUNICIPIO","DEPARTAMENTO","RANGO","CONTEO")]
    return(TABLA_Z)
  })
  
  output$tablaAnual = renderDataTable({datatable(TABLA_Z(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaAnual <- downloadButtonTable(TABLA_Z())
  
  TABLA_Z1 = reactive({
    TABLA_Z1 = TABLA_Z_PRE()
    TABLA_Z1 = merge(TABLA_Z1,CNE_IDEAM[,c("CODIGO","CATEGORIA","ESTADO")],by.x = "CodigoEstacion",by.y = "CODIGO",all.x = T)
    SUMM_TABLA_Z1 = stats::aggregate(CodigoEstacion ~ CATEGORIA + ESTADO, FUN = length, data = TABLA_Z1)
    CNE_IDEAM_FILTER = CNE_IDEAM
    CNE_IDEAM_FILTER$AREA_OPERATIVA = substr(CNE_IDEAM_FILTER$AREA_OPERATIVA,1,17)
    CNE_IDEAM_FILTER = CNE_IDEAM_FILTER %>% filter(AREA_OPERATIVA == input$AO)
    
    CNE_IDEAM_FILTER_B = stats::aggregate(CODIGO ~ CATEGORIA + ESTADO, FUN = length, data = CNE_IDEAM_FILTER)
    TABLA_Z11 = merge(SUMM_TABLA_Z1,CNE_IDEAM_FILTER_B,by = c("CATEGORIA","ESTADO"),all.x = T)
    names(TABLA_Z11) = c("CATEGORIA","ESTADO","Estaciones con datos","Estaciones del CNE")
    return(TABLA_Z11)
  })
  
  output$Text = renderPrint({
    INFO = TABLA_Z1()
    return(INFO)
  })
  
  TABLA_Z12 = reactive({
    TABLA_Z12 = TABLA_Z1()
    TABLA_Z12$`Estaciones del CNE` = NULL
    
    df = data.frame("CATEGORIA" = unique(CNE_IDEAM$CATEGORIA),
                    "Activa" = NA,"Suspendida" = NA,"En Mantenimiento" = NA)
    names(df)[4] = "En Mantenimiento"

    for (i in 1:dim(TABLA_Z12)[1]) {
      df[which(df$CATEGORIA == TABLA_Z12$CATEGORIA[i]),which(names(df) == TABLA_Z12$ESTADO[i])] = TABLA_Z12$`Estaciones con datos`[i]
    }
    
    df = df[rowSums(is.na(df[ , 2:4])) < 3,]
    
    # TABLA_Z12 = reshape2::dcast(TABLA_Z12,CATEGORIA ~ ESTADO,value.var = "Estaciones con datos")
    # colnames(TABLA_Z12) = c("CATEGORIA","Activa","Mantenimiento")
    names(df)[4] = "Mantenimiento"
    df[is.na(df)] <- 0
    return(df)
  })
  
  output$graficoCantidad = renderPlotly({
    fig = plot_ly(TABLA_Z12(),y = ~CATEGORIA, x =  ~Activa,type = 'bar',marker = list(color = "green"),name = "Estaciones Activas")
    fig = fig %>% add_trace(x = ~Mantenimiento,marker = list(color = "blue"),name = "Estaciones en Mantenimiento")
    fig = fig %>% add_trace(x = ~Suspendida,marker = list(color = "red"),name = "Estaciones Suspendidas")
    fig = fig %>% layout(barmode="stack",legend = list(orientation = 'h',y = 1.13), xaxis = list(title = 'Cantidad de Estaciones'),title = "Cantidad de estaciones que reportaron datos")
  })
  
  TABLAZ_FALT_ACT = reactive({
    TABLA_Z2 = TABLA_Z_PRE()
    TABLA_Z2 = merge(TABLA_Z2,CNE_IDEAM[,c("CODIGO","CATEGORIA")],by.x = "CodigoEstacion",by.y = "CODIGO",all.x = T)
    CNE_IDEAM_FILTER = CNE_IDEAM
    CNE_IDEAM_FILTER$AREA_OPERATIVA = substr(CNE_IDEAM_FILTER$AREA_OPERATIVA,1,17)
    CNE_IDEAM_FILTER = CNE_IDEAM_FILTER %>% filter(AREA_OPERATIVA == input$AO & ESTADO == "Activa")
    CNE_IDEAM_FILTER = CNE_IDEAM_FILTER[CNE_IDEAM_FILTER$CATEGORIA %in% unique(TABLA_Z2$CATEGORIA),]
    CNE_IDEAM_FILTER = merge(CNE_IDEAM_FILTER,TABLA_Z2[,c("CodigoEstacion","CONTEO")],by.x = "CODIGO",by.y = "CodigoEstacion",all.x = T)
    CNE_IDEAM_FILTER = CNE_IDEAM_FILTER[is.na(CNE_IDEAM_FILTER$CONTEO),]
    CNE_IDEAM_FILTER = CNE_IDEAM_FILTER[,c("CODIGO","nombre","MUNICIPIO","DEPARTAMENTO","CATEGORIA")]
    return(CNE_IDEAM_FILTER)
  })
  
  output$tablaFaltantesAnual = renderDataTable({datatable(TABLAZ_FALT_ACT(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$DownTablaFaltantesAnual <- downloadButtonTable(TABLAZ_FALT_ACT())
  
  TABLA_R = reactive({
    TABLA_R = RESUMEN_ORG_PORC_MES %>% filter(anio == input$anio & VARIABLE == input$variable & AREA_OPERATIVA == input$AO)
    TABLA_R$VARIABLE = NULL
    TABLA_R$anio = NULL
    TABLA_R$AREA_OPERATIVA = NULL
    colnames(TABLA_R) = c("mes","MesCompleto","24Dias-Mes","15Dias-24Dias","10Dias-15Dias","5Dias-10Dias","<5Dias")
    TABLA_R
  })
  output$RESUMEN_ORG_PORC_MES = renderDataTable({datatable(TABLA_R(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadRESUMEN_ORG_PORC_MES <- downloadButtonTable(TABLA_R())
  
  TABLA_K = reactive({
    TABLA_K = ConteoMes %>% filter(anio == input$anio & VARIABLE == input$variable & AREA_OPERATIVA == input$AO)
    TABLA_K$VARIABLE = NULL
    TABLA_K$anio = NULL
    TABLA_K$AREA_OPERATIVA = NULL
    colnames(TABLA_K) = c("mes","MesCompleto","24Dias-Mes","15Dias-24Dias","10Dias-15Dias","5Dias-10Dias","<5Dias")
    TABLA_K[is.na(TABLA_K)] = 0
    TABLA_K
  })
  output$ConteoMes = renderDataTable({datatable(TABLA_K(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadConteoMes <- downloadButtonTable(TABLA_K())
  
  TABLA_Q = reactive({
    TABLA_Q = RESUMEN_ORG_PORC %>% filter(anio == input$anio & VARIABLE == input$variable)
    TABLA_Q$VARIABLE = NULL
    TABLA_Q$anio = NULL
    TABLA_Q$AREA_OPERATIVA = paste0("AO ",substr(TABLA_Q$AREA_OPERATIVA,16,17))
    colnames(TABLA_Q) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_Q
  })
  output$tablaQ = renderDataTable({datatable(TABLA_Q(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaQ <- downloadButtonTable(TABLA_Q())
  
  TABLA_T = reactive({
    TABLA_T = RESUMEN_ORG %>% filter(anio == input$anio & VARIABLE == input$variable)
    TABLA_T$VARIABLE = NULL
    TABLA_T$anio = NULL
    TABLA_T$AREA_OPERATIVA = paste0("AO ",substr(TABLA_T$AREA_OPERATIVA,16,17))
    colnames(TABLA_T) = c("AO","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses","TOTAL")
    TABLA_T
  })
  output$tablaT = renderDataTable({datatable(TABLA_T(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaT = downloadButtonTable(TABLA_T())
  
  TABLA_S = reactive({
    TABLA_S = RESUMEN_ORG_PORC %>% filter(AREA_OPERATIVA == input$AO & VARIABLE == input$variable)
    TABLA_S$VARIABLE = NULL
    TABLA_S$AREA_OPERATIVA = NULL
    colnames(TABLA_S) = c("Anio","AnioCompleto","10Meses-Anio","8Meses-10Meses","6Meses-8Meses","4Meses-6Meses","<4Meses")
    TABLA_S
  })
  output$tablaS = renderDataTable({datatable(TABLA_S(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTablaS <- downloadButtonTable(TABLA_S())
  
  TABLA_C_PRE = reactive({
    TABLA_C_PRE = TABLA_3 %>% filter(VARIABLE == input$variable & anio == input$anio & mes == input$mes)
    TABLA_C_PRE = merge(TABLA_C_PRE,CNE_IDEAM[,c("CODIGO","latitud","longitud","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA")],by.x = "CodigoEstacion",by.y = "CODIGO",all.x = T)
    TABLA_C_PRE$AREA_OPERATIVA = substr(TABLA_C_PRE$AREA_OPERATIVA,1,17)
    TABLA_C_PRE = TABLA_C_PRE %>% filter(AREA_OPERATIVA == input$AO)
    TABLA_C_PRE = TABLA_C_PRE[,c("CodigoEstacion","nombre","MUNICIPIO","DEPARTAMENTO","AREA_OPERATIVA","latitud","longitud","anio","mes","VARIABLE","RANGO","CONTEO")]
    return(TABLA_C_PRE)
  })
  
  TABLA_C = reactive({
    TABLA_C = TABLA_C_PRE()
    TABLA_C$VARIABLE = NULL
    TABLA_C$AREA_OPERATIVA = NULL
    TABLA_C$anio = NULL
    TABLA_C$mes = NULL
    TABLA_C$latitud = NULL
    TABLA_C$longitud = NULL
    names(TABLA_C)[1] = "COD"
    TABLA_C
  })
  output$tabla3 = renderDataTable({datatable(TABLA_C(),options = list(scrollX = T,pageLength = 25),rownames = F)})
  output$downloadTabla3 <- downloadButtonTable(TABLA_C())
}