Sys.setlocale(locale = "Spanish")

AOSConvencionalesTimeSeriesUI = function(id) {
  ns <- NS(id)
  tabPanel(strong(" Sensores convencionales PRUEBA "),
           dashboardPage(
             dashboardHeader(disable = T),
             dashboardSidebar(
               filtroV1AreasOperativas(ns("AOSCoompleto"),"ÁREA OPERATIVA"),
               selectInput(inputId = ns("seleccionarEstacion"),
                           label = shiny::span(strong("Seleccione código de la estación de interés"), style = "color:red"),#div("Seleccione código de la estación de interés",style = "color: #1C2322"),
                           choices = NULL),
               selectInput(inputId = ns("etiqSeriesTime"),
                           label = "ETIQUETA DHIME",
                           choices = NULL),
               # filtroEtiquetasDHIME(ns('etiqSeriesTime'),'ETIQUETA DHIME'),
               sliderInput(ns("year"),"Data time period",
                           min = 2012,
                           max = 2023,
                           value = c(2012,2023),sep = "")#,
               # verbatimTextOutput("clientdataText")
               
               
             ),
             dashboardBody(
               
               tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style_dashboard.css")),
               fluidRow(box(dygraphOutput(ns("dygraphDashboard"))),
                        box(plotlyOutput(ns("BoxPlotMeses")))),
               fluidRow(box(width=4,verbatimTextOutput(ns("clientdataText"))),
                        box(width=8,verbatimTextOutput(ns("summarymonthText")))),
               # fluidRow(
               #   column(11,
               #          fluidRow(),
               #          fluidRow()
               #          ),
               #   column(11,
               #          fluidRow(),
               #          fluidRow()
               #          )),
               fluidRow(
                 botonAyuda(ns("ayuda20"))
               )
             )
           )
           
  )
}



AOSConvencionalesTimeSeries = function(input, output, session) {
  observeEvent(input$ayuda20, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "PRUEBA !!!",type = "info")
  })
  
  listadoEstaciones = reactive({
    est_AO = CNE_IDEAMA %>% filter(AREA_OPERATIVA == input$AOSCoompleto)
    listado = unique(conteoEtiquetas$codigo)
    listadoFiltrado = data.frame("estacion" = listado[which(listado %in% est_AO$CODIGO)],stringsAsFactors = F)
    return(listadoFiltrado)
  })
  
  observe({
    updateSelectInput(session, inputId = "seleccionarEstacion",label = "Seleccione código de la estación de interés", 
                      choices = c(listadoEstaciones()$estacion))
  })
  
  observe({
    updateSelectInput(session, inputId = "etiqSeriesTime",label = "ETIQUETA DHIME", 
                      choices = c(conteoEtiquetas$etiqueta[conteoEtiquetas$codigo == input$seleccionarEstacion]))
  })
  
  querySQLite = reactive({
    BDETemp = BDEstaciones[BDEstaciones$etiqueta == input$etiqSeriesTime & BDEstaciones$codigo == input$seleccionarEstacion,]
    conn <- dbConnect(RSQLite::SQLite(), paste0(RUTA,"sqlite/",BDETemp$sqliteDB))
    query = dbGetQuery(conn,paste0("SELECT * FROM valores WHERE Codigo = ",input$seleccionarEstacion,""))
    query$year = as.numeric(substr(query$Fecha,1,4))
    query = subset(query,query$year >= input$year[1] & query$year <= input$year[2])
    dbDisconnect(conn)
    return(query)
  })
  
  
  querySQLiteMes = reactive({
    queryTempMes = querySQLite()
    queryTempMes$mes = substr(queryTempMes$Fecha,6,7)
    queryTempMes$mesA = NA
    queryTempMes$mesA[queryTempMes$mes == "01"] = "01_ENE" ; queryTempMes$mesA[queryTempMes$mes == "02"] = "02_FEB"
    queryTempMes$mesA[queryTempMes$mes == "03"] = "03_MAR" ; queryTempMes$mesA[queryTempMes$mes == "04"] = "04_ABR"
    queryTempMes$mesA[queryTempMes$mes == "05"] = "05_MAY" ; queryTempMes$mesA[queryTempMes$mes == "06"] = "06_JUN"
    queryTempMes$mesA[queryTempMes$mes == "07"] = "07_JUL" ; queryTempMes$mesA[queryTempMes$mes == "08"] = "08_AGO"
    queryTempMes$mesA[queryTempMes$mes == "09"] = "09_SEP" ; queryTempMes$mesA[queryTempMes$mes == "10"] = "10_OCT"
    queryTempMes$mesA[queryTempMes$mes == "11"] = "11_NOV" ; queryTempMes$mesA[queryTempMes$mes == "12"] = "12_DIC"
    return(queryTempMes)
  })
  
  querySQLiteOrd = reactive({
    queryTemp = querySQLite()
    queryTemp = queryTemp[,c("Fecha","Valor")]
    
    
    if (input$etiqSeriesTime == "PTPM_CON" | input$etiqSeriesTime == "TMX_CON" | input$etiqSeriesTime == "TMN_CON" |
        input$etiqSeriesTime == "TSSM_MEDIA_D" | input$etiqSeriesTime == "Q_MEDIA_D" | input$etiqSeriesTime == "RCAM_CON") {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d")
      SECUENCIA = data.frame("Fecha"=seq(min(queryTemp$Fecha,na.rm = T),max(queryTemp$Fecha,na.rm = T), by = '1 day'),stringsAsFactors = F)
    } else {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d %H:%M:%S")
      SECUENCIA = data.frame("Fecha"=seq(min(queryTemp$Fecha,na.rm = T),max(queryTemp$Fecha,na.rm = T), by = '1 hour'),stringsAsFactors = F) #
    }
    
    SECUENCIA$Fecha = as.character(SECUENCIA$Fecha)
    queryTemp$Fecha = as.character(queryTemp$Fecha)
    queryTemp = merge(queryTemp,SECUENCIA,by = "Fecha",all = T)
    
    if (input$etiqSeriesTime == "TSSM_CON" | input$etiqSeriesTime == "THSM_CON") {
      queryTemp$dia = as.numeric(substr(queryTemp$Fecha,12,13)) #
      queryTemp = queryTemp[queryTemp$dia %in% c(7,13,18),] #
      queryTemp$dia = NULL
    } else if (input$etiqSeriesTime == "BSHG_CON") {
      queryTemp$dia = as.numeric(substr(queryTemp$Fecha,12,13)) #
      queryTemp = queryTemp[queryTemp$dia %in% c(5:18),] #
      queryTemp$dia = NULL
    } else if (input$etiqSeriesTime == "NVLM_CON") {
      queryTemp$dia = as.numeric(substr(queryTemp$Fecha,12,13)) #
      queryTemp = queryTemp[queryTemp$dia %in% c(6,18),] #
      queryTemp$dia = NULL
    }
    
    if (input$etiqSeriesTime == "PTPM_CON" | input$etiqSeriesTime == "TMX_CON" | input$etiqSeriesTime == "TMN_CON" |
        input$etiqSeriesTime == "TSSM_MEDIA_D" | input$etiqSeriesTime == "Q_MEDIA_D" | input$etiqSeriesTime == "RCAM_CON") {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d")
    } else {
      queryTemp$Fecha = strptime(queryTemp$Fecha,"%Y-%m-%d %H:%M:%S")
    }
    
    xts_uno = xts::xts(queryTemp$Valor,order.by = queryTemp$Fecha,frequency = 365,tz = "GMT")
    return(xts_uno)
  })
  
  output$summarymonthText = renderPrint({
    aa = tidyr::spread(data = querySQLiteMes(),key = mesA,value = Valor)
    print(summary(aa[c("01_ENE","02_FEB","03_MAR","04_ABR","05_MAY","06_JUN","07_JUL","08_AGO","09_SEP","10_OCT","11_NOV","12_DIC")]))
  })
  
  output$dygraphDashboard = renderDygraph({
    dygraph(data = querySQLiteOrd(), main = paste0("Serie de tiempo ",input$etiqSeriesTime), xlab = "Fecha", ylab = "Valor") %>% 
      dyRangeSelector()
  })
  
  output$BoxPlotMeses = renderPlotly({
    fig <- plot_ly(y = querySQLiteMes()$Valor,color = querySQLiteMes()$mesA, type = "box") %>%
      layout(#title = list(text = 'Comportamiento de longitudes de serie \nde estaciones activas\ndepartamento de Valle del Cauca',y = 1.6), 
        yaxis = list(title = '</b> Valor </b>'), xaxis = list(title = '</b> Mes </b>'),showlegend = FALSE)
    fig
  })
  
  output$titleDashboard = renderText({
    print(paste(CNE$nombre[CNE$CODIGO == input$seleccionarEstacion],"|",CNE$MUNICIPIO[CNE$CODIGO == input$seleccionarEstacion],"|",CNE$DEPARTAMENTO[CNE$CODIGO == input$seleccionarEstacion],"|",CNE$AREA_OPERATIVA[CNE$CODIGO == input$seleccionarEstacion]))
  })
  
  output$clientdataText = renderPrint({
    queryTemp2 = querySQLite()
    queryTemp2 = queryTemp2[,c("year","Valor")]
    print(summary(queryTemp2))
  })
}