Sys.setlocale(locale = "Spanish")

seriesHistoricasUI = function(id) {
  ns <- NS(id)
  
  tabPanel(strong("Series Históricas"),value = "panel6",
           dashboardPage(
             dashboardHeader(disable = T),
             dashboardSidebar(
               botonAyuda(ns("ayuda23")),
               selectInput(inputId = ns("seleccionarDpto"),
                           label = "Seleccione el departamento de interés",
                           choices = NULL),
               filtroEtiquetasDHIME(ns('etiq'),'ETIQUETA DHIME'),
               sliderInput(ns("porcComplet"),"Porcentaje de completitud series",
                           min = 0,
                           max = 100,
                           value = c(0,100),sep = "")
             ),
             dashboardBody(
               
               tabsetPanel(
                 tabPanel(strong("Dashboard"),
                          fluidRow(
                            box("MAPA",leafletOutput(ns("mapPrueba"))),
                            box(plotlyOutput(ns("BoxPlotSeries")))
                          ),
                          fluidRow(column(7,
                                          div(tags$a(img(src="leyendaEstaciones.png", height=30,width= 550)),style = "position: relative; top: -4px",align ="center")
                          )
                          ),
                          fluidRow(
                            box(imageOutput(ns("myImage"),height = 1600),width = 7,collapsible = TRUE),
                            box(verbatimTextOutput(ns("TableText")),width = 5)
                          )
                          
                 ),
                 tabPanel(strong("Tabla"),
                          downloadButton(ns("downloadTabla15")),
                          box(div(dataTableOutput(ns("tablaAnualX")),style = "font-size:70%"),width = 12)
                          
                 )
               )
               
               
             )
           )
           
  )
  
}

seriesHistoricas = function(input, output, session) {
  
  dptos = c("Choco" = "Chocó","Antioquia" = "Antioquia","Cordoba" = "Córdoba","Atlantico" = "Atlantico","Sucre" = "Sucre","Bolivar" = "Bolívar","Magdalena" = "Magdalena",
            "LaGuajira" = "La Guajira","Cesar" = "Cesar","NorteDeSantander" = "Norte de Santander","SanAndres" = "Archipiélago de San Andres, Providencia y Santa Catalina",
            "Huila" = "Huila","Cauca" = "Cauca","Tolima" = "Tolima","Cundinamarca" = "Cundinamarca","Bogota" = "Bogotá","Caldas" = "Caldas","Boyaca" = "Boyacá","Santander" = "Santander",
            "ValledelCauca" = "Valle del Cauca","Quindio" = "Quindío","RIsaralda" = "RIsaralda","Guaviare" = "Guaviare","Guainia" = "Guainía","Meta" = "Meta","Vichada" = "Vichada","Casanare" = "Casanare",
            "Arauca" = "Arauca","Vaupes" = "Vaupes","Putumayo" = "Putumayo","Caqueta" = "Caquetá","Amazonas" = "Amazonas","Narino" = "Nariño")
  
  
  observeEvent(input$ayuda23, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","Con los filtros de departamento y de etiqueta DHIME, se filtra de la pestaña de ´Dashboard´, el mapa de ubicación de las estaciones asi como la gráfica de distribución de las estaciones agrupadas por subregiones del departamento y/o por municipios del departamento. Temporalmente la gráfica de series de las estaciones dle conjunto se encuentra estática. Finalmente, debajo de los filtros se encuentra una barra para filtrar las estaciones por porcentaje de completitud de sus series.",type = "info")
  })
  
  observe({
    updateSelectInput(session, inputId = "seleccionarDpto",label = "Seleccione el departamento de interés", 
                      choices = unique(c(analisisSeries$DEPARTA)))
  })
  
  output$TableText = renderPrint({
    seriesHist2 = seriesHistA()
    table(seriesHist2$ESTADO)
  })
  
  seriesHistA = reactive({
    seriesHist = analisisSeries %>% filter(DEPARTA == input$seleccionarDpto & etiquet == input$etiq)
    seriesHist$lngSerie = NA
    seriesHist$lngSerie[which(seriesHist$lngtdSr < 10)] = "< 10 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 10 & seriesHist$lngtdSr < 20)] = "10 - 20 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 20 & seriesHist$lngtdSr < 30)] = "20 - 30 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 30 & seriesHist$lngtdSr < 40)] = "30 - 40 anios"
    seriesHist$lngSerie[which(seriesHist$lngtdSr >= 40)] = "> 40 anios"
    seriesHist$AREA_OP = substr(seriesHist$AREA_OP,1,17)
    seriesHist = subset(seriesHist,seriesHist$porcTtl >= input$porcComplet[1] & seriesHist$porcTtl <= input$porcComplet[2])
    
    seriesHist = merge(seriesHist,CNE_CRUCE_DIVIPOLA,by.x = "MUNICIP",by.y = "MUNICIPIO",all.x = T)
    seriesHist = merge(seriesHist,MunicipioSubregion[,c("MPIO_CCNCT","COD_SUBREG","NOM_SUBREG")],by.x = "COD_CNMO",by.y = "MPIO_CCNCT",all.x = T)
    return(seriesHist)
  })
  
  seriesHistB = reactive({
    seriesHistBA = seriesHistA()
    seriesHistBA = seriesHistBA[,c("codigo","nombre","MUNICIP","AREA_OP","NOM_SUBREG","ESTADO","periodo","lngtdSr","porcTtl","cntddDt","FECHA_INSTALACION","FECHA_SUSPENSION")]
    colnames(seriesHistBA) = c("codigo","nombre","municipio","AREA_OPERATIVA","SUBREGION","ESTADO","periodo","longitudSerie","porcCompletitud","cantidadDatos","FECHA_INSTALACION","FECHA_SUSPENSION")
    return(seriesHistBA)
  })
  
  output$tablaAnualX = ({
    renderDataTable({datatable(seriesHistB(),options = list(scrollX = T,pageLength = 10),rownames = F)})
  })
  output$downloadTabla15 <- downloadButtonTable(seriesHistB())
  
  
  output$mapPrueba = renderLeaflet({
    
    seriesHist1 = seriesHistA() 
    SubRegs1 = SubRegs[SubRegs$COD_SUBREG %in% stringr::str_pad(unique(seriesHist1$COD_SUBREG),4,pad = "0"),]
    
    pal = colorFactor(palette = c("#73FFDF","#0070FF","#FFAA00","#4CE600","#FFFF00"),levels = c("< 10 anios","10 - 20 anios","20 - 30 anios","30 - 40 anios","> 40 anios"))
    
    m = leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>% addProviderTiles(providers$Stamen.TonerLines) %>% addProviderTiles(providers$Stamen.TonerLabels) %>% 
      addPolygons(data = SubRegs1, weight = 1, smoothFactor = 0.5,opacity = 1.0,color = "#6CAB96", fillOpacity = 0.8,group = "Subregiones",label = ~ NOM_SUBREG) %>%
      addCircleMarkers(data = seriesHist1,lng= ~ longitud,lat= ~ latitud,color = ~pal(lngSerie),group = "Estaciones", opacity = 0.8, radius = 5, fillOpacity = 0.8,label =paste("codigo:", seriesHist1$codigo, "<br>",
                                                                                                                                                                                "nombre:", seriesHist1$nombre, "<br>",
                                                                                                                                                                                "municipio:", seriesHist1$MUNICIP, "<br>",
                                                                                                                                                                                "estado", seriesHist1$ESTADO, "<br>",
                                                                                                                                                                                "periodo:", seriesHist1$periodo, "<br>",
                                                                                                                                                                                "longitud_serie:", seriesHist1$lngtdSr, " anios <br>",
                                                                                                                                                                                "cantidad_datos:", seriesHist1$cntddDt, "<br>",
                                                                                                                                                                                "porcentaje_completitud:", seriesHist1$porcTtl, "% <br>") %>% lapply(htmltools::HTML)) %>%
      leaflet::addLegend(pal = pal, values = seriesHist1$lngSerie, position = "topleft") %>%
      addLayersControl(
        overlayGroups = c("Subregiones", "Estaciones"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  output$BoxPlotSeries = renderPlotly({
    seriesHist2 = seriesHistA()
    
    if (seriesHist2$DEPARTA == "Amazonas" | seriesHist2$DEPARTA == "Arauca" | seriesHist2$DEPARTA == "Bogotá" |
        seriesHist2$DEPARTA == "Caquetá" |  seriesHist2$DEPARTA == "Casanare" | seriesHist2$DEPARTA == "Guainía" |
        seriesHist2$DEPARTA == "Guaviare" | seriesHist2$DEPARTA == "Meta" | seriesHist2$DEPARTA == "Putumayo" |
        seriesHist2$DEPARTA == "Vaupes" | seriesHist2$DEPARTA == "Vichada") {
      
      fig <- plot_ly(y = seriesHist2$lngtdSr,color = seriesHist2$MUNICIP, type = "box", boxpoints = "all", jitter = 0.6, pointpos = -2,
                     text = ~paste0("Nombre: ",seriesHist2$nombre,"\n Estado: ",seriesHist2$ESTADO,
                                    "\n Longitud serie : ",seriesHist2$lngtdSr," Años\nPorcentaje Completitud: ",seriesHist2$porcTtl,"%"),
                     width = 510, height = 430) %>%
        layout(#title = list(text = 'Comportamiento de longitudes de serie \nde estaciones activas\ndepartamento de Valle del Cauca',y = 1.6), 
          yaxis = list(title = '</b> Longitud de series (Años) </b>'), xaxis = list(title = '</b> Municipio </b>'),showlegend = FALSE)
      fig
    } else {
      fig <- plot_ly(y = seriesHist2$lngtdSr,color = seriesHist2$NOM_SUBREG, type = "box", boxpoints = "all", jitter = 0.6, pointpos = -2,
                     text = ~paste0("Nombre: ",seriesHist2$nombre,"\n Municipio: ",seriesHist2$MUNICIP,"\n Estado: ",seriesHist2$ESTADO,
                                    "\n Longitud serie : ",seriesHist2$lngtdSr," Años\nPorcentaje Completitud: ",seriesHist2$porcTtl,"%"),
                     width =550, height = 400) %>%
        layout(#title = list(text = 'Comportamiento de longitudes de serie \nde estaciones activas\ndepartamento de Valle del Cauca',y = 1.6), 
          yaxis = list(title = '</b> Longitud de series (Años) </b>'), xaxis = list(title = '</b> Subregión </b>'),showlegend = FALSE)
      fig
    }
  })
  output$myImage = renderImage({
    filename <- normalizePath(file.path('www/series',paste0(input$etiq,'-',names(dptos)[dptos == input$seleccionarDpto],'.png')))
    seriesB = seriesHistB()
    
    if (dim(seriesB)[1] <= 10) {sizeY = 500}
    else if (dim(seriesB)[1] > 10 & dim(seriesB)[1] <= 50) {sizeY = 800} 
    else if (dim(seriesB)[1] > 50 & dim(seriesB)[1] <= 100) {sizeY = 900} 
    else if (dim(seriesB)[1] > 100 & dim(seriesB)[1] <= 150) {sizeY = 1100} 
    else if (dim(seriesB)[1] > 150) {sizeY = 1200}
    
    list(src = filename,
         contentType = "www/series/png",
         width = 590,
         height = sizeY,
         alt = paste0(input$etiq,'-',input$seleccionarDpto,'.png'))
    
  }, deleteFile = FALSE)
  
}