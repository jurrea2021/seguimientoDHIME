Sys.setlocale(locale = "Spanish")

TSetiqDHIMEUI = function(id) {
  ns <- NS(id)
  
  tabPanel(strong(" Etiquetas DHIME "),value = "panel10",
    fluidRow(
      column(3,
             botonAyuda(ns("ayuda35"))
      ),
      column(3,
             filtroParamsEtqDHIME(ns('parametro'), 'PARAMETRO DHIME')
             ),
      column(3,
             selectInput(inputId = ns("selEtiquetas"),
                         label = "Seleccione etiqueta",
                         choices = NULL)
             ),
      column(1),
      column(2,
             downloadButton(ns("downloadTabla27"))
      )
    ),
    fluidRow(
      box(div(dataTableOutput(ns("tablaEtiqDHIMEest")),style = "font-size:70%"),width = 12)
    )
  )
}


TSetiqDHIME = function(input, output, session) {
  
  estEtq = reactive({
    ETQ = tsDHIME %>% filter(IDPARAMETRO == input$parametro)
    return(ETQ)
  })
  
  observe({
    updateSelectInput(session, inputId = "selEtiquetas",label = "Seleccione etiqueta", 
                      choices = c(unique(estEtq()$ETIQUETA)))
  })
  
  estEtq2 = reactive({
    ETQ2 = tsDHIME %>% filter(ETIQUETA == input$selEtiquetas)
    CNEtemp = CNE[CNE$CODIGO %in% ETQ2$IDESTACION,]
    CNEtemp = CNEtemp[CNEtemp$FUENTE == "CNE_IDEAM",]
    CNEtemp = merge(CNEtemp[,c("CODIGO","nombre","MUNICIPIO","DEPARTAMENTO",
                               "AREA_OPERATIVA","CATEGORIA","TECNOLOGIA",
                               "ESTADO")],ETQ2[,c("IDESTACION","ETIQUETA",
                                                  "INICIODATA","FINDATA")],
                    by.x = "CODIGO",by.y = "IDESTACION",all.x = T)
    return(CNEtemp)
  })
  
  output$downloadTabla27 <- downloadButtonTable(estEtq2())
  
  output$tablaEtiqDHIMEest = ({
    renderDataTable({datatable(estEtq2(),options = list(scrollX = T,pageLength = 10),rownames = F)})
  })
  
  observeEvent(input$ayuda35, {
    # Show a modal when the button is pressed
    shinyalert("Importante!","El filtro de 'Etiqueta' se actualiza de acuerdo al filtro 'PARAMETRO DHIME'.",type = "info")
  })
  
}