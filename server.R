#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


source("./global.R")




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  moduleServer("modulo_AOSeguimiento",AOSeguimiento)
  
  moduleServer("modulo_AOPorcentajes",AOPorcentajes)
  
  moduleServer("modulo_AOAvances",AOAvances)
  
  moduleServer("modulo_AOSensoresConvencionales",AOSensoresConvencionales)

  moduleServer("modulo_AOSensoresConvencionalesTS",AOSConvencionalesTimeSeries)
  
  moduleServer("modulo_AOSAUT_PT_AUT_10",AOSAUT_PT_AUT_10)
  
  moduleServer("modulo_AOSAUT_NV_AUT_60",AOSAUT_NV_AUT_60)
  
  moduleServer("modulo_seriesHistoricas",seriesHistoricas)
  
  moduleServer("modulo_coordErroneas",coordErroneas)
  
  moduleServer("modulo_TSDHIME",TSetiqDHIME)
  
  
#############################################################################################################################  
  

  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel4")
  })
  observeEvent(input$jumpToP5, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel5")
  })
  observeEvent(input$jumpToP6, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel6")
  })
  observeEvent(input$jumpToP7, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel7")
  })
  observeEvent(input$jumpToP10, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel10")
  })
  observeEvent(input$principal, {
    # Show a modal when the button is pressed
    shinyalert("Importante!", "Para acceder a cada uno de los módulos descritos, está la opción de dar clic en el botón que se encuentra encima de la descripción de cada módulo o también en las pestañas de la barra superior de la herramienta.",type = "info")
  })
}