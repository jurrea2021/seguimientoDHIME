#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  # headerPanel("Saludes de Shiny"),
  # titlePanel(
  #   h1("Análisis de digitación de valores en DHIME por las Áreas Operativas", align = "center")
  #   ),
  navbarPage(id = "inTabset",
    windowTitle = "Seguimiento DHIME", #title for browser tab
    title = div(tags$a(img(src="FONDO_DHIME_2.png", height=55), href= "http://dhime.ideam.gov.co/webgis/home/"),
                style = "position: relative; top: -16px;"), # Navigation bar
    # theme = bslib::bs_theme(
    #   bg = "#F2F4F8",
    #   fg = "#000000",
    #   primary = "#F02F1F",
    #   base_font = font_google("Prompt"),
    #   code_font = font_google("JetBrains Mono")
    #   ),
    # HTML("Actualizacion:<br/>04/06/2021 19:46"),
    tabPanel(icon = icon("home"),title = strong("Principal"),
             fluidRow(
               column(1),
               column(10,
                      p(strong("Análisis de digitación de valores en DHIME por las Áreas Operativas"), style = "font-size:29px;text-align : center")
                      ),
               column(1,
                      botonAyuda("principal")
                      )
             ),
             br(),
             fluidRow(
               p("La herramienta propuesta facilita el análisis estadístico descriptivo por medio de gráficas, tablas y mapas para algunas de las etiquetas básicas de DHIME más representativas:
                 PTPM_CON, PTPG_CON, TMX_CON, TMN_CON, TSSM_CON, THSM_CON, TSSM_MEDIA_D, BSHG_CON, EVTE_CON, NIVEL_H, Q_MEDIA_D, RCAM_CON, NVLM_CON, CAUDAL_H, PT_AUT_10 y NV_AUT_60.","Los datos fueron descargados de las plataformas de DHIME, con periodos de series de tiempo que van desde el año",strong(" 2012")," hasta el", strong(" 1 de septiembre de 2023"),". 
                 El Catálogo Nacional de estaciones del IDEAM utilizado en la herramienta fue descargado de la página del Instituto el ", strong("1 de septiembre de 2023"),". La herramienta se compone de 7 módulos lo cuáles se explican a continuación:",style="text-align:justify"),
               br()
               # p("La herramienta se compone de 5 módulos lo cuáles se explican a continuación:")
             ),
             fluidRow(
               column(4,
                      div(actionButton('jumpToP1',strong('Seguimiento Áreas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Seguimiento Áreas Operativas: "),"A través de este módulo, se puede conocer el estado de digitación de las estaciones de cada Área Operativa del IDEAM. A partir de unos rangos de digitación propuestos, 
                        en escala anual y mensual se agrupan las estaciones, así cómo gráficas de barras apiladas y mapas, junto con tablas que resumen la cantidad de datos por rangos para cada estación asi,
                        cómo el porcentaje de estaciones y cantidad que se encuentra en cada rango propuesto.",style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP2',strong('Porcentaje digitación Áreas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Porcentaje digitación Áreas Operativas: "),"A través de este módulo, se puede conocer el porcentaje de digitación de valores de las etiquetas de DHIME básicas por Área Operativa.
                        Además, se resumen en tablas las metas mensuales y anuales por Área Operativa asociadas con las estaciones con registros en el periodo de tiempo
                        de estudio y las estaciones que son reportadas en Intranet por algunas Áreas Operativas.",style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP3',strong('Avances Áreas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Avances Áreas Operativas: "),"A través de este módulo, se puede apreciar los avances de digitación de algunas etiquetas de DHIME por parte de las Áreas Operativas.
                         A partir de la descarga realizada el ",strong("18 de agosto de 2023")," y la última descarga del ", strong("1 de septiembre de 2023")," se calculan diferencias que 
                         permiten asociar el estado de avance por cada Área Operativa.",style="text-align:justify")
                      )
             ),
             # br(),
             fluidRow(
               column(4,
                      div(actionButton('jumpToP4',strong('Sensores Convencionales'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Sensores Convencionales: "),"A través de este módulo, se pueden realizar consultas de información de ubicación, cantidad de datos mensual y anual 
                         de una estación en partícular con etiquetas de DHIME consideradas de estaciones convencionales." ,style="text-align:justify")

                      ),
               column(4,
                      div(actionButton('jumpToP5',strong('Sensores Automáticos'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Sensores Automáticos: "),"A través de este módulo, se pueden realizar consultas de información de ubicación, cantidad de datos mensual y anual
                         de una estación en partícular con etiquetas de DHIME consideradas de estaciones automáticas (las etiquetas disponibles en éste módulo son PT_AUT_10 y NV_AUT_60)." ,style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP6',strong('Series Históricas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Series Históricas: "),"A través de este módulo, se puede explorar por departamento y por variable, productos relacionados con las series históricas y ubicaciones de estaciones categorizadas por la longitud de series. Información acrtualizada hasta el mes de JULIO DE 2021." ,style="text-align:justify")
                      )
             ),
             fluidRow(
               column(2),
               column(4,
                      div(actionButton('jumpToP7',strong('Coordenadas Erróneas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Estaciones con Coordenadas Erróneas: "),"A través de este módulo, se identifican las estaciones con coordenadas erróneas, lo cual fueron comparadas con las estaciones del catálogo del año 2014. Se dispone de un mapa para que el usuario tome una decisión para decidir las coordenadas apropiadas para cada estación e informar al grupo de Planeación Operativa" ,style="text-align:justify")
                     ),
               column(4,
                      div(actionButton('jumpToP10',strong('Series Etiquetas DHIME'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Módulo Series Etiquetas DHIME: "),"A través de este módulo, se enlistan las estaciones que tienen registradas etiquetas de DHIME con datos actualizados al ", strong("1 de septiembre de 2023"),"." ,style="text-align:justify")
               ),
               column(2)
             ),
             fluidRow(
               hr(),
               p(em("Desarrollado por:"),em("Julián David Urrea Urrego (Ingeniero Ambiental | Especialista SIG)"),style="text-align:center; font-family: times"),
               p(em("CTO. 334 - 2023 | IDEAM-Planeación Operativa"),style="text-align:center; font-family: times")
               
             )
             ),
    

    
    navbarMenu(strong("Áreas Operativas"),
               AOSeguimientoUI("modulo_AOSeguimiento"),
               AOPorcentajesUI("modulo_AOPorcentajes"),
               AOAvancesUI("modulo_AOAvances")
               ),
    navbarMenu(strong("Sensores Convencionales"),
               AOSensoresConvencionalesUI("modulo_AOSensoresConvencionales"),
               AOSConvencionalesTimeSeriesUI("modulo_AOSensoresConvencionalesTS")
               ),
    navbarMenu(strong("Sensores Automáticos"),
               AOSAUT_PT_AUT_10_UI("modulo_AOSAUT_PT_AUT_10"),
               AOSAUT_NV_AUT_60_UI("modulo_AOSAUT_NV_AUT_60")
      
    ),
    
    seriesHistoricasUI("modulo_seriesHistoricas"),
    
    coordErroneasUI("modulo_coordErroneas"),
    
    TSetiqDHIMEUI("modulo_TSDHIME")

  ),
  
  
  div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
  ###############################################.             
  ##############Footer----    
  ###############################################.
  #Copyright warning
  tags$footer( "© Instituto de Hidrología, Meteorología y Estudios Ambientales (Grupo de Planeación Operativa) v2.0 2023", 
              style = "
              position:fixed;
              text-align:center;
              left: 0;
              bottom:0;
              width:100%;
              z-index:1000;  
              height:30px; /* Height of the footer */
              color: white;
              padding: 5px;
              font-weight: bold;
              background-color: #2C3E50"
  )
))
