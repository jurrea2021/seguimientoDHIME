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
                      p(strong("Analisis de digitacion de valores en DHIME por las Areas Operativas"), style = "font-size:29px;text-align : center")
                      ),
               column(1,
                      botonAyuda("principal")
                      )
             ),
             br(),
             fluidRow(
               p("La herramienta propuesta facilita el analisis estadistico descriptivo por medio de grAficas, tablas y mapas para algunas de las etiquetas basicas de DHIME mAs representativas:
                 PTPM_CON, PTPG_CON, TMX_CON, TMN_CON, TSSM_CON, THSM_CON, TSSM_MEDIA_D, BSHG_CON, EVTE_CON, NIVEL_H, Q_MEDIA_D, RCAM_CON, NVLM_CON, CAUDAL_H, PT_AUT_10 y NV_AUT_60.","Los datos fueron descargados de las plataformas de DHIME, con periodos de series de tiempo que van desde el anio",strong(" 2014")," hasta el", strong(" 9 de mayo de 2025"),". 
                 El Catalogo Nacional de estaciones del IDEAM utilizado en la herramienta fue descargado de la pagina del Instituto el ", strong("19 de febrero de 2025"),". La herramienta se compone de 7 modulos lo cuales se explican a continuacion:",style="text-align:justify"),
               br()
               # p("La herramienta se compone de 5 módulos lo cuáles se explican a continuación:")
             ),
             fluidRow(
               column(4,
                      div(actionButton('jumpToP1',strong('Seguimiento Areas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Seguimiento Areas Operativas: "),"A traves de este modulo, se puede conocer el estado de digitacion de las estaciones de cada Area Operativa del IDEAM. A partir de unos rangos de digitacion propuestos, 
                        en escala anual y mensual se agrupan las estaciones, asi como graficas de barras apiladas y mapas, junto con tablas que resumen la cantidad de datos por rangos para cada estacion asi,
                        como el porcentaje de estaciones y cantidad que se encuentra en cada rango propuesto.",style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP2',strong('Porcentaje digitacion Areas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Porcentaje digitacion Areas Operativas: "),"A traves de este modulo, se puede conocer el porcentaje de digitacion de valores de las etiquetas de DHIME basicas por Area Operativa.
                        Ademas, se resumen en tablas las metas mensuales y anuales por Area Operativa asociadas con las estaciones con registros en el periodo de tiempo
                        de estudio y las estaciones que son reportadas en Intranet por algunas Áreas Operativas.",style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP3',strong('Avances Áreas Operativas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Avances Areas Operativas: "),"A traves de este modulo, se puede apreciar los avances de digitacion de algunas etiquetas de DHIME por parte de las Areas Operativas.
                         A partir de la descarga realizada el ",strong("6 de mayo de 2025")," y la ultima descarga del ", strong("9 de mayo de 2025")," se calculan diferencias que 
                         permiten asociar el estado de avance por cada Area Operativa.",style="text-align:justify")
                      )
             ),
             # br(),
             fluidRow(
               column(4,
                      div(actionButton('jumpToP4',strong('Sensores Convencionales'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Sensores Convencionales: "),"A traves de este modulo, se pueden realizar consultas de informacion de ubicacion, cantidad de datos mensual y anual 
                         de una estacion en partícular con etiquetas de DHIME consideradas de estaciones convencionales." ,style="text-align:justify")

                      ),
               column(4,
                      div(actionButton('jumpToP5',strong('Sensores Automaticos'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Sensores Automaticos: "),"A traves de este modulo, se pueden realizar consultas de informacion de ubicacion, cantidad de datos mensual y anual
                         de una estacion en partícular con etiquetas de DHIME consideradas de estaciones automaticas (las etiquetas disponibles en este modulo son PT_AUT_10 y NV_AUT_60)." ,style="text-align:justify")
                      ),
               column(4,
                      div(actionButton('jumpToP6',strong('Series Historicas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Series Historicas: "),"A traves de este modulo, se puede explorar por departamento y por variable, productos relacionados con las series historicas y ubicaciones de estaciones categorizadas por la longitud de series. Informacion actualizada hasta el mes de JULIO DE 2021." ,style="text-align:justify")
                      )
             ),
             fluidRow(
               column(2),
               column(4,
                      div(actionButton('jumpToP7',strong('Coordenadas Erróneas'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Estaciones con Coordenadas Erroneas: "),"A través de este módulo, se identifican las estaciones con coordenadas erroneas, lo cual fueron comparadas con las estaciones del catalogo del anio 2014. Se dispone de un mapa para que el usuario tome una decision para decidir las coordenadas apropiadas para cada estacion e informar al grupo de Planeacion Operativa" ,style="text-align:justify")
                     ),
               column(4,
                      div(actionButton('jumpToP10',strong('Series Etiquetas DHIME'),style="color: white; background-color: #2C3E50; border-color: #18BC9C"),align ="center"),
                      br(),
                      p(strong("Modulo Series Etiquetas DHIME: "),"A traves de este modulo, se enlistan las estaciones que tienen registradas etiquetas de DHIME con datos actualizados al ", strong("3 de febrero de 2025"),"." ,style="text-align:justify")
               ),
               column(2)
             ),
             fluidRow(
               hr(),
               p(em("Desarrollado por:"),em("Julian David Urrea Urrego (Ingeniero Ambiental | Especialista SIG)"),style="text-align:center; font-family: times"),
               p(em("CTO. 044 - 2025 | IDEAM-Planeacion Operativa"),style="text-align:center; font-family: times")
               
             )
             ),
    

    
    navbarMenu(strong("Areas Operativas"),
               AOSeguimientoUI("modulo_AOSeguimiento"),
               AOPorcentajesUI("modulo_AOPorcentajes"),
               AOAvancesUI("modulo_AOAvances")
               ),
    navbarMenu(strong("Sensores Convencionales"),
               AOSensoresConvencionalesUI("modulo_AOSensoresConvencionales"),
               AOSConvencionalesTimeSeriesUI("modulo_AOSensoresConvencionalesTS")
               ),
    navbarMenu(strong("Sensores Automaticos"),
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
  tags$footer( "© Instituto de Hidrologia, Meteorologia y Estudios Ambientales (Grupo de Planeacion Operativa) v3.0 2025", 
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
