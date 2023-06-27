filtroEtiquetasDHIME = function(id,labelId) {
  selectInput(id,
              label = labelId,
              choices = c("PTPM_CON" = "PTPM_CON",
						  "PTPG_CON" = "PTPG_CON",
                          "TMX_CON" = "TMX_CON",
                          "TMN_CON" = "TMN_CON",
                          "NVLM_CON" = "NVLM_CON",
                          "CAUDAL_H" = "CAUDAL_H",
                          "TSSM_MEDIA_D" = "TSSM_MEDIA_D",
                          "TSSM_CON" = "TSSM_CON",
                          "THSM_CON" = "THSM_CON",
                          "BSHG_CON" = "BSHG_CON",
                          "EVTE_CON" = "EVTE_CON",
                          "RCAM_CON" = "RCAM_CON",
                          "Q_MEDIA_D" = "Q_MEDIA_D",
                          "NIVEL_H" = "NIVEL_H"
              ))
}

filtroAnios = function(id,labelId) {
  selectInput(id,
              label = labelId,
              choices = c("2012" = 2012,"2013" = 2013,
                          "2014" = 2014,"2015" = 2015,
                          "2016" = 2016,"2017" = 2017,
                          "2018" = 2018,"2019" = 2019,
                          "2020" = 2020,"2021" = 2021,"2022" = 2022,"2023" = 2023))
}

filtroV1AreasOperativas = function(id,labelId) {
  selectInput(id,
              label = labelId,
              choices = c("Area Operativa 01" = "Area Operativa 01",
                          "Area Operativa 02" = "Area Operativa 02",
                          "Area Operativa 03" = "Area Operativa 03",
                          "Area Operativa 04" = "Area Operativa 04",
                          "Area Operativa 05" = "Area Operativa 05",
                          "Area Operativa 06" = "Area Operativa 06",
                          "Area Operativa 07" = "Area Operativa 07",
                          "Area Operativa 08" = "Area Operativa 08",
                          "Area Operativa 09" = "Area Operativa 09",
                          "Area Operativa 10" = "Area Operativa 10",
                          "Area Operativa 11" = "Area Operativa 11"))
}


filtroV2AreasOperativas = function(id,labelId) {
  selectInput(id,
              label = labelId,
              choices = c("Area Operativa 01" = "Area Operativa 01 - Antioquia-Chocó",
                          "Area Operativa 02" = "Area Operativa 02 - Atlántico-Bolivar-Sucre",
                          "Area Operativa 03" = "Area Operativa 03 - Meta-Guaviare-Guainía",
                          "Area Operativa 04" = "Area Operativa 04 - Huila-Caquetá" ,
                          "Area Operativa 05" = "Area Operativa 05 - Magdalena-Cesar-Guajira",
                          "Area Operativa 06" = "Area Operativa 06 - Boyacá-Casanare-Vichada",
                          "Area Operativa 07" = "Area Operativa 07 - Nariño-Putumayo",
                          "Area Operativa 08" = "Area Operativa 08 - Santanderes-Arauca",
                          "Area Operativa 09" = "Area Operativa 09 - Cauca-Valle-Caldas",
                          "Area Operativa 10" = "Area Operativa 10 - Tolima",
                          "Area Operativa 11" = "Area Operativa 11 - Cundinamarca-Amazonas-San Andrés"))
}

filtroParamsEtqDHIME = function(id,labelId) {
  selectInput(id,
              label = labelId,
              choices = c("HUM RELATIVA" = "HUM RELATIVA","DURACION" = "DURACION","PRECIPITACION" = "PRECIPITACION",
                          "TEMPERATURA" = "TEMPERATURA","PUNTO ROCIO" = "PUNTO ROCIO","TENSION VAPOR" = "TENSION VAPOR",        
                          "DIR VIENTO" = "DIR VIENTO","EVAPORACION" = "EVAPORACION","FEN ATMOS" = "FEN ATMOS",
                          "PRES ATMOS" = "PRES ATMOS","REC VIENTO" = "REC VIENTO","VEL VIENTO" = "VEL VIENTO",           
                          "BRILLO SOLAR" = "BRILLO SOLAR","CS" = "CS","PORCENTAJE" = "PORCENTAJE",
                          "CM" = "CM","NIVEL" = "NIVEL","CAUDAL" = "CAUDAL","TM" = "TM","PESO MUESTRA" = "PESO MUESTRA",
                          "HUM SUELO" = "HUM SUELO","TEMP SUELO" = "TEMP SUELO","NUBOSIDAD" = "NUBOSIDAD",
                          "ADIMENSIONAL" = "ADIMENSIONAL","RAD SOLAR" = "RAD SOLAR","ALTURA" = "ALTURA",
                          "ANG ELEVACION" = "ANG ELEVACION","DIR NUBOSIDAD" = "DIR NUBOSIDAD","TIPO NUBOSIDAD" = "TIPO NUBOSIDAD",
                          "VOL MUESTRA" = "VOL MUESTRA","VISIBILIDAD" = "VISIBILIDAD","RAD UV" = "RAD UV",
                          "BRILLO SOLAR RELATIVO" = "BRILLO SOLAR RELATIVO","ESTADO SUELO" = "ESTADO SUELO",
                          "DEUTERIO" = "DEUTERIO","OXIGENO 18" = "OXIGENO 18","TRITIO" = "TRITIO")
  )
}


botonAyuda = function(id) {
  div(useShinyalert(), # Set up shinyalert
      actionButton(id, strong("Ayuda"),icon('question-circle'),style="color: #1C2322; background-color: #3BE6C4; border-color: #18BC9C"))
} 

setDownTable = function(idText,idTable,idDownload,titleStatic = FALSE) {
  if (isTRUE(titleStatic)) {titleTable = idText} else {titleTable = strong(textOutput(idText))}
  fluidRow(
    h4(titleTable, align = "center"),
    div(dataTableOutput(idTable),style = "font-size:70%"),
    downloadButton(idDownload)
  )
}


downloadButtonTable = function(tablaTemp) {
  downloadHandler(
    filename = function() {
      paste0("descarga_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tablaTemp, file,row.names = F)
    }
  )
}