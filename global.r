## Por favor descomentar la línea de código 3 para instalar los paquetes requeridos para el funcionamiento de la herramienta.
## Luego, por favor comentarla con # para próximas corridas del aplicativo de Shiny.
# install.packages(c("leaflet","sf","dplyr","raster","plotly","rgdal","readxl","stringr","tidyr","dygraphs","xts","leaflet.extras","bslib"))

# Ajustar las rutas locales en las líneas de código 20

#install.packages(setdiff('terra', rownames(installed.packages())), repos='https://rspatial.r-universe.dev')
#install.packages(setdiff('raster', rownames(installed.packages())), repos='https://rspatial.r-universe.dev')

#packs = c("mapview","leaflet","sf","dplyr","plotly","rgdal","leafem","readxl","shinydashboard","stringr","tidyr","dygraphs","xts","DT","leaflet.extras","bslib","shinythemes","shinyalert","RSQLite","reshape2")
packs = c("Rcpp","mapview","leaflet","sf","dplyr","plotly","leafem","readxl","shinydashboard","stringr","tidyr","dygraphs","xts","DT","leaflet.extras","bslib","shinythemes","shinyalert","RSQLite","reshape2")

install.packages(setdiff('terra', rownames(installed.packages())), repos='https://rspatial.r-universe.dev')
install.packages(setdiff(packs, rownames(installed.packages()))) 

library(leaflet)
#library(raster)
library(sf)
library(dplyr)
try(library(raster),silent = T)
library(plotly)
#library(rgdal)
library(readxl)
library(stringr)
library(tidyr);
try(library(mapview),silent = T);
try(library(leafem),silent = T)
library(dygraphs);library(shinydashboard)
library(xts);library(DT);library(shinyalert);library(RSQLite)
library(leaflet.extras);library(bslib);library(shinythemes);library(reshape2)

RUTA = "data/"
# RUTA = "C:/jurrea/CTO_501_2020/Obligaciones/02_AccionesSeguimientoInformacion/PaginasShiny/SeguimientoDHIME/data/"

print("modulos...")
source("./functions.R")
source("./modules/AOSeguimiento_modulo.R",encoding = "UTF-8")
source("./modules/AOPorcentajes_modulo.R",encoding = "UTF-8")
source("./modules/AOAvances_modulo.R",encoding = "UTF-8")
source("./modules/AOSConvencionales_modulo.R",encoding = "UTF-8")
source("./modules/AOSConvencionalesTS_modulo.R",encoding = "UTF-8")
source("./modules/AOSaut_PT_AUT_10_modulo.R",encoding = "UTF-8")
source("./modules/AOSaut_NV_AUT_60_modulo.R",encoding = "UTF-8")
source("./modules/seriesHistoricas_modulo.R",encoding = "UTF-8")
source("./modules/coordErroneas_modulo.R",encoding = "UTF-8")
source("./modules/TSetiqDHIME_modulo.R",encoding = "UTF-8")

print("modulos...")
# conn <- dbConnect(RSQLite::SQLite(), paste0(RUTA,"HIST_PTPM_CON.db"))

SubRegs = sf::st_read(paste0(RUTA,"SUBREG.json"))
AreasOpPoly = sf::st_read(paste0(RUTA,"areas_operativas_2013_V11.json"))
Muni = sf::st_read(paste0(RUTA,"MGN_MPIO_POLITICO_V2 (1).json"))
print(1)
# SubRegs = geojsonio::geojson_read(paste0(RUTA,"SUBREG.json"), what = "sp")
# SubRegs = readOGR(dsn=paste0(RUTA,"Subregiones_-_Provincias_de_Colombia.shp"),layer = "Subregiones_-_Provincias_de_Colombia",encoding = "UTF-8")
SubRegs$NOM_SUBREG = iconv(SubRegs$NOM_SUBREG,from = "UTF-8", to = "latin1")

#catalogos
CNE_IDEAM = read_excel(paste0(RUTA,"CNE_IDEAM.xls"),sheet = 1)
CNE_OE = read_excel(paste0(RUTA,"CNE_OE.xls"),sheet = 1)
CNE_IDEAM$subred = NULL
CNE_IDEAM = as.data.frame(CNE_IDEAM)
CNE_OE = as.data.frame(CNE_OE)
CNE_IDEAM$FUENTE = "CNE_IDEAM"
CNE_OE$FUENTE = "CNE_OE"
CNE = rbind(CNE_IDEAM,CNE_OE) ## Union de ambos catalogos
CNE$CODIGO = as.numeric(CNE$CODIGO)
print(2)
codigosHist = read.table(paste0(RUTA,"codigosHist.txt"),sep = "\t",dec = ",",header = T)
CNE_IDEAM = CNE_IDEAM[CNE_IDEAM$CODIGO %in% codigosHist$CodigoEstacion,]
print(3)
CNE_IDEAMA = CNE_IDEAM
CNE_IDEAMA$AREA_OPERATIVA = substr(CNE_IDEAMA$AREA_OPERATIVA,1,17)
############ Listado de las estaciones ##############
LISTADOESTACIONES = read.table(paste0(RUTA,"todosCodigosEstaciones.txt"),sep = "\t",dec = ",",header = T)
LISTADOESTACIONESCONV = read.table(paste0(RUTA,"convCodigosEstaciones.txt"),sep = "\t",dec = ",",header = T)
LISTADOESTACIONESAUTS = read.table(paste0(RUTA,"autCodigosEstaciones.txt"),sep = "\t",dec = ",",header = T)
LISTADOESTACIONESNV_AUT_60 = read.table(paste0(RUTA,"autCodigosEstaciones_NV_AUT_60.txt"),sep = "\t",dec = ",",header = T)

load(paste0(RUTA,"estacionesEtiquetas.RData"))

BDEstaciones = read.table(paste0(RUTA,"codsSQLiteDB.txt"),sep = "\t",dec = ",",header = T)
print(4)
############ Analisis de series historicas ##############

load(paste0(RUTA,"analisisSeries.RData"))
print(5)
############ Analisis para la generacion de la grafica principal ##############

load(paste0(RUTA,"resumenHIST.RData"))
RESUMEN_OPT = resumenHIST; rm(resumenHIST)
names(RESUMEN_OPT)[1] = "VARIABLE"
RESUMEN_OPT$AREA_OPERATIVA = substr(RESUMEN_OPT$AREA_OPERATIVA,1,17)
RESUMEN_OPT_ANIO = RESUMEN_OPT
RESUMEN_OPT_ANIO$anio = as.numeric(substr(RESUMEN_OPT_ANIO$anio,1,4))
RESUMEN_ORG = tidyr::spread(data = RESUMEN_OPT_ANIO,key = RANGO,value = CONTEO)
colnames(RESUMEN_ORG) = c("VARIABLE","AREA_OPERATIVA","anio","COMPLETO","periodoA","periodoB",
                          "periodoC","periodoD","periodoE")
RESUMEN_ORG[is.na(RESUMEN_ORG)] = 0
RESUMEN_ORG$TOTAL = RESUMEN_ORG$COMPLETO+RESUMEN_ORG$periodoA+RESUMEN_ORG$periodoB+RESUMEN_ORG$periodoC+RESUMEN_ORG$periodoD+RESUMEN_ORG$periodoE
RESUMEN_ORG$anio = as.numeric(substr(RESUMEN_ORG$anio,1,4))

RESUMEN_ORG_PORC = RESUMEN_ORG
RESUMEN_ORG_PORC$COMPLETO = round((RESUMEN_ORG_PORC$COMPLETO / RESUMEN_ORG_PORC$TOTAL)*100,1)
RESUMEN_ORG_PORC$periodoA = round((RESUMEN_ORG_PORC$periodoA / RESUMEN_ORG_PORC$TOTAL)*100,1)
RESUMEN_ORG_PORC$periodoB = round((RESUMEN_ORG_PORC$periodoB / RESUMEN_ORG_PORC$TOTAL)*100,1)
RESUMEN_ORG_PORC$periodoC = round((RESUMEN_ORG_PORC$periodoC / RESUMEN_ORG_PORC$TOTAL)*100,1)
RESUMEN_ORG_PORC$periodoD = round((RESUMEN_ORG_PORC$periodoD / RESUMEN_ORG_PORC$TOTAL)*100,1)
RESUMEN_ORG_PORC$periodoE = round((RESUMEN_ORG_PORC$periodoE / RESUMEN_ORG_PORC$TOTAL)*100,1)
RESUMEN_ORG_PORC$TOTAL = NULL
print(6)

################## Se incluyen las tablas para la gráfica principal de las etiquetas de estaciones automáticas
load(paste0(RUTA,"resumenPT_AUT_10HIST.RData"))
print(7)
RESUMEN_PT_AUT10 = resumenPT_AUT_10HIST; rm(resumenPT_AUT_10HIST)
RESUMEN_ORG_PT_AUT10 = tidyr::spread(data = RESUMEN_PT_AUT10,key = RANGO,value = CONTEO)
colnames(RESUMEN_ORG_PT_AUT10) = c("AREA_OPERATIVA","anio","COMPLETO","periodoA","periodoB",
                          "periodoC","periodoD","periodoE")
RESUMEN_ORG_PT_AUT10[is.na(RESUMEN_ORG_PT_AUT10)] = 0
RESUMEN_ORG_PT_AUT10$TOTAL = RESUMEN_ORG_PT_AUT10$COMPLETO+RESUMEN_ORG_PT_AUT10$periodoA+RESUMEN_ORG_PT_AUT10$periodoB+RESUMEN_ORG_PT_AUT10$periodoC+RESUMEN_ORG_PT_AUT10$periodoD+RESUMEN_ORG_PT_AUT10$periodoE
RESUMEN_ORG_PT_AUT10$anio = as.numeric(substr(RESUMEN_ORG_PT_AUT10$anio,1,4))
# RESUMEN_ORG_PT_AUT10 = RESUMEN_ORG_PT_AUT10[RESUMEN_ORG_PT_AUT10$anio %in% 2010:2021,]


load(paste0(RUTA,"resumenNV_AUT_60HIST.RData"))

RESUMEN_NV_AUT_60 = resumenNV_AUT_60HIST; rm(resumenNV_AUT_60HIST)
RESUMEN_ORG_NV_AUT_60 = tidyr::spread(data = RESUMEN_NV_AUT_60,key = RANGO,value = CONTEO)
colnames(RESUMEN_ORG_NV_AUT_60) = c("AREA_OPERATIVA","anio","COMPLETO","periodoA","periodoB",
                                   "periodoC","periodoD","periodoE")
RESUMEN_ORG_NV_AUT_60[is.na(RESUMEN_ORG_NV_AUT_60)] = 0
RESUMEN_ORG_NV_AUT_60$TOTAL = RESUMEN_ORG_NV_AUT_60$COMPLETO+RESUMEN_ORG_NV_AUT_60$periodoA+RESUMEN_ORG_NV_AUT_60$periodoB+RESUMEN_ORG_NV_AUT_60$periodoC+RESUMEN_ORG_NV_AUT_60$periodoD+RESUMEN_ORG_NV_AUT_60$periodoE
RESUMEN_ORG_NV_AUT_60$anio = as.numeric(substr(RESUMEN_ORG_NV_AUT_60$anio,1,4))
print(8)

############ Analisis de los avnaces por estacion ##############
load(paste0(RUTA,"unionMesDepConteo.RData"))
AVANCE_ESTACION = unionMesDepConteo; rm(unionMesDepConteo)
print(9)
############ Analisis para la generacion de la tabla asociada a la primera grafica ##############

load(paste0(RUTA,"conteoConsolidadoHIST.RData"))
conteoConsolidado = conteoConsolidadoHIST1; rm(conteoConsolidadoHIST1)
names(conteoConsolidado)[4] = "VARIABLE"
# conteoConsolidado$AREA_OPERATIVA = substr(conteoConsolidado$AREA_OPERATIVA,1,17)
conteoConsolidado$anio = as.numeric(substr(conteoConsolidado$anio,1,4))
############ Analisis para la generacion de la primera tabla ##############
load(paste0(RUTA,"PrimeraTabla.RData"))
load(paste0(RUTA,"SegundaTabla.RData"))
# load(paste0(RUTA,"PorcentajesMes.RData"))
print(10)
TABLA_1 = PrimeraTabla; rm(PrimeraTabla)
TABLA_2 = SegundaTabla; rm(SegundaTabla)
TABLA_2$AO = paste0("AO ",substr(TABLA_2$AO,16,17))
names(TABLA_2)[4:15] = c("ENEconteo","FEBconteo","MARconteo","ABRconteo","MAYconteo","JUNconteo",
                         "JULconteo","AGOconteo","SEPconteo","OCTconteo","NOVconteo","DICconteo")
TABLA_2S = merge(TABLA_1,TABLA_2,by = c("VARIABLE","anio","AO"),all = T)
# TABLA_M = PorcentajesMes

# TABLA_1 = read.table(paste0(RUTA,"PrimeraTabla.txt"),sep = "\t",dec = ",",header = T)
# TABLA_2 = read.table(paste0(RUTA,"SegundaTabla.txt"),sep = "\t",dec = ",",header = T)

############ Analisis para la generacion de la segunda grafica ##############

load(paste0(RUTA,"ConteoMes.RData"))
ConteoMes = tidyr::spread(data = ConteoMes,key = RANGO,value = CONTEO)
colnames(ConteoMes) = c("VARIABLE","AREA_OPERATIVA","anio","mes","periodoA","periodoB",
                          "periodoC","periodoD","periodoE","periodoF")
ConteoMes$AREA_OPERATIVA = substr(ConteoMes$AREA_OPERATIVA,1,17)
print(11)

load(paste0(RUTA,"Grafica2.RData"))
TAB_GRAF_2 = Grafica2; rm(Grafica2)
print(12)

RESUMEN_ORG_PORC_MES = TAB_GRAF_2
RESUMEN_ORG_PORC_MES$periodoA = round((RESUMEN_ORG_PORC_MES$periodoA / RESUMEN_ORG_PORC_MES$TOTAL)*100,1)
RESUMEN_ORG_PORC_MES$periodoB = round((RESUMEN_ORG_PORC_MES$periodoB / RESUMEN_ORG_PORC_MES$TOTAL)*100,1)
RESUMEN_ORG_PORC_MES$periodoC = round((RESUMEN_ORG_PORC_MES$periodoC / RESUMEN_ORG_PORC_MES$TOTAL)*100,1)
RESUMEN_ORG_PORC_MES$periodoD = round((RESUMEN_ORG_PORC_MES$periodoD / RESUMEN_ORG_PORC_MES$TOTAL)*100,1)
RESUMEN_ORG_PORC_MES$periodoE = round((RESUMEN_ORG_PORC_MES$periodoE / RESUMEN_ORG_PORC_MES$TOTAL)*100,1)
RESUMEN_ORG_PORC_MES$periodoF = round((RESUMEN_ORG_PORC_MES$periodoF / RESUMEN_ORG_PORC_MES$TOTAL)*100,1)
RESUMEN_ORG_PORC_MES$TOTAL = NULL

############ Análisis para la generacion de la tabla de las estaciones ##############
load(paste0(RUTA,"ConsolidadoConteoPorEstacion.RData"))
TABLA_3 = ConsolidadoConteoPorEstacion; rm(ConsolidadoConteoPorEstacion)
print(13)
# load(paste0(RUTA,"TABLA_3S.RData"))
# TABLA_3S = RESUMEN_OPT_MES_L; rm(RESUMEN_OPT_MES_L)
# TABLA_3S = merge(TABLA_3,CNE[,c("nombre","latitud","longitud")],by = "nombre",all.x = T)
# save(TABLA_3S,file = "C:/jurrea/CTO_501_2020/Obligaciones/02_AccionesSeguimientoInformacion/PaginasShiny/SeguimientoDHIME/data/TABLA_3S.RData")

# TABLA_3 = read.table(paste0(RUTA,"ConsolidadoConteoPorEstacion.txt"),sep = "\t",dec = ",",header = T)
# TABLA_3$CodigoEstacion = NULL
############ Analisis para la generacion de la tabla de cantidad de datos anuales por estaciones ##############
load(paste0(RUTA,"ConsolidadoConteoAnualEstacion.RData"))
TABLA_4 = ConsolidadoConteoAnualEstacion; rm(ConsolidadoConteoAnualEstacion)
load(paste0(RUTA,"ConsolidadoPorcAnualEstacion.RData"))
TABLA_45 = RESUMEN_CONTEO_EST1; rm(RESUMEN_CONTEO_EST1)
print(14)
############ Analisis para la generación de la tabla de cantidad de datos mensuales por estaciones ##############
load(paste0(RUTA,"ConsolidadoConteoMesEstacion.RData"))
TABLA_Y = ConsolidadoConteoMesEstacion; rm(ConsolidadoConteoMesEstacion)
print(15)
############ Analisis para la generación de la tabla de cantidad de datos mensuales originales para varias estaciones ##############
load(paste0(RUTA,"ConsolidadoConteoMesOrigEstacion.RData "))
TABLA_MESORIG = CONTEO_DATOS_MES_ORG
print(16)
############ Analisis para la generación de la tabla de cantidad de dias con datos mensuales alternativos para varias estaciones ##############
load(paste0(RUTA,"ConsolidadoConteoMesAlteEstacion.RData "))
TABLA_MESALTE = CONTEO_DATOS_MES_ALT
print(17)
load(paste0(RUTA,"ConsolidadoConteoMesOrigEstacionPorc.RData "))
TABLA_MESORIGPORC = CONTEO_DATOS_MES_ORG_PORC
# TABLA_4 = read.table(paste0(RUTA,"ConsolidadoConteoAnualEstacion.txt"),sep = "\t",dec = ",",header = T)


############ Análisis para la generación de la tabla de la diferencia porcentual de datos  ##############
load(paste0(RUTA,"avancesVariables.RData"))
TABLA_5 = avancesVariables; rm(avancesVariables)
# TABLA_5$variable = substr(TABLA_5$variable,1,nchar(TABLA_5$variable) - 3)
print(18)
############ Tabla de la primera grafica de las diferencias porcentuales de datos  ##############
load(paste0(RUTA,"grafAvancePorAO.RData"))
GRAF_1_AVANCE = union; rm(union)
# GRAF_1_AVANCE$variable = substr(GRAF_1_AVANCE$variable,1,nchar(GRAF_1_AVANCE$variable) - 3)
print(19)
############ Tabla de la segunda grafica de las diferencias porcentuales de datos  ##############
load(paste0(RUTA,"grafAvancePorMes.RData"))
GRAF_2_AVANCE = consolidadoConteo; rm(consolidadoConteo)
# GRAF_2_AVANCE$variable = substr(GRAF_2_AVANCE$variable,1,nchar(GRAF_2_AVANCE$variable) - 3)
print(20)
############ Análisis para la generación de la tabla anual de datos de PT_AUT_10  ##############
load(paste0(RUTA,"conteoDiasAnualPT_AUT_10.RData"))
TABLA_8 = conteoDiasAnualPT_AUT_10; rm(conteoDiasAnualPT_AUT_10)
print(21)
############ Análisis para la generación de la tabla anual de datos de NV_AUT_60  ##############
load(paste0(RUTA,"conteoDiasAnualNV_AUT_60.RData"))
TABLA8NV_AUT_60 = conteoDiasAnualNV_AUT_60; rm(conteoDiasAnualNV_AUT_60)
load(paste0(RUTA,"conteoDiasAnualPT_AUT_10_B.RData"))
TABLA_88 = conteoDiasAnualPT_AUT_10_B; rm(conteoDiasAnualPT_AUT_10_B)
TABLA_88_CODS = data.frame("codigo" = unique(TABLA_88$Codigo), stringsAsFactors = F)
TABLA_88_CODS = merge(TABLA_88_CODS,CNE[,c("CODIGO","AREA_OPERATIVA")],by.x = "codigo",by.y = "CODIGO",all.x = T)
TABLA_88_CODS$AREA_OPERATIVA = substr(TABLA_88_CODS$AREA_OPERATIVA,1,17)
print(22)
############ Análisis para la generación de la tabla mensual de datos de PT_AUT_10  ##############
load(paste0(RUTA,"conteoDiasMesPT_AUT_10.RData"))
TABLA_9 = conteoDiasMesPT_AUT_10; rm(conteoDiasMesPT_AUT_10)
print(23)
############ Análisis para la generación de la tabla mensual de datos de NV_AUT_60  ##############
load(paste0(RUTA,"conteoDiasMesNV_AUT_60.RData"))
TABLA_9NV_AUT_60 = conteoDiasMesNV_AUT_60; rm(conteoDiasMesNV_AUT_60)
print(24)
########### Analisis de las metas ##############################
load(paste0(RUTA,"meta.RData"))
TABLA_META = metaUnion; rm(metaUnion)
TABLA_META = TABLA_META[,c("VARIABLE","AREA_OPERATIVA","cantidadEstaciones","anio","metaAnio","mesesProcesados","porcMeta")]
names(TABLA_META)[3] = "Estaciones"
TABLA_META$AREA_OPERATIVA = paste0("AO ",substr(TABLA_META$AREA_OPERATIVA,16,17)) 

load(paste0(RUTA,"metaMes.RData"))
TABLA_META_MES = meta2


load(paste0(RUTA,"metaBeta.RData"))
TABLA_META_2 = metaUnionA; rm(metaUnionA)
TABLA_META_2 = TABLA_META_2[,c("VARIABLE","AREA_OPERATIVA","cantidadEstaciones","anio","metaAnio","mesesProcesados","porcMeta")]
names(TABLA_META_2)[3] = "Estaciones"
TABLA_META_2$AREA_OPERATIVA = paste0("AO ",substr(TABLA_META_2$AREA_OPERATIVA,16,17))

load(paste0(RUTA,"metaBetaMes.RData"))
TABLA_META_MES_2 = metaGamma; rm(metaGamma)
print(25)
############### Archivos para las subregiones de cada departamento ############################################

CNE_CRUCE_DIVIPOLA = read.table(paste0(RUTA,"CNE_CRUCE_DIVIPOLA.txt"),sep = "\t",dec = ",",header = T)

MunicipioSubregion = read.table(paste0(RUTA,"MunicipioSubregion.txt"),sep = "\t",dec = ",",header = T)

print(26)

############### Archivo con el analisis de estaciones con coordenadas erroneas ############################################

load(paste0(RUTA,"CNE_UNION.RData"))
CNE_UNION = CNE_UNION[,c("codigo","nombreCNE_2014","longitudCNE_2014","latitudCNE_2014","entidadCNE2014","nombreCNE_2021","municipioCNE_2021","departamentoCNE_2021","estadoCNE_2021","AREA_OPERATIVA",
                         "categoriaCNE_2021","longitudCNE_2021","latitudCNE_2021","entidadCNE2021","orientacion","distancia","COMPARACION","DISTANCIA","DIVIPOLA","MUNICIPIO_SHAPE","DEPTO_SHAPE")]

# CNE_UNION = CNE_UNION[,c("codigo","nombreCNE_2014","longitudCNE_2014","latitudCNE_2014","entidadCNE2014","nombreCNE_2021","municipioCNE_2021","departamentoCNE_2021","estadoCNE_2021","AREA_OPERATIVA",
#                          "categoriaCNE_2021","longitudCNE_2021","latitudCNE_2021","entidadCNE2021","orientacion","distancia","COMPARACION","DISTANCIA")]


print(27)
############ Cargue historico etiquetas ##############

# load(paste0(RUTA,"PTPM_CON.RData"))
# HIST_PTPM = Consolidado
# HIST_PTPM = HIST_PTPM[,c("CodigoEstacion","Fecha","Valor")]


# HIST_PTPM = subset(HIST_PTPM,HIST_PTPM$CodigoEstacion==21205791)
# HIST_PTPM = HIST_PTPM[HIST_PTPM$CodigoEstacion == 21205791,]
# xts_uno = xts::xts(HIST_PTPM$Valor,order.by = HIST_PTPM$Fecha,frequency = 365)
# aaa = read.zoo(HIST_PTPM)
# dygraph(data = aaa, main = "Serie de tiempo Prec", xlab = "Fecha", ylab = "Valor")


############### Archivo con el consolidado de series de DHIME ############################################
load(paste0(RUTA,"tsDHIMECom.RData"))  
print(28)
