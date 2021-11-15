# 
# Autores: Amo Nestares, Beatriz; Barcina Muñoz, Víctor; Lozano Juárez, Samuel
# Organización: Universidad de Burgos (UBU)
# Fecha: 12 - Noviembre - 2021
# Grado: 3º Ingeniería de la Salud
# Asignatura: Fuentes de Datos Biomédicas y Web Semántica
# 
# El código que aparece a continuación forma parte del proyecto de investigación "Cambio Climático y ECV"©. Contiene la información necesaria para poder cargar los datos, analizarlos y procesar y comunicar los resultados obtenidos.
# El proyecto se encuentra amparado bajo una licencia creativa Creative Commons.
# 


# CARGA DE LOS PAQUETES ---------------------------------------------------
# Carga de los paquetes que se van a emplear a lo largo del código
library(climaemet)
library(dplyr)
library(tidyverse)
library(fs)
library(readxl)

# API KEY AEMET -----------------------------------------------------------
# Si no se detecta la presencia de una API KEY de AEMET, se abrirá la página oportuna para su obtención
if(aemet_detect_api_key() == FALSE){
  browseURL("https://opendata.aemet.es/centrodedescargas/altaUsuario?")
}

# Instalación de la API KEY de AEMET en el equipo para poder acceder a los datos de climatología
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzYW11ZWxsb3phbm9qdWFyZXpAZ21haWwuY29tIiwianRpIjoiZjEzZWM0NDktOTc1Ny00MGNjLTg5MDktZmFhOTZjZmFkMTcxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MzYzNzMwMDAsInVzZXJJZCI6ImYxM2VjNDQ5LTk3NTctNDBjYy04OTA5LWZhYTk2Y2ZhZDE3MSIsInJvbGUiOiIifQ.HSs6bokk9cYquyGSRBOSC6_fxQoK8ZSlRQR64qMtBns", overwrite = TRUE, install = TRUE)


# DECLARACIÓN VARIABLES GLOBALES --------------------------------------------

# * Vector de provincias --------------------------------------------------
# Vector que contiene el nombre de las 52 provincias de España y que va a ser empleado para dar nombre a los diferentes data frames
Provincias <- c("Alava","Albacete","Alicante","Almeria","Asturias","Avila","Badajoz","Islas_Baleares","Barcelona","Bizkaia","Burgos","Caceres","Cadiz","Cantabria","Castellon","Ceuta","Ciudad_Real","Cordoba","A_Coruna","Cuenca","Girona","Granada","Guadalajara","Guipuzkoa","Huelva","Huesca","Jaen","Leon","Lleida","Lugo","Madrid","Malaga","Melilla","Murcia","Navarra","Ourense","Palencia","Las_Palmas","Pontevedra","La_Rioja","Salamanca","Tenerife","Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid","Zamora","Zaragoza")

# * Vector de estaciones --------------------------------------------------
# Vector que contiene el identificador de cada estación provincial de AEMET. Ocupan la misma posición en este vector que su provincia correspondiente en el vector Provincias, así el identificador 9091O se corresponde con Álava, el 8175 con Albacete ... y el 9434 con Zaragoza.
Estaciones <- c("9091O","8175","8019","6325O","1212E","2444","4452","B954","0201D","1082","2331","3469A","5960","1109","8500A","5000C","4121","5402","1387E","8096","0367","5530E","3168D","1014A","4642E","9898","5270B","2661","9771C","1505","3129","6155A","6000A","7178I","9263D","1690A","2374X","C029O","1495","9170","2867","C447A","2465","5783","2030","9981A","9381I","3260B","8416","2422","2614","9434")



# CARGA DE DATOS ----------------------------------------------------------

# * Datos meteorológicos --------------------------------------------------
# A continuación empleando un bucle for y la función aemet_monthly_period() del paquete climaemet vamos a importar los datos meteorológicos de cada provincia en el periodo temportal de 2010 a 2019, creando un Data Frame para cada provincia.
for (i in 1:length(Provincias)){
  Nam <- paste(Provincias[i], "Meteo", sep="")
  Objeto <- aemet_monthly_period(station = Estaciones[i], start = 2010, end = 2019)
  assign(Nam, Objeto)
}

# Debido a que las estaciones seleccionadas de Guadalajara, Málaga, Palencia, Soria y Valladolid carecen de la información de determinados años, vamos a inconporar la información ausente de forma manual empleando la información para esos años de otras estaciones de esas mismas provincias.
# Además en Guadalajara vamos a sustituir la información del año 2011 de la estación 3168D por la de 3168C.
GuadalajaraMeteo <- GuadalajaraMeteo[14:nrow(GuadalajaraMeteo), ]
GuadalajaraMeteo <- bind_rows(GuadalajaraMeteo, aemet_monthly_clim(station = "3168C", year = 2010), aemet_monthly_clim(station = "3168C", year = 2011))

MalagaMeteo <- bind_rows(MalagaMeteo, aemet_monthly_clim(station = "6084X", year = 2015))
PalenciaMeteo <- bind_rows(PalenciaMeteo, aemet_monthly_clim(station = "2374X", year = 2016))
SoriaMeteo <- bind_rows(SoriaMeteo, aemet_monthly_clim(station = "2030", year = 2011))
ValladolidMeteo <- bind_rows(ValladolidMeteo, aemet_monthly_clim(station = "2422", year = 2012))

# * Datos de morbilidad ---------------------------------------------------
DFMorbilidad <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="morbilidad")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMorbilidad <- bind_rows(DFMorbilidad, temporal)
}

# Incluimos también la posibilidad de importar los datos empleando programación funcional (función map_df())

# DFMorb_func <- dir_ls("INPUT", regexp = "morbilidad") %>% 
#   map_df(read_excel, .id = "Periodo")
# 
# DFMorb_func <- subset(DFMorb_func, select = -c(`Encuesta de morbilidad hospitalaria 2011`,`Encuesta de morbilidad hospitalaria 2012`,`Encuesta de morbilidad hospitalaria 2013`,`Resultados por comunidades autónomas y provincias`,`Lista detallada`,`Encuesta de morbilidad hospitalaria. Año 2016`,`Encuesta de morbilidad hospitalaria. Año 2017`,`Encuesta de morbilidad hospitalaria. Año 2018`,`Encuesta de morbilidad hospitalaria. Año 2019`))%>%
#   drop_na(.)
# 
# DFMorb_headers <- c()
# for (i in DFMorb_func[1, ]){
#   DFMorb_headers <- c(DFMorb_headers, i)
# }
# 
# colnames(DFMorb_func) = DFMorb_headers
# DFMorb_func <- filter(DFMorb_func, CAUSA != "CAUSA")

# * Datos de mortalidad nacional mensual---------------------------------------------------
DFMort_Mens <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="Mort_nacion")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMort_Mens <- bind_rows(DFMort_Mens, temporal)
}

# * Datos de mortalidad provincial ----------------------------------------
DFMort_Mens <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="Mort")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMort_Mens <- bind_rows(DFMort_Mens, temporal)
}


# REFINAMIENTO DE LOS DATOS -----------------------------------------------

# * Datos meteorológicos --------------------------------------------------

# * Datos de morbilidad --------------------------------------------------
# Seleccionamos únicamente los datos relativos a enfermedades del sistema circulatorio, eliminamos la columna CAUSA (ya no es necesaria), incluimos una columna que especifique el año de los datos y el sexo al que se refieren los datos.
DFMorbilidad <- filter(DFMorbilidad, CAUSA %in% c("390-459 VII ENFERMEDADES DEL SISTEMA CIRCULATORIO", "0900 ENFERMEDADES DEL APARATO CIRCULATORIO I00-I99")) %>% 
  bind_cols(Periodo = rep(c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), each=3), Sexo = rep(c("Ambos sexos", "Hombres", "Mujeres"), 10)) %>% 
  subset(select = - CAUSA)

# A continuación vamos a eliminar las columnas referentes a las comunidades autónomas, para quedarnos exclusivamente con las columnas de provincias.
DFMorbilidad <- subset(DFMorbilidad, select = -c(`Total Nacional`,`Andalucía`,`Aragón`,`Canarias`,`Castilla y León`,`Castilla - La Mancha`,`Cataluña`,`Comunitat Valenciana`,`Extremadura`,`Galicia`,`País Vasco`,`Extranjero`))

# Por último vamos a renombrar las columnas para emplear un conjunto de nombres uniforme para las provincias. Ese conjunto es el definido en el vector Provincias y colocamos las columnas "Periodo" y "Sexo" como las 2 primeras.
DFMorbilidad <- DFMorbilidad[ ,order(names(DFMorbilidad))] %>% 
  relocate(.,`Araba/Álava`, .before = `Albacete`) %>% 
  relocate(., `Gipuzkoa`, .after = `Guadalajara`)

j <- 1
for (i in 1:length(DFMorbilidad)){
  if (!colnames(DFMorbilidad)[i] %in% c("Sexo","Periodo")){
    colnames(DFMorbilidad)[i] = Provincias[j]
    j <- j+1
  }
}

DFMorbilidad <- relocate(DFMorbilidad,c(`Periodo`,`Sexo`),.before = `Alava`)

# * Datos de mortalidad nacional mensual -------------------------------------------

# * Datos de mortalidad provincial ----------------------------------------
