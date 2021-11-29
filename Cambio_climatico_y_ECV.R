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
library(ggplot2)

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
Estaciones <- c("9091O","8175","8019","6325O","1212E","2444","4452","B954","0200E","1082","2331","3469A","5960","1109","8500A","5000C","4121","5402","1387","8096","0367","5530E","3168D","1014A","4642E","9898","5270B","2661","9771C","1505","3129","6155A","6000A","7178I","9263D","1690A","2374X","C029O","1495","9170","2867","C447A","2465","5783","2030","9981A","9381I","3260B","8416","2422","2614","9434")

# CARGA DE DATOS ----------------------------------------------------------

# * Datos meteorológicos --------------------------------------------------
# A continuación empleando un bucle for y la función aemet_monthly_period() del paquete climaemet vamos a importar los datos meteorológicos de cada provincia en el periodo temporal de 2010 a 2019, creando un Data Frame para cada provincia.
for (i in 1:length(Provincias)){
  Nam <- paste(Provincias[i], "Meteo", sep="")
  Objeto <- aemet_monthly_period(station = Estaciones[i], start = 2010, end = 2019)
  assign(Nam, Objeto)
}

# Debido a que las estaciones seleccionadas de Guadalajara, Málaga, Palencia, Soria y Valladolid carecen de la información de determinados años, vamos a inconporar la información ausente de forma manual empleando la información para esos años de otras estaciones de esas mismas provincias.
# Además en Guadalajara vamos a sustituir la información del año 2011 de la estación 3168D por la de 3168C.
GuadalajaraMeteo <- GuadalajaraMeteo[14:nrow(GuadalajaraMeteo), ] # de esta manera logramos eliminar las filas referentes al año 2011
GuadalajaraMeteo <- bind_rows(GuadalajaraMeteo, aemet_monthly_clim(station = "3168C", year = 2010), aemet_monthly_clim(station = "3168C", year = 2011))

# Vamos a reordenar las filas por fecha, para ello primero debemos añadir un "0" en los meses del 1 al 9, para que no tome como mayor el mes 2 que el 12 (vamos a realizarlo mediante programación funcional para reciclar código y no copiar y pegar).
Reorder <- function(provincia){
  for (i in 1:length(provincia$fecha)){
    if(nchar(provincia$fecha[i])<7){
      provincia$fecha[i] <- paste(str_sub(provincia$fecha[i], start = 1L, end = 5L), "0", str_sub(provincia$fecha[i], start = -1L, end=-1L), sep = "")
    }
  }
  return(arrange(provincia, fecha))
}

GuadalajaraMeteo <- Reorder(GuadalajaraMeteo)


# A continuación añadimos de forma manual los años que faltan en las estaciones mencionadas (y reordenamos cada una de ellas por fecha)
MalagaMeteo <- bind_rows(MalagaMeteo, aemet_monthly_clim(station = "6084X", year = 2015))
MalagaMeteo <- Reorder(MalagaMeteo)

PalenciaMeteo <- bind_rows(PalenciaMeteo, aemet_monthly_clim(station = "2374X", year = 2016))
PalenciaMeteo <- Reorder(PalenciaMeteo)

SoriaMeteo <- bind_rows(SoriaMeteo, aemet_monthly_clim(station = "2030", year = 2011))
SoriaMeteo <- Reorder(SoriaMeteo)

ValladolidMeteo <- bind_rows(ValladolidMeteo, aemet_monthly_clim(station = "2422", year = 2012))
ValladolidMeteo <- Reorder(ValladolidMeteo)



# * Datos de morbilidad ---------------------------------------------------
DFMorbilidad <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="morbilidad")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6, col_types = c("text",rep("numeric",64))) 
  DFMorbilidad <- bind_rows(DFMorbilidad, temporal)
}

str(DFMorbilidad)

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
DFMort_Prov <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="mort_prov")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMort_Prov <- bind_rows(DFMort_Prov, temporal)
}

# REFINAMIENTO DE LOS DATOS -----------------------------------------------

# * Datos meteorológicos --------------------------------------------------

Mod_meteo <- function(prov){
  Objeto <- get(prov)
  Objeto <- select(Objeto, c(`fecha`,`tm_max`,`tm_min`,`q_max`,`q_min`,`p_sol`,`hr`)) %>% 
    filter(.data = . , !`fecha` %in% c("2010-13","2011-13","2012-13","2013-13","2014-13","2015-13","2016-13","2017-13","2018-13","2019-13")) %>% 
    bind_cols(Periodo = rep(c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), each=12), Mes = rep(c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), 10)) %>% 
    relocate(.,c(`Periodo`,`Mes`),.before = `fecha`) %>% 
    subset(select = -`fecha`)
  
  for (i in 1:length(Objeto$q_max)){
    Objeto$q_max[i] <- str_sub(string = Objeto$q_max[i], start = 1L, end = -5L)
    Objeto$q_min[i] <- str_sub(string = Objeto$q_min[i], start = 1L, end = -5L)
  }
  
  Objeto$q_max <- as.numeric(Objeto$q_max)
  Objeto$q_min <- as.numeric(Objeto$q_min)
  
  return(Objeto)
}

for (i in 1:length(Provincias)){
  Nam <- paste(Provincias[i],"Meteo",sep = "")
  assign(Nam,Mod_meteo(Nam))
}

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

DFMorbilidad <- relocate(DFMorbilidad,c(`Periodo`,`Sexo`),.before = `Alava`) %>% 
  pivot_longer(data = ., names_to = "Provincias", values_to = "Altas", cols= c(Alava:Zaragoza))

# * Datos de mortalidad nacional mensual -------------------------------------------



# * Datos de mortalidad provincial ----------------------------------------
DFMort_Prov <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="mort_prov")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMort_Prov <- bind_rows(DFMort_Prov, temporal)
}DFMort_Prov <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="mort_prov")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMort_Prov <- bind_rows(DFMort_Prov, temporal)
}


# ANÁLISIS DE LOS DATOS ---------------------------------------------------


# * Datos meteorológicos --------------------------------------------------

Generador_Av_Anom <- function(provincia){
  Interna <- left_join(x = provincia, y = provincia %>% 
                         group_by(Mes) %>% 
                         summarise(Av_tmax = mean(tm_max, na.rm = TRUE),Av_tmin = mean(tm_min, na.rm = TRUE),Av_qmax = mean(q_max, na.rm = TRUE),Av_qmin = mean(q_min, na.rm = TRUE),Av_psol = mean(p_sol, na.rm = TRUE),Av_hr = mean(hr, na.rm = TRUE))
                       , by = "Mes") %>% 
    mutate(Anom_tmax = (`tm_max`-`Av_tmax`), Periodo_Mes = paste(Periodo,Mes,sep=" "), Anom_tmin = (`tm_min`-`Av_tmin`), Anom_qmax = (`q_max`-`Av_qmax`), Anom_qmin = ((`q_min`-`Av_qmin`)), Anom_psol = ((`p_sol`-`Av_psol`)), Anom_hr = ((`hr`-`Av_hr`)))
  return(Interna)
}

for (i in Provincias){
  Nam <- paste(i,"Meteo",sep = "")
  assign(Nam, Generador_Av_Anom(get(Nam)))
}

Graf_filt_Meteo <- function(provincia){
  graf_temp <- ggplot(provincia , aes(x = Periodo_Mes)) + 
    geom_line(aes(y = Anom_tmax**2, group = 1), colour = "red",) + 
    geom_point(size = 1.5, aes(y = Anom_tmax**2), colour = "red") + 
    geom_smooth(aes( y = Anom_tmax**2, group = 1), fill = "red", alpha = 0.25, colour = "red", level = 0.995) +
    geom_line(aes(y = Anom_tmin**2,  group = 1), colour = "blue",) + 
    geom_point(size = 1.5, aes(y = Anom_tmin**2), colour = "blue") + 
    geom_smooth(aes(y = Anom_tmin**2, group = 1), fill = "blue", alpha = 0.25, colour = "blue", level = 0.995) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Periodo Mes",
         y = "Anomalía^2") + 
    ggtitle(label = "Anomalía Temperatura", subtitle = as.character(quote(MadridMeteo)))
  
  graf_presion <- ggplot(provincia , aes(x = Periodo_Mes)) + 
    geom_line(aes(y = Anom_qmax**2, group = 1), colour = "red") + 
    geom_point(size = 1.5, aes(y = Anom_qmax**2), colour = "red") + 
    geom_smooth(aes( y = Anom_qmax**2, group = 1), fill = "red", alpha = 0.25, colour = "red", level = 0.995) +
    geom_line(aes(y = Anom_qmin**2,  group = 1), colour = "blue",) + 
    geom_point(size = 1.5, aes(y = Anom_qmin**2), colour = "blue") + 
    geom_smooth(aes(y = Anom_qmin**2, group = 1), fill = "blue", alpha = 0.25, colour = "blue", level = 0.995) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
    labs(x = "Periodo Mes",
         y = "Anomalía^2") + 
    ggtitle(label = "Anomalía Presión Atm", subtitle = as.character(quote(MadridMeteo)))
  
  graf_sol_hr <- ggplot(provincia , aes(x = Periodo_Mes)) + 
    geom_line(aes(y = Anom_psol**2, group = 1), colour = "red") + 
    geom_point(size = 1.5, aes(y = Anom_psol**2), colour = "red") + 
    geom_smooth(aes( y = Anom_psol**2, group = 1), fill = "red", alpha = 0.25, colour = "red", level = 0.995) +
    geom_line(aes(y = Anom_hr**2,  group = 1), colour = "blue",) + 
    geom_point(size = 1.5, aes(y = Anom_hr**2), colour = "blue") + 
    geom_smooth(aes(y = Anom_hr**2, group = 1), fill = "blue", alpha = 0.25, colour = "blue", level = 0.995) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    labs(x = "Periodo Mes",
         y = "Anomalía^2") + 
    ggtitle(label = "Anomalía Insolación y Humedad", subtitle = as.character(quote(provincia)))
  
}

Graf_filt_Meteo(A_CorunaMeteo)

IC_tmax <- ggplot_build(grafica)$data[[3]] %>% 
  select(. , c(ymax)) %>% 
  rename(high_IC_tmax = ymax)

IC_tmin <- ggplot_build(grafica)$data[[6]] %>% 
  select(. , c(ymax)) %>% 
  rename(high_IC_tmin = ymax)

y <- bind_cols(y, IC_tmax, IC_tmin)
