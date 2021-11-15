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

# Carga de los paquetes que se van a emplear a lo largo del código
library(climaemet)
library(dplyr)

# Si no se detecta la presencia de una API KEY de AEMET, se abrirá la página oportuna para su obtención
if(aemet_detect_api_key() == FALSE){
  browseURL("https://opendata.aemet.es/centrodedescargas/altaUsuario?")
}

# Instalación de la API KEY de AEMET en el equipo para poder acceder a los datos de climatología
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzYW11ZWxsb3phbm9qdWFyZXpAZ21haWwuY29tIiwianRpIjoiZjEzZWM0NDktOTc1Ny00MGNjLTg5MDktZmFhOTZjZmFkMTcxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MzYzNzMwMDAsInVzZXJJZCI6ImYxM2VjNDQ5LTk3NTctNDBjYy04OTA5LWZhYTk2Y2ZhZDE3MSIsInJvbGUiOiIifQ.HSs6bokk9cYquyGSRBOSC6_fxQoK8ZSlRQR64qMtBns", overwrite = TRUE, install = TRUE)

# Vector que contiene el nombre de las 52 provincias de España y que va a ser empleado para dar nombre a los diferentes data frames
Provincias <- c("Alava","Albacete","Alicante","Almeria","Asturias","Avila","Badajoz","Islas_Baleares","Barcelona","Bizkaia","Burgos","Caceres","Cadiz","Cantabria","Castellon","Ceuta","Ciudad_Real","Cordoba","A_Coruna","Cuenca","Girona","Granada","Guadalajara","Guipuzkoa","Huelva","Huesca","Jaen","Leon","Lleida","Lugo","Madrid","Malaga","Melilla","Murcia","Navarra","Ourense","Palencia","Las_Palmas","Pontevedra","La_Rioja","Salamanca","Tenerife","Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid","Zamora","Zaragoza")

# Vector que contiene el identificador de cada estación provincial de AEMET. Ocupan la misma posición en este vector que su provincia correspondiente en el vector Provincias, así el identificador 9091O se corresponde con Álava, el 8175 con Albacete ... y el 9434 con Zaragoza.
Estaciones <- c("9091O","8175","8019","6325O","1212E","2444","4452","B954","0201D","1082","2331","3469A","5960","1109","8500A","5000C","4121","5402","1387E","8096","0367","5530E","3168D","1014A","4642E","9898","5270B","2661","9771C","1505","3129","6155A","6000A","7178I","9263D","1690A","2374X","C029O","1495","9170","2867","C447A","2465","5783","2030","9981A","9381I","3260B","8416","2422","2614","9434")

for (i in 1:length(Provincias)){
  Nam <- paste(Provincias[i], "Meteo", sep="")
  Objeto <- aemet_monthly_period(station = Estaciones[i], start = 2010, end = 2019)
  assign(Nam, Objeto)
}



tbl <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))


library(tidyverse)

library(readxl)
morbilidad2010 <- read_excel("INPUT/morbilidad2010.xlsx", 
                             sheet = "tabla-0", skip = 6)




### how to read excel with map_def several files 

# tbl <-
  list.files(path = "INPUT/", pattern = "*.xlsx") %>% 
  map_df(~read_excel(path="INPUT/", 
                     sheet = "tabla-0", skip = 6))

