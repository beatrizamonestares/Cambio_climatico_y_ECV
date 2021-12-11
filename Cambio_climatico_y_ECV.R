# 
# Autores: Amo Nestares, Beatriz; Barcina Muñoz, Víctor; Lozano Juárez, Samuel
# Organización: Universidad de Burgos (UBU)
# Fecha: 12 - Diciembre - 2021
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
library(Hmisc)

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

# Debido a que las estaciones seleccionadas de Guadalajara, Málaga, Palencia, Soria y Valladolid carecen de la información de determinados años, vamos a incorporar la información ausente de forma manual empleando la información para esos años de otras estaciones de esas mismas provincias.
# Además en Guadalajara vamos a sustituir la información del año 2011 de la estación 3168D por la de 3168C.
GuadalajaraMeteo <- GuadalajaraMeteo[14:nrow(GuadalajaraMeteo), ] # de esta manera logramos eliminar las filas referentes al año 2011
GuadalajaraMeteo <- bind_rows(GuadalajaraMeteo, aemet_monthly_clim(station = "3168C", year = 2010), aemet_monthly_clim(station = "3168C", year = 2011))

# Vamos a reordenar las filas por fecha, para ello primero debemos añadir un "0" en los meses del 1 al 9, para que no tome como mayor el mes 2 que el 12 (vamos a realizarlo mediante programación funcional para reciclar código y no copiar y pegar).
# Declaramos la función Reorder para poder emplearla con el resto de provincias

Reorder <- function(provincia){
  #recorre todos los elementos de la columna fecha de la provincia pasada como parámetro
  for (i in 1:length(provincia$fecha)){
    #comprueba si el tamaño del elemento actual de la columna fecha tiene menos de 7 caracteres
    if(nchar(provincia$fecha[i])<7){
      #en caso de que así sea, corta la cadena donde debiera ir el 0, lo introduce y pega la parte de después del 0
      provincia$fecha[i] <- paste(str_sub(provincia$fecha[i], start = 1L, end = 5L), "0", str_sub(provincia$fecha[i], start = -1L, end=-1L), sep = "")
    }
  }
  #por último devuelve la provincia pasada, pero ahora ordenada por fecha (donde ya sí que se puede ordenar correctamente por fecha)
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


#  * Datos poblacionales --------------------------------------------------

# Cargamos un excel que contiene información acerca del número de habitantes de cada provincia

DFPoblacion <- read_excel("C:/Users/samue/Documents/UBU/3er curso/FDB y Web Semántica/Cambio_climatico_y_ECV/INPUT/Poblacion_prov.xlsx", 
                          skip = 6)

# * Datos de morbilidad ---------------------------------------------------

#La carga de datos de morbilidad se lleva a cabo mediante un bucle for. Leemos todos los archivos de la carpeta INPUT que empiecen con 'morbilidad' y vamos pegando su contenido uno debajo de otro. Es importante indicar el tipo de columnas, para que nos introduzca las numéricas como tal y no como carácter.
DFMorbilidad <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="morbilidad")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6, col_types = c("text",rep("numeric",64))) 
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

# La carda de datos de mortalidad se realiza exactamente igual a los datos de morbilidad, solo que en vez de leer todos los archivos que empiecen por 'morbilidad', le pasamos como criterio que los archivos empiecen por 'Mort_nacion'.
DFMort_Mens <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="Mort_nacion")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMort_Mens <- bind_rows(DFMort_Mens, temporal)
}

# * Datos de mortalidad provincial ----------------------------------------

# Al igual que en los dos casos anteriores empleamos el bucle for y leemos y añadimos uno a uno los ficheros que empiecen por 'mort_prov'
DFMort_Prov <- data.frame()
for (i in dir_ls(path = "INPUT", regexp="mort_prov")){
  temporal <- read_excel(path = i, sheet = "tabla-0", skip = 6) 
  DFMort_Prov <- bind_rows(DFMort_Prov, temporal)
}


# REFINAMIENTO DE LOS DATOS -----------------------------------------------

# * Datos meteorológicos --------------------------------------------------

# Para el refinamiento de datos meterológicos vamos a:
#     - Eliminar las filas referentes a las medias anuales (seleccionar solo la información de los meses)
#     - Eliminar las columnas referentes a variables no deseadaes (solo vamos a registrar tm_max, tm_min, q_max, q_min, hr y p_sol)
#     - Vamos a añadir una columna 'Periodo' con el año y otra columna 'Mes' con el mes del año para cada dato.
#     - Vamos a eliminar la columna `fecha` que viene por defecto en los datos (ya que ahora hemos introducido el formato Perido-Mes)
#     - Vamos a convertir la columna q_max y q_min de character a numeric
# Para realizar todo ello vamos a definir una función 'Mod_meteo' y posteriormente aplicarla a cada uno de los dataframes de las diferentes provincias.

Mod_meteo <- function(prov){
  # Como vamos a emplear la función sobre una lista de nombres (y no con los objetos directamente), debemos emplear la función 'get()' que nos permita obtener el objeto al que referencia ese nombre
  Objeto <- get(prov)
  # Nos quedamos solo con las columnas de interés
  Objeto <- select(Objeto, c(`fecha`,`tm_max`,`tm_min`,`q_max`,`q_min`,`p_sol`,`hr`)) %>% 
    # Eliminamos las filas referentes a valores de medias anuales
    filter(.data = . , !`fecha` %in% c("2010-13","2011-13","2012-13","2013-13","2014-13","2015-13","2016-13","2017-13","2018-13","2019-13")) %>% 
    # Introducimos las columnas de Periodo y de Mes
    bind_cols(Periodo = rep(c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), each=12), Mes = rep(c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), 10)) %>% 
    # Colocamos estas columnas recién creadas como las 2 primeras y eliminamos la columna de fecha empleando el subset y el complementario de fecha (indicado como - `fecha`) 
    relocate(.,c(`Periodo`,`Mes`),.before = `fecha`) %>% 
    subset(select = -`fecha`)
  
  # Recorremos cada elemento de la columna q_max del dataframe respectivo y eliminamos las posiciones que contienen caracteres no numéricos.
  for (i in 1:length(Objeto$q_max)){
    Objeto$q_max[i] <- str_sub(string = Objeto$q_max[i], start = 1L, end = -5L)
    Objeto$q_min[i] <- str_sub(string = Objeto$q_min[i], start = 1L, end = -5L)
  }
  
  # Forzamos la conversión a numeric una vez eliminados los elementos del tipo character
  Objeto$q_max <- as.numeric(Objeto$q_max)
  Objeto$q_min <- as.numeric(Objeto$q_min)
  
  return(Objeto)
}

# Aplicamos esta función 'Mod_meteo' a cada elemento del vector Provincias, unido con la palabra 'Meteo', lo que da lugar al nombre de cada dataframe meteorológico de cada provincia.
for (i in 1:length(Provincias)){
  Nam <- paste(Provincias[i],"Meteo",sep = "")
  assign(Nam,Mod_meteo(Nam))
}


#  * Datos de población ---------------------------------------------------

names(DFPoblacion)[1] <- "Localidad"
DFPoblacion <- select(DFPoblacion, starts_with(c("Localidad","1 de enero"))) %>% 
  select(. , ends_with(c("Localidad","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))) %>%
  filter(!Localidad %in% c("Total","Total Nacional"))
names(DFPoblacion) <- c("Localidad","2010","2011","2012","2013","2014","2015","2016","2017", "2018","2019")


i <- 2
while(i<length(DFPoblacion$Localidad)){
  nombre <- DFPoblacion$Localidad[i]
  DFPoblacion$Localidad[i+1] <- nombre
  i <- i+2
}

DFPoblacion <- drop_na(DFPoblacion)
DFPoblacion <- filter(.data = DFPoblacion, Localidad != "Ambos sexos")

for (i in 1:length(DFPoblacion$Localidad)){
  DFPoblacion$Localidad[i] <- str_sub(DFPoblacion$Localidad[i], start = 4)
}

DFPoblacion$Localidad[which(DFPoblacion$Localidad %in% c("Araba/Álava"))] <- "Alava"
DFPoblacion$Localidad[which(DFPoblacion$Localidad %in% c("Gipuzkoa"))] <- "Guipuzkoa"

DFPoblacion <- arrange(DFPoblacion, Localidad)

DFPoblacion <- mutate(DFPoblacion, "Provincia" = Provincias) %>% 
  relocate(., `Provincia`, .before = `Localidad`) %>% 
  subset(., select = - "Localidad") %>% 
  pivot_longer(., names_to = "Periodo", values_to = "Habitantes", cols = c(`2010`:`2019`))

DFPoblacion <- pivot_longer(DFPoblacion, names_to = "Periodo", values_to = "Habitantes", cols = c(`2010`:`2019`))

# * Datos de morbilidad --------------------------------------------------

# Para el refinamiento de datos de morbilidad vamos a:
#     - Seleccionar solo aquellas filas que contienen información de enfermedades relacionadas con ECV (para ambos sexos)
#     - Eliminar las columnas referentes a las comunidades autónomas (seleccionar solo las que contienen información de las provincias)
#     - Una vez seleccionadas las filas de ECV vamos a eliminar la columna de Causa (ya que ahora solo va a haber 1 causa: ECV)
#     - Añadimos una columna `Periodo` que contiene los años y otra `Sexo` con los sexos para cada dato.
#     - Por último renombramos las columnas empleando los nombres del vector Provincias

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

#Por último vamos a recolocar las columnas periodo y sexo y a cambiar la tabla a formato 'longer' para que se adecúe más a nuestro tipo de datos de meteorología
DFMorbilidad <- relocate(DFMorbilidad,c(`Periodo`,`Sexo`),.before = `Alava`) %>% 
  pivot_longer(data = ., names_to = "Provincia", values_to = "Altas", cols= c(Alava:Zaragoza))

# * Datos de mortalidad nacional mensual -------------------------------------------

# Primero obtenemos aquellas posiciones de las filas que contienen las muertes correspondientes a la causa que nos interesa

posiciones <- which(DFMort_Mens$Causa %in% c("053-061 IX.Enfermedades del sistema circulatorio"))

# Vamos a crear un data frame temporal que va a servirnos de pivote donde cargar temporalmente el DFMort_Mens antes de cargarlo definitivamente

temporal <- data.frame()

# Ahora empleamos las posiciones en que se encuentran los datos de interés y seleccionamos las 3 filas siguientes (que contienen los datos de Ambos Sexos, Hombres y Mujeres en ese orden)
for(i in posiciones){
  temporal <- bind_rows(temporal, DFMort_Mens[(i+1):(i+3), ])
}

# Guardamos ahora sí esta información en el dataframe DFMort_Mens y le realizamos una serie de modificaciones para facilitar su lectura (incorporamos la columna año, la recolocamos y eliminamos la columna de `Todos los meses`)
DFMort_Mens <- mutate(temporal, Periodo = rep(c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), each=3)) %>% 
  mutate(Sexo = Causa) %>% 
  relocate(`Sexo`, .before = `Todos los meses`) %>% 
  relocate(`Periodo`, .before = `Sexo`) %>% 
  subset(select = -c(`Todos los meses`,`Causa`)) %>% 
  # Realizamos también un pivot_longer() para almacenar los datos en formato longer y ponemos en mayúsculas el nombre de los meses para facilitar posteriores join con otras tablas
  pivot_longer(names_to = "Mes", values_to = "Muertes", cols = c(enero:diciembre)) %>% 
  relocate(Mes, .before = `Sexo`) %>% 
  mutate(Mes = capitalize(Mes))


# * Datos de mortalidad provincial ----------------------------------------

# Primero vamos a nombrar la primera columna como CAUSA para poder manejarla de forma más cómoda
names(DFMort_Prov)[1] <-"Causa"
# Al igual que en el refinamiento anterior, vamos a localizar aquellas filas que contienen la información referente a los datos que nos interesan (muertes por ECV)
posiciones <- which(DFMort_Prov$Causa %in% c("053-061 IX.Enfermedades del sistema circulatorio"))

# E igual que antes almacenamos los datos en un dataframe temporal que usaremos como pivote

temporal <- data.frame()

# Ahora empleamos las posiciones en que se encuentran los datos de interés y seleccionamos las 3 filas siguientes (que contienen los datos de Ambos Sexos, Hombres y Mujeres en ese orden)
for(i in posiciones){
  temporal <- bind_rows(temporal, DFMort_Prov[(i+1):(i+3), ])
}

DFMort_Prov <- temporal

DFMort_Prov <- DFMort_Prov[ ,1:(length(DFMort_Prov)-3)] %>% 
  mutate(Sexo = Causa, Periodo = rep(c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), each=3)) %>% 
  relocate(Periodo, .before = Causa) %>% 
  relocate(Sexo, .before = Causa) %>% 
  subset(select = -c(Causa, Total)) %>% 
  relocate(.,`Araba/Álava`, .before = `Albacete`) %>% 
  relocate(., `Gipuzkoa`, .after = `Guadalajara`) %>%
  relocate(., `Ceuta`, .before = `Ciudad Real`) %>% 
  relocate(., Melilla, .before = `Murcia`) %>% 
  pivot_longer(names_to = "temporal", values_to = "Mortalidad", cols = c(`Araba/Álava`:`Zaragoza`)) %>% 
  mutate(., `Provincia` = rep(Provincias, 30)) %>% 
  relocate(Provincia, .before = temporal) %>% 
  subset(select = -temporal)


# ANÁLISIS DE LOS DATOS ---------------------------------------------------

# * Datos meteorológicos --------------------------------------------------

# Una vez ya tenemos modificados nuestro datos meteorológicos comenzamos a comprobar qué meses y en qué provincias fueron los más dispares climatológicamente hablando. Para ello vamos a seguir los siguientes pases:
#     - Para cada dataframe de meteorología de cada provincia, calcular la media de cada variable meteorológica entre el mismo mes de los diferentes años (obtendremos una media para todos los eneros, otra para todos los febreros, otra para marzos...).
#     - Calcular la anomalía de cada valor meteorológico (calculado como la diferencia entre el valor y la media de dicho valor)
#     - Obtener una gráfica para representar cada una de estas anomalías elevadas al cuadrado para cada provincia.
#     - Con los valores de la gráfica seleccionar aquellos  meses cuya anomalía fue especialmente grande y añadirlos a un dataframe: DFMeteo_Extremos

# Comenzamos definiendo una función 'Generador_Av_Anom' que calcula la media y anomalía AL CUADRADO para las variables meteorológicas del dataframe pasado como parámetro.
Generador_Av_Anom <- function(provincia){
  # Para cada mes, calcula la media a lo largo de los años y la añade en una columna `Av_nombre_variable`.
  Interna <- left_join(x = provincia, y = provincia %>% 
                         group_by(Mes) %>% 
                         summarise(Av_tmax = mean(tm_max, na.rm = TRUE),Av_tmin = mean(tm_min, na.rm = TRUE),Av_qmax = mean(q_max, na.rm = TRUE),Av_qmin = mean(q_min, na.rm = TRUE),Av_psol = mean(p_sol, na.rm = TRUE),Av_hr = mean(hr, na.rm = TRUE))
                       , by = "Mes")
  
  # A continuación vamos a sustituir los valores NA que encontremos en las columnas hr y p_sol por el valor de la media calculada para ese mes
  for (i in 1:length(Interna$hr)){
    if (is.na(Interna$hr[i])){
      Interna$hr[i] <- Interna$Av_hr[i]
    }
    if (is.na(Interna$p_sol[i])){
      Interna$p_sol[i] <- Interna$Av_psol[i]
    }
  }
    # Una vez creada esta nueva columna, resta el valor mensual particular y esa nueva columna, generando así la anomalía y almacenándola en la columna `Anom_nombre_variable`
  Interna <-mutate(.data = Interna, Anom_tmax = (`tm_max`-`Av_tmax`)**2, Anom_tmin = (`tm_min`-`Av_tmin`)**2, Anom_qmax = (`q_max`-`Av_qmax`)**2, Anom_qmin = (`q_min`-`Av_qmin`)**2, Anom_psol = (`p_sol`-`Av_psol`)**2, Anom_hr = (`hr`-`Av_hr`)**2)
  return(Interna)
}

# Aplicamos la función que acabamos de definir para cada dataframe de meteorología de las provincias.
for (i in Provincias){
  Nam <- paste(i,"Meteo",sep = "")
  assign(Nam, Generador_Av_Anom(get(Nam)))
}

# Para el filtrado de aquellos meses y provincias que tienen valores extremos (con anomalías muy grandes) vamos a definir 2 funciones: una primera 'Graf_filt_Meteo()' que va a generar las gráficas para las anomalías de tm_max, tm_min por un ladom q_max, q_min por otro y hr y p_sol en una tercera gráfica y va a guardar esas gráficas; y una segunda función que va a seleccionar a partir de los datos de las gráficas aquellos meses de cada provincia cuya anomalía se sale del Intervalo de Confianza para esa anomalía.


Graf_filt_Meteo <- function(objeto){
  # Creamos la gráfica que representa las anomalías al cuadrado de tm_max y tm_min. Vamos a generarla por capas para poder juntar distintos estilos de representación.
  provincia <- get(objeto)
  colors <- c("Anom_tmax" = "red", "Anom_tmin" = "blue", )
  
  graf_temp <- ggplot(provincia , aes(x = paste(Periodo, Mes, sep = " "))) + 
    geom_line(aes(y = Anom_tmax, group = 1), colour = "Anom_tmax") + 
    geom_point(size = 1.5, aes(y = Anom_tmax), colour = "red") + 
    geom_smooth(aes( y = Anom_tmax, group = 1), fill = "Anom_tmax", alpha = 0.25, colour = "Anom_tmax", level = 0.995) +
    geom_line(aes(y = Anom_tmin,  group = 1), colour = "Anom_tmin") + 
    geom_point(size = 1.5, aes(y = Anom_tmin), colour = "blue") + 
    geom_smooth(aes(y = Anom_tmin, group = 1), fill = "Anom_tmin", alpha = 0.25, colour = "Anom_tmin", level = 0.995) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Periodo Mes", y = expression("Anomalía"~^2), colour = "Serie") + 
    scale_color_manual(values = colors) +
    # A continuación se emplea la función 'deparse()' junto con 'substitute()' que nos permite obtener en forma de character el nombre del objeto pasado como parámetro.
    ggtitle(label = "Anomalía Temperatura", subtitle = str_sub(objeto, start = 1L, end = -6L))

  # Creamos la gráfica que representa las anomalías al cuadrado de q_max y q_min. Vamos a generarla por capas para poder juntar distintos estilos de representación.
  graf_presion <- ggplot(provincia , aes(x = paste(Periodo, Mes, sep = " "))) + 
    geom_line(aes(y = Anom_qmax, group = 1), colour = "red") + 
    geom_point(size = 1.5, aes(y = Anom_qmax), colour = "red") + 
    geom_smooth(aes( y = Anom_qmax, group = 1), fill = "red", alpha = 0.25, colour = "red", level = 0.995) +
    geom_line(aes(y = Anom_qmin,  group = 1), colour = "blue") + 
    geom_point(size = 1.5, aes(y = Anom_qmin), colour = "blue") + 
    geom_smooth(aes(y = Anom_qmin, group = 1), fill = "blue", alpha = 0.25, colour = "blue", level = 0.995) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
    labs(x = "Periodo Mes",
         y = "Anomalía^2") + 
    ggtitle(label = "Anomalía Presión Atm", subtitle = str_sub(objeto, start = 1L, end = -6L))
  
  # Por último creamos la gráfica que representa las anomalías al cuadrado de p_sol y hr. Vamos a generarla por capas para poder juntar distintos estilos de representación.
  graf_sol_hr <- ggplot(provincia , aes(x = paste(Periodo, Mes, sep = " "))) + 
    geom_line(aes(y = Anom_psol, group = 1), colour = "red") + 
    geom_point(size = 1.5, aes(y = Anom_psol), colour = "red") + 
    geom_smooth(aes( y = Anom_psol, group = 1), fill = "red", alpha = 0.25, colour = "red", level = 0.995) +
    geom_line(aes(y = Anom_hr,  group = 1), colour = "blue") + 
    geom_point(size = 1.5, aes(y = Anom_hr), colour = "blue") + 
    geom_smooth(aes(y = Anom_hr, group = 1), fill = "blue", alpha = 0.25, colour = "blue", level = 0.995) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+ 
    labs(x = "Periodo Mes",
         y = "Anomalía^2") + 
    ggtitle(label = "Anomalía Insolación y Humedad", subtitle = str_sub(objeto, start = 1L, end = -6L))
  
  # A cotinuación guardamos cada una de las gráficas generadas en la carpeta /OUTPUT bajo el nombre de la provincia seguido del nombre de la variable meteorológica de la cual se almacenan las anomalías.
  
  ggsave(
    filename = paste(str_sub(objeto, start = 1L, end = -6L), "Temperatura.png", sep = "_"),
    plot = graf_temp ,
    path = paste(getwd(), "/OUTPUT", sep = ""),
    scale = 1,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 320
  )
  
  ggsave(
    filename = paste(str_sub(objeto, start = 1L, end = -6L), "Presion.png", sep = "_"),
    plot = graf_presion ,
    path = paste(getwd(), "/OUTPUT", sep = ""),
    scale = 1,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 320
  )
  
  ggsave(
    filename = paste(str_sub(objeto, start = 1L, end = -6L), "Sol_Humedad.png", sep = "_"),
    plot = graf_sol_hr ,
    path = paste(getwd(), "/OUTPUT", sep = ""),
    scale = 1,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 320
  )
  
  return(list(graf_temp,graf_presion,graf_sol_hr))
  
}

Filtro_Extremos <- function(provincia, dataframe){
  
  name <- str_sub(deparse(substitute(provincia)))
  lista_graficas <- Graf_filt_Meteo(name)
  
  IC_tmax <- left_join(ggplot_build(lista_graficas[[1]])$data[[2]] %>% 
                         select(. , c(x)),
                       ggplot_build(lista_graficas[[1]])$data[[3]] %>% 
                         select(. , c(x, ymax)) %>% 
                         rename(high_IC_tmax = ymax), 
                       by = "x")
  
  IC_tmin <- left_join(ggplot_build(lista_graficas[[1]])$data[[5]] %>% 
                         select(. , c(x)),
                       ggplot_build(lista_graficas[[1]])$data[[6]] %>% 
                         select(. , c(x, ymax)) %>% 
                         rename(high_IC_tmin = ymax), 
                       by = "x")
  
  IC_qmax <- left_join(ggplot_build(lista_graficas[[2]])$data[[2]] %>% 
                         select(. , c(x)),
                       ggplot_build(lista_graficas[[2]])$data[[3]] %>% 
                         select(. , c(x, ymax)) %>% 
                         rename(high_IC_qmax = ymax), 
                       by = "x")
  
  IC_qmin <- left_join(ggplot_build(lista_graficas[[2]])$data[[5]] %>% 
                         select(. , c(x)),
                       ggplot_build(lista_graficas[[2]])$data[[6]] %>% 
                         select(. , c(x, ymax)) %>% 
                         rename(high_IC_qmin = ymax), 
                       by = "x")
  
  IC_psol <- left_join(ggplot_build(lista_graficas[[3]])$data[[2]] %>% 
                         select(. , c(x)),
                       ggplot_build(x[[3]])$data[[3]] %>% 
                         select(. , c(x, ymax)) %>% 
                         rename(high_IC_psol = ymax), 
                       by = "x")
  
  IC_hr <- left_join(ggplot_build(lista_graficas[[3]])$data[[5]] %>% 
                       select(. , c(x)),
                     ggplot_build(lista_graficas[[3]])$data[[6]] %>% 
                       select(. , c(x, ymax)) %>% 
                       rename(high_IC_hr = ymax), 
                     by = "x")
  
  
  Interna <- bind_cols(provincia, IC_tmax) %>% 
    left_join(.,IC_tmin, by = "x") %>% 
    left_join(.,IC_qmax, by = "x") %>% 
    left_join(.,IC_qmin, by = "x") %>% 
    left_join(.,IC_psol, by = "x") %>% 
    left_join(.,IC_hr, by = "x") %>% 
    subset(select = -x) %>% 
    mutate(Provincia = str_sub(name, start = 1L, end = -6L)) %>% 
    relocate(., Provincia, .before = Periodo)
  
  dataframe <- bind_rows(dataframe, filter(Interna,
    Anom_tmax>high_IC_tmax
  )) %>% 
    bind_rows(. , filter(Interna,
    Anom_tmin>high_IC_tmin
    )) %>% 
    bind_rows(. , filter(Interna,
    Anom_qmax>high_IC_qmax
    )) %>% 
    bind_rows(. , filter(Interna,
    Anom_qmin>high_IC_qmin
    )) %>% 
    bind_rows(. , filter(Interna,
    Anom_psol>high_IC_psol
    )) %>% 
    bind_rows(. , filter(Interna,
    Anom_hr>high_IC_hr
    ))
  
  return(dataframe)
  
}

DFMeteo_Extrem <- data.frame()
DFMeteo_Extrem <- Filtro_Extremos(A_CorunaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(AlavaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(AlbaceteMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(AlicanteMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(AlmeriaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(AsturiasMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(AvilaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(BadajozMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(BarcelonaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(BizkaiaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(BurgosMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(CaceresMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(CadizMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(CantabriaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(CastellonMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(CeutaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(Ciudad_RealMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(CordobaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(CuencaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(GironaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(GranadaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(GuadalajaraMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(GuipuzkoaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(HuelvaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(HuescaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(Islas_BalearesMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(JaenMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(La_RiojaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(Las_PalmasMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(LeonMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(LleidaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(LugoMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(MadridMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(MalagaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(MelillaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(MurciaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(NavarraMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(OurenseMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(PalenciaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(PontevedraMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(SalamancaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(SegoviaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(SevillaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(SoriaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(TarragonaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(TenerifeMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(TeruelMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(ToledoMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(ValenciaMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(ValladolidMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(ZamoraMeteo, DFMeteo_Extrem)
DFMeteo_Extrem <- Filtro_Extremos(ZaragozaMeteo, DFMeteo_Extrem)


DFMorbilidad$Periodo <- as.character(DFMorbilidad$Periodo)
DFMort_Prov$Periodo <- as.character(DFMort_Prov$Periodo)
DFMeteo_Extrem$Periodo <- as.character(DFMeteo_Extrem$Periodo)
DFMort_Mens$Periodo <- as.character(DFMort_Mens$Periodo)

DFMorbilidad <- left_join(DFMorbilidad, DFPoblacion, by = c("Provincia", "Periodo"))
DFMort_Prov <- left_join(DFMort_Prov, DFPoblacion, by = c("Provincia", "Periodo"))


Grafica_Meteo_Morbi_AS <- DFMeteo_Extrem %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMorbilidad, by = c("Periodo", "Provincia")) %>% 
  filter(., `Sexo` == "Ambos sexos")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Altas/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Morbilidad Nacional por número de sucesos meteorológicos extremos - Ambos Sexos", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Morbilidad anual")

Grafica_Meteo_Morbi_Hombres <- DFMeteo_Extrem %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMorbilidad, by = c("Periodo", "Provincia")) %>% 
  filter(., `Sexo` == "Hombres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Altas*2/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Morbilidad Nacional por número de sucesos meteorológicos extremos - Hombres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Morbilidad anual")

Grafica_Meteo_Morbi_Mujeres <- DFMeteo_Extrem %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMorbilidad, by = c("Periodo", "Provincia")) %>% 
  filter(., `Sexo` == "Mujeres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Altas*2/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) + 
  ggtitle(label = "Morbilidad Nacional por número de sucesos meteorológicos extremos - Mujeres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Morbilidad anual")

ggsave(
  filename = "Morbilidad frente a sucesos extremos Ambos Sexos.png",
  plot = Grafica_Meteo_Morbi_AS,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad frente a sucesos extremos Hombres.png",
  plot = Grafica_Meteo_Morbi_Hombres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad frente a sucesos extremos Mujeres.png",
  plot = Grafica_Meteo_Morbi_Mujeres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

Grafica_Meteo_MortPro_AS <- DFMeteo_Extrem %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia","Periodo")) %>% 
  filter(., `Sexo` == "Ambos sexos")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Mortalidad/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Mortalidad Nacional por número de sucesos meteorológicos extremos - Ambos Sexos", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Mortalidad anual")

Grafica_Meteo_MortPro_Hombres <- DFMeteo_Extrem %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia","Periodo")) %>% 
  filter(., `Sexo` == "Hombres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Mortalidad/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Mortalidad Nacional por número de sucesos meteorológicos extremos - Hombres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Mortalidad anual")

Grafica_Meteo_MortPro_Mujeres <- DFMeteo_Extrem %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia","Periodo")) %>% 
  filter(., `Sexo` == "Mujeres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Mortalidad/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Mortalidad por número de sucesos meteorológicos extremos - Mujeres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Mortalidad anual")

ggsave(
  filename = "Mortalidad frente a sucesos extremos Ambos Sexos (Prov).png",
  plot = Grafica_Meteo_MortPro_AS,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad frente a sucesos extremos Hombres (Prov).png",
  plot = Grafica_Meteo_MortPro_Hombres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad frente a sucesos extremos Mujeres (Prov).png",
  plot = Grafica_Meteo_MortPro_Mujeres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

Grafica_Meteo_MortNac_AS <- DFMeteo_Extrem %>% 
  group_by(Periodo, Mes) %>% 
  summarise(Sucesos = length(Mes)) %>% 
  left_join(., DFMort_Mens, by = c("Periodo", "Mes")) %>% 
  filter(., `Sexo` == "Ambos sexos") %>% 
  ggplot(., aes(x = reorder(paste(Periodo, Mes, sep = " "), +Sucesos))) + 
  geom_bar(stat = "identity", aes(y = Sucesos, fill = Periodo), color = "black") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  scale_fill_manual(values=c("grey99", "grey85", "grey75", "grey65","grey55", "grey45", "grey35","grey25","grey15","grey5")) + 
  geom_point(aes(y = (Muertes/100)), color = "red", size = 1.5) + 
  stat_smooth(aes(y = (Muertes/100), group = 1), color = "red") + 
  ggtitle(label = "Mortalidad Nacional por mes y número de sucesos meteorológicos extremos - Ambos Sexos") + 
  labs(x = "Mes y Año", y = "Cantidad de sucesos // Muertes nacionales/100")
  
Grafica_Meteo_MortNac_Hombres <- DFMeteo_Extrem %>% 
  group_by(Periodo, Mes) %>% 
  summarise(Sucesos = length(Mes)) %>% 
  left_join(., DFMort_Mens, by = c("Periodo", "Mes")) %>% 
  filter(., `Sexo` == "Hombres") %>% 
  ggplot(., aes(x = reorder(paste(Periodo, Mes, sep = " "), +Sucesos))) + 
  geom_bar(stat = "identity", aes(y = Sucesos, fill = Periodo), color = "black") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  scale_fill_manual(values=c("grey99", "grey85", "grey75", "grey65","grey55", "grey45", "grey35","grey25","grey15","grey5")) + 
  geom_point(aes(y = (Muertes/50)), color = "red", size = 1.5) + 
  stat_smooth(aes(y = (Muertes/50), group = 1), color = "red") + 
  ggtitle(label = "Mortalidad Nacional por mes y número de sucesos meteorológicos extremos - Hombres") + 
  labs(x = "Mes y Año", y = "Cantidad de sucesos // Muertes nacionales/50")

Grafica_Meteo_MortNac_Mujeres <- DFMeteo_Extrem %>% 
  group_by(Periodo, Mes) %>% 
  summarise(Sucesos = length(Mes)) %>% 
  left_join(., DFMort_Mens, by = c("Periodo", "Mes")) %>% 
  filter(., `Sexo` == "Mujeres") %>% 
  ggplot(., aes(x = reorder(paste(Periodo, Mes, sep = " "), +Sucesos))) + 
  geom_bar(stat = "identity", aes(y = Sucesos, fill = Periodo), color = "black") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  scale_fill_manual(values=c("grey99", "grey85", "grey75", "grey65","grey55", "grey45", "grey35","grey25","grey15","grey5")) +
  geom_point(aes(y = (Muertes/50)), color = "red", size = 1.5) + 
  stat_smooth(aes(y = (Muertes/50), group = 1), color = "red") + 
  ggtitle(label = "Mortalidad Nacional por mes y número de sucesos meteorológicos extremos - Mujeres") + 
  labs(x = "Mes y Año", y = "Cantidad de sucesos // Muertes nacionales/50")

ggsave(
  filename = "Mortalidad por mes frente a sucesos meteorológicos extremos - Ambos Sexos.png",
  plot = Grafica_Meteo_MortNac_AS,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad por mes frente a sucesos meteorológicos extremos - Hombres.png",
  plot = Grafica_Meteo_MortNac_Hombres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad por mes frente a sucesos meteorológicos extremos - Mujeres.png",
  plot = Grafica_Meteo_MortNac_Mujeres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)



# ANÁLISIS EXTRA DE LOS DATOS ---------------------------------------------

# Hemos realizado este estudio basándonos en la hipótesis de que los valores meteorológicos extremos (aquellos que se salían de lo normal)
# podían estar relacionados con las enfermedades cardiovasculares, a continuación vamos a formular una segunda hipótesis. Ya no nos vamos a
# basar en valores extremos, sino en valores de riesgo. Para ello primero tenemos que definir qué valores de riesgo existen para las
# variables meteorológicas medidas:
# - Temperatura media de las máximas: aquellos valores más altos son de mayor riesgo
# - Temperatura media de las mínimas: aquellos valores más bajos son de mayor riesgo
# - Presión media de las máximas: aquellos valores más altos son de mayor riesgo
# - Presión media de las mínimas: aquellos valores más altos son de mayor riesgo
# - Humedad relativa: valores más altos son de mayor riesgo
# - Insolación diaria: valores más altos son de mayor riesgo

# Ahora vamos a obtener un dataframe que contenga los 100 valores de mayor riesgo para cada variable.

DFMeteo_Risk <- data.frame()
for(i in 1:length(Provincias)){
  Nam <- paste(Provincias[i], "Meteo", sep = "")
  Obj <- get(Nam)
  tmax <- slice_max(Obj, order_by = `tm_max`, n = 20) %>% 
    mutate(Provincia = Provincias[i])
  tmin <- slice_min(Obj, order_by = `tm_min`, n = 20) %>% 
    mutate(Provincia = Provincias[i])
  qmax <- slice_max(Obj, order_by = `q_max`, n = 20) %>% 
    mutate(Provincia = Provincias[i])
  qmin <- slice_max(Obj, order_by = `q_max`, n = 20) %>% 
    mutate(Provincia = Provincias[i])
  hr <- slice_max(Obj, order_by = `hr`, n = 20) %>% 
    mutate(Provincia = Provincias[i])
  psol <- slice_max(Obj, order_by = `p_sol`, n = 20) %>% 
    mutate(Provincia = Provincias[i])
  DFMeteo_Risk <- bind_rows(DFMeteo_Risk, tmax, tmin, qmax, qmin, hr, psol)
}

DFMeteo_Risk$Periodo <- as.character(DFMeteo_Risk$Periodo)

Grafica_Risk_Morbi_AS <- DFMeteo_Risk %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMorbilidad, by = c("Periodo", "Provincia")) %>% 
  filter(., `Sexo` == "Ambos sexos")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Altas/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Morbilidad Nacional por número de sucesos meteorológicos de riesgo - Ambos Sexos", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Morbilidad anual")

Grafica_Risk_Morbi_Hombres <- DFMeteo_Risk %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMorbilidad, by = c("Periodo", "Provincia")) %>% 
  filter(., `Sexo` == "Hombres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Altas*2/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Morbilidad Nacional por número de sucesos meteorológicos de riesgo - Hombres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Morbilidad anual")

Grafica_Risk_Morbi_Mujeres <- DFMeteo_Risk %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMorbilidad, by = c("Periodo", "Provincia")) %>% 
  filter(., `Sexo` == "Mujeres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Altas*2/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Morbilidad Nacional por número de sucesos meteorológicos de riesgo - Mujeres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Morbilidad anual")

ggsave(
  filename = "Morbilidad por mes frente a sucesos meteorológicos de riesgo - Ambos Sexos.png",
  plot = Grafica_Risk_Morbi_AS,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad por mes frente a sucesos meteorológicos de riesgo - Hombres.png",
  plot = Grafica_Risk_Morbi_Hombres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad por mes frente a sucesos meteorológicos de riesgo - Mujeres.png",
  plot = Grafica_Risk_Morbi_Mujeres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

Grafica_Risk_MortPro_AS <- DFMeteo_Risk %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia","Periodo")) %>% 
  filter(., `Sexo` == "Ambos sexos")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Mortalidad/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Mortalidad Nacional por número de sucesos meteorológicos de riesgo - Ambos Sexos", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Mortalidad anual")

Grafica_Risk_MortPro_Hombres <- DFMeteo_Risk %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia","Periodo")) %>% 
  filter(., `Sexo` == "Hombres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Mortalidad*2/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Mortalidad Nacional por número de sucesos meteorológicos de riesgo - Hombres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Mortalidad anual")

Grafica_Risk_MortPro_Mujeres <- DFMeteo_Risk %>% 
  group_by(Provincia,Periodo) %>% 
  summarise(Sucesos = length(Periodo)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia","Periodo")) %>% 
  filter(., `Sexo` == "Mujeres")%>% 
  ggplot(data = ., aes(x = factor(Sucesos), y = (Mortalidad*2/Habitantes)*100, fill = factor(Sucesos))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) +
  ggtitle(label = "Mortalidad Nacional por número de sucesos meteorológicos de riesgo - Mujeres", subtitle = "Calculado anual y provincialmente") + 
  labs(x = "Nº Sucesos meteorológicos adversos/año", y = "% de Mortalidad anual")

ggsave(
  filename = "Mortalidad anual frente a sucesos meteorológicos de riesgo - Ambos Sexo.png",
  plot = Grafica_Risk_MortPro_AS,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad anual frente a sucesos meteorológicos de riesgo - Hombres.png",
  plot = Grafica_Risk_MortPro_Hombres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad anual frente a sucesos meteorológicos de riesgo - Mujeres.png",
  plot = Grafica_Risk_MortPro_Mujeres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

Grafica_Risk_MortNac_AS <- DFMeteo_Risk %>% 
  group_by(Periodo, Mes) %>% 
  summarise(Sucesos = length(Mes)) %>% 
  left_join(., DFMort_Mens, by = c("Periodo", "Mes")) %>% 
  filter(., `Sexo` == "Ambos sexos") %>% 
  ggplot(., aes(x = reorder(paste(Periodo, Mes, sep = " "), +Sucesos))) + 
  geom_bar(stat = "identity", aes(y = Sucesos, fill = Periodo), color = "black") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  scale_fill_manual(values=c("grey99", "grey85", "grey75", "grey65","grey55", "grey45", "grey35","grey25","grey15","grey5")) + 
  geom_point(aes(y = (Muertes/100)), color = "red", size = 1.5) + 
  stat_smooth(aes(y = (Muertes/100), group = 1), color = "red") + 
  ggtitle(label = "Mortalidad Nacional por mes y número de sucesos meteorológicos de riesgo - Ambos Sexos") + 
  labs(x = "Mes y Año", y = "Cantidad de sucesos // Muertes nacionales/100")

Grafica_Risk_MortNac_Hombres <- DFMeteo_Risk %>% 
  group_by(Periodo, Mes) %>% 
  summarise(Sucesos = length(Mes)) %>% 
  left_join(., DFMort_Mens, by = c("Periodo", "Mes")) %>% 
  filter(., `Sexo` == "Hombres") %>% 
  ggplot(., aes(x = reorder(paste(Periodo, Mes, sep = " "), +Sucesos))) + 
  geom_bar(stat = "identity", aes(y = Sucesos, fill = Periodo), color = "black") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  scale_fill_manual(values=c("grey99", "grey85", "grey75", "grey65","grey55", "grey45", "grey35","grey25","grey15","grey5")) + 
  geom_point(aes(y = (Muertes/50)), color = "red", size = 1.5) + 
  stat_smooth(aes(y = (Muertes/50), group = 1), color = "red") + 
  ggtitle(label = "Mortalidad Nacional por mes y número de sucesos meteorológicos de riesgo - Hombres") + 
  labs(x = "Mes y Año", y = "Cantidad de sucesos // Muertes nacionales/50")

Grafica_Risk_MortNac_Mujeres <- DFMeteo_Risk %>% 
  group_by(Periodo, Mes) %>% 
  summarise(Sucesos = length(Mes)) %>% 
  left_join(., DFMort_Mens, by = c("Periodo", "Mes")) %>% 
  filter(., `Sexo` == "Mujeres") %>% 
  ggplot(., aes(x = reorder(paste(Periodo, Mes, sep = " "), +Sucesos))) + 
  geom_bar(stat = "identity", aes(y = Sucesos, fill = Periodo), color = "black") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  scale_fill_manual(values=c("grey99", "grey85", "grey75", "grey65","grey55", "grey45", "grey35","grey25","grey15","grey5")) + 
  geom_point(aes(y = (Muertes/50)), color = "red", size = 1.5) + 
  stat_smooth(aes(y = (Muertes/50), group = 1), color = "red") + 
  ggtitle(label = "Mortalidad Nacional por mes y número de sucesos meteorológicos de riesgo - Mujeres") + 
  labs(x = "Mes y Año", y = "Cantidad de sucesos // Muertes nacionales/50")

ggsave(
  filename = "Mortalidad anual frente a sucesos meteorológicos de riesgo (Mensual)- Ambos Sexos.png",
  plot = Grafica_Risk_MortNac_AS,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad anual frente a sucesos meteorológicos de riesgo (Mensual)- Hombres.png",
  plot = Grafica_Risk_MortNac_Hombres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad anual frente a sucesos meteorológicos de riesgo (Mensual)- Mujeres.png",
  plot = Grafica_Risk_MortNac_Mujeres,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

#Graficación de la relación entre las variables meteorológicas de riesgo con la morbilidad

Grafica_tmax_Morbilidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(tmax = mean(`tm_max`, na.rm = TRUE)) %>% 
  left_join(., DFMorbilidad, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = tmax, y = Altas/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth() + 
  ggtitle(label = "Morbilidad nacional respecto a la temperatura media de las máximas")

Grafica_tmin_Morbilidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(tmin = mean(`tm_min`, na.rm = TRUE)) %>% 
  left_join(., DFMorbilidad, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = tmin, y = Altas/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth() +
  ggtitle(label = "Morbilidad nacional respecto a la temperatura media de las mínimas")

Grafica_qmax_Morbilidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(qmax = mean(`q_max`, na.rm = TRUE)) %>% 
  left_join(., DFMorbilidad, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = qmax, y = Altas/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth(method = "lm")+
  ggtitle(label = "Morbilidad nacional respecto a la presión media de las máximas")

Grafica_qmin_Morbilidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(qmin = mean(`q_min`, na.rm = TRUE)) %>% 
  left_join(., DFMorbilidad, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = qmin, y = Altas/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth(method = "lm")+
  ggtitle(label = "Morbilidad nacional respecto a la presión media de las mínimas")

Grafica_hr_Morbilidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(hr = mean(`hr`, na.rm = TRUE)) %>% 
  left_join(., DFMorbilidad, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = hr, y = Altas/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Morbilidad nacional respecto a la humedad relativa")

Grafica_psol_Morbilidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(psol = mean(`p_sol`, na.rm = TRUE)) %>% 
  left_join(., DFMorbilidad, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = psol, y = Altas/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Morbilidad nacional respecto a la insolación real / insolación teórica (%)")

#Graficación de la relación entre las variables meteorológicas de riesgo con la mortalidad (obtenida por provincias y año)

Grafica_tmax_Mortalidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(tmax = mean(`tm_max`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = tmax, y = Mortalidad/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la temperatura media de las máximas (calculada por provincias)")

Grafica_tmin_Mortalidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(tmin = mean(`tm_min`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = tmin, y = Mortalidad/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la temperatura media de las mínimas (calculada por provincias)")

Grafica_qmax_Mortalidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(qmax = mean(`q_max`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = qmax, y = Mortalidad/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth(method = "lm")+
  ggtitle(label = "Mortalidad nacional respecto a la presión media de las máximas (calculada por provincias)")

Grafica_qmin_Mortalidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(qmin = mean(`q_min`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = qmin, y = Mortalidad/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth(method = "lm")+
  ggtitle(label = "Mortalidad nacional respecto a la presión media de las mínimas (calculada por provincias)")

Grafica_hr_Mortalidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(hr = mean(`hr`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = hr, y = Mortalidad/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la humedad relativa (calculada por provincias)")

Grafica_psol_Mortalidad <- DFMeteo_Extrem %>% 
  group_by(Provincia, Periodo) %>% 
  summarise(psol = mean(`p_sol`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Prov, by = c("Provincia", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = psol, y = Mortalidad/Habitantes*100)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la insolación real / insolación teórica (%)(calculada por provincias)")

#Graficación de la relación entre las variables meteorológicas de riesgo con la mortalidad (Nacional y mensual)

Grafica_tmax_MortalidadMes <- DFMeteo_Extrem %>% 
  group_by(Mes, Periodo) %>% 
  summarise(tmax = mean(`tm_max`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Mens, by = c("Mes", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = tmax, y = Muertes)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la temperatura media de las máximas (calculada nacional por mes)")

Grafica_tmin_MortalidadMes <- DFMeteo_Extrem %>% 
  group_by(Mes, Periodo) %>% 
  summarise(tmin = mean(`tm_min`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Mens, by = c("Mes", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = tmin, y = Muertes)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la temperatura media de las mínimas (calculada nacional por mes)")

Grafica_qmax_MortalidadMes <- DFMeteo_Extrem %>% 
  group_by(Mes, Periodo) %>% 
  summarise(qmax = mean(`q_max`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Mens, by = c("Mes", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = qmax, y = Muertes)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la presión media de las máximas (calculada nacional por mes)")

Grafica_qmin_MortalidadMes <- DFMeteo_Extrem %>% 
  group_by(Mes, Periodo) %>% 
  summarise(qmin = mean(`q_min`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Mens, by = c("Mes", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = qmin, y = Muertes)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la presión media de las mínimas (calculada nacional por mes)")

Grafica_hr_MortalidadMes <- DFMeteo_Extrem %>% 
  group_by(Mes, Periodo) %>% 
  summarise(hr = mean(`hr`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Mens, by = c("Mes", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = hr, y = Muertes)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la humedad relativa (calculada nacional por mes)")

Grafica_psol_MortalidadMes <- DFMeteo_Extrem %>% 
  group_by(Mes, Periodo) %>% 
  summarise(psol = mean(`p_sol`, na.rm = TRUE)) %>% 
  left_join(., DFMort_Mens, by = c("Mes", "Periodo")) %>% 
  filter(., Sexo == "Ambos sexos")%>% 
  ggplot(., aes(x = psol, y = Muertes)) + 
  geom_point(size = 1.5) + 
  geom_smooth()+
  ggtitle(label = "Mortalidad nacional respecto a la insolación real / insolación teórica (%) (calculada nacional por mes)")

# Por último vamos a guardar las gráficas recién creadas 

# MORBILIDAD

ggsave(
  filename = "Morbilidad nacional frente a temperatura media de las máximas.png",
  plot = Grafica_tmax_Morbilidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad nacional frente a temperatura media de las mínimas.png",
  plot = Grafica_tmin_Morbilidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad nacional frente a presión media de las máximas.png",
  plot = Grafica_qmax_Morbilidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad nacional frente a presión media de las mínimas",
  plot = Grafica_qmin_Morbilidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad nacional frente a humedad relativa.png",
  plot = Grafica_hr_Morbilidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Morbilidad nacional frente a insolación diaria.png",
  plot = Grafica_psol_Morbilidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

# MORTALIDAD CALCULADA MEDIANTE PROVINCIAS

ggsave(
  filename = "Mortalidad nacional frente a temperatura media de las máximas (provincias).png",
  plot = Grafica_tmax_Mortalidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a temperatura media de las mínimas (provincias).png",
  plot = Grafica_tmin_Mortalidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a presión media de las máximas (provincias).png",
  plot = Grafica_qmax_Mortalidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a presión media de las mínimas (provincias).png",
  plot = Grafica_qmin_Mortalidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a humedad relativa (provincias).png",
  plot = Grafica_hr_Mortalidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a insolación diaria (provincias).png",
  plot = Grafica_psol_Mortalidad,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

# MORTALIDAD NACIONAL CALCULADA POR MESES

ggsave(
  filename = "Mortalidad nacional frente a temperatura media de las máximas (mensual).png",
  plot = Grafica_tmax_MortalidadMes,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a temperatura media de las mínimas (mensual).png",
  plot = Grafica_tmin_MortalidadMes,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a presión media de las máximas (mensual).png",
  plot = Grafica_qmax_MortalidadMes,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a presión media de las mínimas (mensual).png",
  plot = Grafica_qmin_MortalidadMes,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a humedad relativa.png",
  plot = Grafica_hr_MortalidadMes,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Mortalidad nacional frente a insolación diaria (mensual).png",
  plot = Grafica_psol_MortalidadMes,
  path = paste(getwd(), "/OUTPUT", sep = ""),
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)