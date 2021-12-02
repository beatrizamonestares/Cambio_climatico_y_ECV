# 
# Autores: Amo Nestares, Beatriz; Barcina Muñoz, Víctor; Lozano Juárez, Samuel
# Organización: Universidad de Burgos (UBU)
# Fecha: 1 - Diciembre - 2021
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
  pivot_longer(data = ., names_to = "Provincias", values_to = "Altas", cols= c(Alava:Zaragoza))

# * Datos de mortalidad nacional mensual -------------------------------------------



# * Datos de mortalidad provincial ----------------------------------------



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


Graf_filt_Meteo <- function(provincia){
  # Creamos la gráfica que representa las anomalías al cuadrado de tm_max y tm_min. Vamos a generarla por capas para poder juntar distintos estilos de representación.
  graf_temp <- ggplot(provincia , aes(x = paste(Periodo, Mes, sep = " "))) + 
    geom_line(aes(y = Anom_tmax, group = 1), colour = "red",) + 
    geom_point(size = 1.5, aes(y = Anom_tmax), colour = "red") + 
    geom_smooth(aes( y = Anom_tmax, group = 1), fill = "red", alpha = 0.25, colour = "red", level = 0.995) +
    geom_line(aes(y = Anom_tmin,  group = 1), colour = "blue") + 
    geom_point(size = 1.5, aes(y = Anom_tmin), colour = "blue") + 
    geom_smooth(aes(y = Anom_tmin, group = 1), fill = "blue", alpha = 0.25, colour = "blue", level = 0.995) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Periodo Mes",
         y = "Anomalía^2") + 
    # A continuación se emplea la función 'deparse()' junto con 'substitute()' que nos permite obtener en forma de character el nombre del objeto pasado como parámetro.
    ggtitle(label = "Anomalía Temperatura", subtitle = deparse(substitute(provincia)))

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
    ggtitle(label = "Anomalía Presión Atm", subtitle = deparse(substitute(provincia)))
  
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
    ggtitle(label = "Anomalía Insolación y Humedad", subtitle = deparse(substitute(provincia)))
  
  # A cotinuación guardamos cada una de las gráficas generadas en la carpeta /OUTPUT bajo el nombre de la provincia seguido del nombre de la variable meteorológica de la cual se almacenan las anomalías.
  
  ggsave(
    filename = paste(deparse(substitute(provincia)), "Temperatura.png", sep = "_"),
    plot = graf_temp ,
    path = paste(getwd(), "/OUTPUT", sep = ""),
    scale = 1,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 320
  )
  
  ggsave(
    filename = paste(deparse(substitute(provincia)), "Presion.png", sep = "_"),
    plot = graf_presion ,
    path = paste(getwd(), "/OUTPUT", sep = ""),
    scale = 1,
    width = 40,
    height = 20,
    units = "cm",
    dpi = 320
  )
  
  ggsave(
    filename = paste(deparse(substitute(provincia)), "Sol_Humedad.png", sep = "_"),
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
  lista_graficas <- Graf_filt_Meteo(provincia)
  
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
    mutate(Provincia = deparse(substitute(provincia))) %>% 
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

DFPrueba1 <- data.frame()
DFPrueba1 <- Filtro_Extremos(A_CorunaMeteo, DFPrueba1)

DFPrueba1 <- Filtro_Extremos(AlavaMeteo, DFPrueba1)
