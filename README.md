# Cambio climático y enfermedades cardiovasculares
Repositorio creado y desarrollado por los alumnos del grupo A de la asignatura "Fuentes de Datos Biomédicas y Web Semántica" del grado en Ingeniería de la Salud, Universidad de Burgos. 

### Integrantes del grupo
Beatriz Amo Nestares, Víctor Barcina Muñoz, Samuel Lozano Juárez

### Título  del proyecto
Estudio de la posible influencia del cambio climático en la incidencia y mortalidad de enfermedades cardiovasculares.

### Objetivos
 - Estudiar las variables meteorológicas mensuales (temperatura, presión atmosférica e insolación diaria) de todas las provincias españolas desde 2010 hasta 2019, con el fin de determinar qué meses y provincias fueron los más adversos. 
 - Una vez conocidas las provincias con climatología más adversa para cada año del periodo 2010-2019, comprobar si la morbilidad hospitalaria y la mortalidad por ECV fue significativamente mayor en dichas provincias con respecto a las climatológicamente normales.
 - Una vez conocidos qué meses y de qué año tuvieron una climatología más extrema en todo el territorio nacional, comprobar si la mortalidad por ECV fue significaticamente mayor esos meses en comparación con los demás meses.
### Metodología
 - Descargar los archivos en formato .xml/.json que contienen la información meterológica mensual por provincias desde la API de AEMET Open Data.
 - Combinar los archivos correspondientes a cada provincia empleando R, obteniendo 52 dataframes (uno por provincia) que contengan toda la información meteorológica desde 2010 a 2019.
 - Filtrar cada dataframe para que únicamente contenga la información de cada año y mes referente a: temperatura media, temperatura media de las máximas, temperatura media de las mínimas, presión media, presión máxima absoluta, presión mínima absoluta, media de insolación y porcentaje medio mensual de la insolación diaria frente a la insolación teórica.
 - 
 
