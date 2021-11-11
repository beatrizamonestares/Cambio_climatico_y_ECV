library(climaemet)

if(aemet_detect_api_key() == FALSE){
  browseURL("https://opendata.aemet.es/centrodedescargas/altaUsuario?")
}

aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzYW11ZWxsb3phbm9qdWFyZXpAZ21haWwuY29tIiwianRpIjoiZjEzZWM0NDktOTc1Ny00MGNjLTg5MDktZmFhOTZjZmFkMTcxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MzYzNzMwMDAsInVzZXJJZCI6ImYxM2VjNDQ5LTk3NTctNDBjYy04OTA5LWZhYTk2Y2ZhZDE3MSIsInJvbGUiOiIifQ.HSs6bokk9cYquyGSRBOSC6_fxQoK8ZSlRQR64qMtBns", overwrite = TRUE, install = TRUE)

Provincias <- c("AlavaMeteo","AlbaceteMeteo","AlicanteMeteo","AlmeriaMeteo","AsturiasMeteo","AvilaMeteo","BadajozMeteo","Islas_BalearesMeteo","BarcelonaMeteo","BuzkaiaMeteo","BurgosMeteo","CaceresMeteo","CadizMeteo","CantabriaMeteo","CastellonMeteo","CeutaMeteo","Ciudad_RealMeteo","CordobaMeteo","A_CoruñaMeteo","CuencaMeteo","GironaMeteo","GranadaMeteo","GuadalajaraMeteo","GuipuzkoaMeteo","HuelvaMeteo","HuescaMeteo","JaenMeteo","LeonMeteo","LleidaMeteo","LugoMeteo","MadridMeteo","MalagaMeteo","MelillaMeteo","MurciaMeteo","NavarraMeteo","OurenseMeteo","PalenciaMeteo","Las_PalmasMeteo","PontevedraMeteo","La_RiojaMeteo","SalamancaMeteo","TenerifeMeteo","SegoviaMeteo","SevillaMeteo","SoriaMeteo","TarragonaMeteo","TeruelMeteo","ToledoMeteo","ValenciaMeteo","ValladolidMeteo","ZamoraMeteo","ZaragozaMeteo")

Estaciones <- c("9091O","8175","8019","6325O","1212E","2444","4452","B954","0201D","1082","2331","3469A","5960","1109","8500A","5000C","4121","5402","1387E","8096","0367","5530E","3168D","1014A","4642E","9898","5270B","2661B","9771C","1505","3129","6155A","6000A","7178I","9263D","1690A","2374X","C029O","1495","9170","2867","C447A","2465","5783","2030","9981A","9381I","3260B","8416","2422","2614","9434")
