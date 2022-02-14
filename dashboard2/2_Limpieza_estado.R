#Mayusculas a minusculas

datos$estado <- tolower(datos$estado)


#Unicos

unicos_e <- unique(datos$estado)
unicos_e<-as.data.frame(unicos_e)

#Para crear un archivo de Excel solo con los datos únicos de países
write.xlsx(unicos,file="unicos.xlsx", sheetName="unicos" )

#Mayusculas a minusculas

datos$ciudad <- tolower(datos$ciudad)


#Unicos

unicos_e <- unique(datos$estado)
unicos_e<-as.data.frame(unicos_e)

#Para crear un archivo de Excel solo con los datos únicos de países
write.xlsx(unicos,file="unicos.xlsx", sheetName="unicos" )