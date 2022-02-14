library(readr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tm)
library(forcats)
library(Hmisc)
library(xlsx)

#Carga y limpieza

datos <- read_delim("G:/Unidades compartidas/Maestría en Inteligencia Analítica de Datos/Cursos/Visualización y Storytelling/Proyectos R/tarea2/Datos/Copia de Ask A Manager Salary Survey 2021 (Responses).csv", 
                    delim = ";", escape_double = FALSE, col_types = cols(...19 = col_skip(), 
                                                                         ...20 = col_skip(), ...21 = col_skip(), 
                                                                         ...22 = col_skip(), ...23 = col_skip(), 
                                                                         ...24 = col_skip(),Timestamp = col_skip()), trim_ws = TRUE)

str(datos)
names(datos)

#Renombrar variables

datos <- rename(datos,edad='How old are you?', industria=Industry, titulo='Job title',
                contexto_adicional='Additional context on job title', salario_anual='Annual salary',
                otros_ingresos='Other monetary comp',moneda=Currency, otra_moneda='Currency - other',
                informacion_adicional_ingresos='Additional context on income', pais=Country,
                estado=State, ciudad=City, anos_experiencia_profesional='Overall years of professional experience',
                anos_experiencia_campo='Years of experience in field', nivel_educativo='Highest level of education completed',
                genero=Gender, raza=Race)

#Todos los datos en minísculas

datos$pais <- tolower(datos$pais)

#Para revisar los datos únicos
unicos <- unique(datos$pais)
unicos<-as.data.frame(unicos)

#Para crear un archivo de Excel solo con los datos únicos de países
write.xlsx(unicos,file="unicos.xlsx", sheetName="unicos" )


#Desconocidos
datos$pais[datos$pais =="contracts"|datos$pais =="we don't get raises, we get quarterly bonuses, but they periodically asses income in the area you work, so i got a raise because a 3rd party assessment showed i was paid too little for the area we were located"
           |datos$pais =="global"|datos$pais =="worldwide (based in us but short term trips aroudn the world)"
           |datos$pais =="currently finance"|datos$pais =="uxz"|datos$pais =="catalonia"
           |datos$pais =="$2,175.84/year is deducted for benefits"|datos$pais =="uxz"|datos$pais =="catalonia"
           |datos$pais =="jersey, channel islands"|datos$pais =="virginia"|datos$pais =="catalonia"
           |datos$pais =="hartford"|datos$pais =="nl"|datos$pais =="i.s."
           |datos$pais =="remote"|datos$pais =="uae"|datos$pais =="bonus based on meeting yearly goals set w/ my supervisor"
           |datos$pais =="is"|datos$pais =="international"|datos$pais =="i earn commission on sales. if i meet quota, i'm guaranteed another 16k min. last year i earned an additional 27k. it's not uncommon for people in my space to earn 100k+ after commission."
           |datos$pais =="i was brought in on this salary to help with the ehr and very quickly was promoted to current position but compensation was not altered."
           |datos$pais =="n/a (remote from wherever i want)"
           |datos$pais =="ua"|datos$pais =="y"|datos$pais =="united y"
           |datos$pais =="europe"|datos$pais =="policy"|datos$pais =="na"
           |datos$pais =="company in germany. i work from pakistan."|datos$pais =="#N/D"|datos$pais =="na"]<-"desconocido"

#japan
datos$pais[grepl("japan", datos$pais)]<-"japan"

#Philippines
datos$pais[grepl("philippines", datos$pais)]<-"philippines"

#Denmark
datos$pais[grepl("danmark", datos$pais)]<-"denmark"

#Romania
datos$pais[grepl("romania", datos$pais)]<-"romania"

#Ivory Coast

datos$pais[grepl("cote d'ivoire", datos$pais)]<-"ivory coast"


#Austria

datos$pais[grepl("austria", datos$pais)]<-"austria"
#Australia

datos$pais[grepl("austra", datos$pais)]<-"australia"

#China

datos$pais[grepl("china", datos$pais)]<-"china"


#italia

datos$pais[grepl("ital", datos$pais)]<-"italy"

#czech republic

datos$pais[grepl("cze", datos$pais)]<-"czech republic"

#New zealand
datos$pais[grepl("new", datos$pais)]<-"new zealand"
datos$pais[grepl("nz", datos$pais)]<-"new zealand"

#Canada
datos$pais[grepl("can", datos$pais)]<-"canada"
datos$pais[grepl("csnada", datos$pais)]<-"canada"

#Hong Kong
datos$pais[grepl("hong", datos$pais)]<-"hong kong"

#Argentina

datos$pais[grepl("argentina", datos$pais)]<-"argentina"


#netherlands

datos$pais[grepl("netherlands", datos$pais)]<-"netherlands"
datos$pais[grepl("nederland", datos$pais)]<-"netherlands"

#United Kingdom


datos$pais[grepl("england", datos$pais)]<-"united kingdom"
datos$pais[grepl("scotland", datos$pais)]<-"united kingdom"
datos$pais[grepl("northern ireland", datos$pais)]<-"united kingdom"
datos$pais[grepl("wales", datos$pais)]<-"united kingdom"

datos$pais[datos$pais =="uk"|datos$pais =="uk, remote"
          |datos$pais =="wales, uk"|datos$pais =="wales (uk)"
          |datos$pais =="uk (northern ireland)"|datos$pais =="scotland, uk"
          |datos$pais =="scotland, uk"|datos$pais =="uk, but for globally fully remote company"
          |datos$pais =="great britain"|datos$pais =="u.k."
          |datos$pais =="u.k"|datos$pais =="englang"
          |datos$pais =="united kingdom."|datos$pais =="britain"
          |datos$pais =="united kindom"|datos$pais =="united kingdomk"
          |datos$pais =="unites kingdom"|datos$pais =="united kingdomk"]<- "united kingdom"




#United States

datos$pais[grepl("united st", datos$pais)]<-"united states"
datos$pais[grepl("america", datos$pais)]<-"united states"
datos$pais[grepl("u\\.s", datos$pais)]<-"united states"

datos$pais[datos$pais =="us"|datos$pais =="usa"|datos$pais =="u.s."|datos$pais =="united states of america"
           |datos$pais =="usa-- virgin islands"|datos$pais =="usa (company is based in a us territory, i work remote)"
           |datos$pais =="usab"|datos$pais =="usa tomorrow"|datos$pais =="usat"|datos$pais =="usaa"
           |datos$pais =="usa, but for foreign gov't"|datos$pais =="canada and usa"
           |datos$pais =="united states (i work from home and my clients are all over the us/canada/pr"
           |datos$pais =="i am located in canada but i work for a company in the us"
           |datos$pais =="us of a"|datos$pais =="united status"|datos$pais =="uss"
           |datos$pais =="the us"|datos$pais =="us govt employee overseas, country withheld"
           |datos$pais =="usd"|datos$pais =="i work for a uae-based organization, though i am personally in the us."
           |datos$pais =="united state"|datos$pais =="unitedstates"
           |datos$pais =="united  states"|datos$pais =="united sates"
           |datos$pais =="uniited states"|datos$pais =="uniyed states"
           |datos$pais =="unitied states"|datos$pais =="unitef stated"
           |datos$pais =="uniyes states"|datos$pais =="uniteed states"
           |datos$pais =="unites states"|datos$pais =="unite states"
           |datos$pais =="uniter statez"|datos$pais =="u. s."
           |datos$pais =="u.a."|datos$pais =="unted states"
           |datos$pais =="🇺🇸"|datos$pais =="untied states"
           |datos$pais =="san francisco"|datos$pais =="california"] <- "united states"


#Generar xlsx con lo base limpia
write.xlsx(datos,file="base_tarea2.xlsx", sheetName="base" )

