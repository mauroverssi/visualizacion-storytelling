#Salario y compensaciones a pesos Colombianos

datos_l$salario_anual <- as.numeric(datos_l$salario_anual)
datos_l$otros_ingresos <- as.numeric(datos_l$otros_ingresos)
datos_l$salario_anual[is.na(datos_l$salario_anual)]<-0
datos_l$otros_ingresos[is.na(datos_l$otros_ingresos)]<-0

monedas <- unique(datos_l$moneda)

datos_l <- datos_l %>%
                mutate(tipo_cambio=case_when(moneda =="USD"~3939.71
                                             ,moneda =="EUR"~4471.69
                                             ,moneda =="GBP"~5343.23
                                             ,moneda =="CAD"~3094.32
                                             ,moneda =="JPY"~31.13
                                             ,moneda =="SEK"~421.69
                                             ,moneda == "AUD/NZD"~2811.54
                                             ,moneda == "CHF"~4253.90
                                             ,moneda == "ZAR"~258.91
                                             ,moneda == "HKD"~505.06))


datos_l <- datos_l%>%
              mutate(salario_anual_c=salario_anual*tipo_cambio)%>%
              mutate(compensaciones_c=otros_ingresos*tipo_cambio)%>%
              mutate(ingreso_total_c=salario_anual_c+compensaciones_c)

datos_final<-datos_l%>%
                select(-c(direccion,pais,estado,ciudad))%>%
                rename(ciudad=ciudad_l, pais=pais_l, estado=estado_l)

write.csv(datos_final, "datos_tarea2.csv", row.names = FALSE)
