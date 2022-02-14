

datos$direccion <- str_c(datos$ciudad, ", ", datos$estado,", ", datos$pais)

direccion <- datos %>%
                select(direccion)

direccion_unicos <- direccion %>%
  unique()


direccion_limpia_ex <-direccion_unicos[3001:3612,]
direccion_limpia_7<-geocode_google(direccion_limpia_ex, key)



limpia <- direccion_limpia_1 %>%
              rbind(direccion_limpia_2) %>%
              rbind(direccion_limpia_3) %>%
              rbind(direccion_limpia_4) %>%
              rbind(direccion_limpia_5) %>%
              rbind(direccion_limpia_6) %>%
              rbind(direccion_limpia_7) 

write_csv2(limpia,file="limpia.csv" )



limpia <- rename(limpia, direccion='search query')


datos_l <- merge(datos, limpia, by="direccion")

datos_l <- datos_l %>%
             extract(address, c("ciudad_l", "estado_l", "pais_l"), "([^,]+), ([^,]+), ([^)]+)" )
