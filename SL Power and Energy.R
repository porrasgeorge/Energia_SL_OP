library(ggplot2)
library(dplyr)
library(lubridate)




SL = read.csv("SL_Potencias.CSV", stringsAsFactors = F)
names(SL)[1] = 'Hora'
names(SL)[2] = 'U1'
names(SL)[3] = 'U2'
#SL$Hora <- as_datetime(SL$Hora, format = "%d.%m.%Y %H:%M")
SL$Hora <- as.POSIXct(SL$Hora, format = "%d.%m.%Y %H:%M")



SL <- SL %>% arrange(Hora) %>% 
  mutate(anho = year(Hora), 
         mes = month(Hora),
         dia = day(Hora),
         hora = hour(Hora),
         minuto = minute(Hora),
  )
