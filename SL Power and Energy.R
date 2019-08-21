library(ggplot2)
library(dplyr)
library(lubridate)




SL = read.csv("SL_Potencias.CSV", stringsAsFactors = F)
names(SL)[1] = 'Hora'
names(SL)[2] = 'U1'
names(SL)[3] = 'U2'
SL$Hora <- as.POSIXct(SL$Hora, format = "%d.%m.%Y %H:%M")

periodo <- function(fecha_hora){
  hora <- 100*hour(fecha_hora) + minute(fecha_hora)
  
  per <-  ifelse(hora <= 600,
                 1 ,
                 ifelse(hora <= 1000,
                        2, 
                        ifelse(hora <= 1230, 
                               ifelse(wday(fecha_hora) %in% c(1, 7),
                                      2, 
                                      3),
                               ifelse(hora <= 1730, 
                                      2,
                                      ifelse(hora <= 2000,
                                             ifelse(wday(fecha_hora) %in% c(1, 7),
                                                    2, 
                                                    3),
                                             1)
                                      )
                              )
                        )
                 )
  return(per)
}

SL <- SL %>% arrange(Hora) %>% 
  mutate(anho = year(Hora), 
         mes = month(Hora),
         dia = day(Hora),
         hora = hour(Hora),
         minuto = minute(Hora),
         dow = wday(Hora),
         periodo = periodo(Hora)
  )
