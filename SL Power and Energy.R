library(ggplot2)
library(dplyr)
library(lubridate)

corte <- 8400

SL = read.csv("SL_Potencias.CSV", stringsAsFactors = F)
names(SL)[1] = 'Hora'
names(SL)[2] = 'U1'
names(SL)[3] = 'U2'
SL$Hora <- as.POSIXct(SL$Hora, format = "%d.%m.%Y %H:%M") - 60

periodo <- function(fecha_hora){
  hora <- 100*hour(fecha_hora) + minute(fecha_hora)
  
  per <-  ifelse(hora < 600,
                 1 ,
                 ifelse(hora < 1000,
                        2, 
                        ifelse(hora < 1230, 
                               ifelse(wday(fecha_hora) %in% c(1, 7),
                                      2, 
                                      3),
                               ifelse(hora < 1730, 
                                      2,
                                      ifelse(hora < 2000,
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
         minuto = (minute(Hora) %/% 15) * 15,
         dow = wday(Hora),
         periodo = periodo(Hora)) %>%
  group_by(anho, mes, dia, hora, minuto) %>%
  summarise(U1 = sum(U1)/15, U2 = sum(U2)/15, periodo = mean(periodo))

SL$periodo <- factor(SL$periodo, levels = c(1, 2, 3), labels = c("Noche", "Valle", "Punta"))


SL <- SL %>% mutate(E1 = ifelse(U1 > corte, 
                                (U1 - corte) / 4, 
                                0), 
                    E2 = ifelse(U2 > corte, 
                                (U2 - corte) / 4, 
                                0)) %>%
  group_by(anho, mes, dia, periodo) %>%
  summarise(T1 = sum(E1), T2 = sum(E2))
