library(ggplot2)
library(dplyr)
library(lubridate)
library(openxlsx)

corte <- 8350

SL = read.csv("SL_Potencias2.CSV", stringsAsFactors = F)
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
                                      4,
                                      ifelse(hora < 2000,
                                             ifelse(wday(fecha_hora) %in% c(1, 7),
                                                    4, 
                                                    5),
                                             6)
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

SL$periodo <- factor(SL$periodo, levels = c(1, 2, 3, 4, 5, 6), 
                     labels = c("Noche1", "Valle1", "Punta1", "Valle2", "Punta2", "Noche2"))


SL <- SL %>% mutate(E1 = ifelse(U1 > corte, 
                                (U1 - corte) / 4, 
                                0), 
                    E2 = ifelse(U2 > corte, 
                                (U2 - corte) / 4, 
                                0))


SL_dia <- SL %>% 
  group_by(anho, mes, dia, periodo) %>%
  summarise(Energia_U1 = sum(E1), Energia_U2 = sum(E2))

SL_mes <- SL %>% 
  group_by(anho, mes, periodo) %>%
  summarise(Energia_U1 = sum(E1), Energia_U2 = sum(E2))

DS_list <- list("Diario" = SL_dia, "Mensual" = SL_mes)
write.xlsx(DS_list, file = "Energia por periodo.xlsx")
