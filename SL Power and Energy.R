library(ggplot2)
library(dplyr)
library(lubridate)
library(openxlsx)
source("FunctionsFile.R")

corte <- 8350
corte_contTiempo <- 5000
Precios <- c(0.016804, 0.026858, 0.067145)

PreciosPeriodos <- c(Precios[1], 
                     Precios[2], 
                     Precios[3],
                     Precios[2],
                     Precios[3],
                     Precios[1])

PreciosPeriodos_c <- c(Precios[1], 
                     Precios[2], 
                     Precios[3] - Precios[2],
                     Precios[2],
                     Precios[3] - Precios[1],
                     Precios[1])

SL <- read.csv("SL_Potencias2.CSV", stringsAsFactors = F)
names(SL)[1] <- 'Hora'
names(SL)[2] <- 'P1'
names(SL)[3] <- 'P2'
SL$Hora <- as.POSIXct(SL$Hora, 
                      format = "%d.%m.%Y %H:%M") - 60

SL <- SL %>% arrange(Hora) %>% 
  mutate(anho = year(Hora), 
         mes = month(Hora),
         dia = day(Hora),
         hora = hour(Hora),
         minuto = (minute(Hora) %/% 15) * 15,
         dow = wday(Hora),
         periodo = Calc_periodo(Hora)) %>%
  group_by(anho, mes, dia, hora, minuto) %>%
  summarise(U1 = sum(P1)/15, 
            U2 = sum(P2)/15, 
            periodo = mean(periodo),
            minutes1 = mean(P1 > corte_contTiempo),
            minutes2 = mean(P2 > corte_contTiempo))

SL$periodo <- factor(SL$periodo, levels = c(1, 2, 3, 4, 5, 6), 
                     labels = c("Noche1", "Valle1", "Punta1", "Valle2", "Punta2", "Noche2"))

SL <- SL %>% mutate(E1 = U1 / 4,
                    E2 = U2 / 4,
                    E1_c = ifelse(U1 > corte, 
                                (U1 - corte) / 4, 
                                0), 
                    E2_c = ifelse(U2 > corte, 
                                (U2 - corte) / 4, 
                                0),
                    Monto_U1 = E1 * PreciosPeriodos[periodo],
                    Monto_U2 = E2 * PreciosPeriodos[periodo],
                    Monto_U1_c = E1_c * PreciosPeriodos_c[periodo],
                    Monto_U2_c = E2_c * PreciosPeriodos_c[periodo])

SL_dia_per <- SL %>% 
  group_by(anho, mes, dia, periodo) %>%
  summarise(Energia_U1 = sum(E1), 
            Energia_U2 = sum(E2),
            Tiempo_U1 = mean(minutes1),
            Tiempo_U2 = mean(minutes2),
            Monto_U1 = sum(Monto_U1),
            Monto_U2 = sum(Monto_U2),
            Monto_U1_c = sum(Monto_U1_c),
            Monto_U2_c = sum(Monto_U2_c))

SL_dia <- SL %>% 
  group_by(anho, mes, dia) %>%
  summarise(Energia_U1 = sum(E1), 
            Energia_U2 = sum(E2),
            Tiempo_U1 = mean(minutes1),
            Tiempo_U2 = mean(minutes2),
            Monto_U1 = sum(Monto_U1),
            Monto_U2 = sum(Monto_U2),
            Monto_U1_c = sum(Monto_U1_c),
            Monto_U2_c = sum(Monto_U2_c))

SL_mes <- SL %>% 
  group_by(anho, mes, periodo) %>%
  summarise(Energia_U1 = sum(E1), 
            Energia_U2 = sum(E2),
            Tiempo_U1 = mean(minutes1),
            Tiempo_U2 = mean(minutes2),
            Monto_U1 = sum(Monto_U1),
            Monto_U2 = sum(Monto_U2),
            Monto_U1_c = sum(Monto_U1_c),
            Monto_U2_c = sum(Monto_U2_c))

DS_list <- list("Diario_per" = SL_dia_per, "Diario" = SL_dia, "Mensual" = SL_mes)
write.xlsx(DS_list, file = "C:/Data Science/ArhivosGenerados/Energia por periodo.xlsx")