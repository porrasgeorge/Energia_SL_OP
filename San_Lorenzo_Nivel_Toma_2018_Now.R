library(tidyverse)
SL_Toma <- read.csv("San Lorenzo Nivel Toma.txt", stringsAsFactors = F)
SL_Toma$TIME <- as.POSIXct(SL_Toma$TIME)

glimpse(SL_Toma)

SL_Toma01 <- SL_Toma %>% 
  filter(!is.na(Nivel01)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel01,
         Flag = Flag01) %>%
  select(Hora, Nivel, Flag)

SL_Toma02 <- SL_Toma %>% 
  filter(!is.na(Nivel02)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel02,
         Flag = Flag02) %>%
  select(Hora, Nivel, Flag)

SL_Toma03 <- SL_Toma %>% 
  filter(!is.na(Nivel03)) %>%
  mutate(Hora = TIME,
         Nivel = Nivel03,
         Flag = Flag03) %>%
  select(Hora, Nivel, Flag)
rm(SL_Toma)
t3 <- rbind(SL_Toma01, SL_Toma02, SL_Toma03)
rm(SL_Toma01, SL_Toma02, SL_Toma03)

sum(is.na(t3))
