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
#rm(SL_Toma)

NivelToma <- rbind(SL_Toma01, SL_Toma02, SL_Toma03)
rm(SL_Toma, SL_Toma01, SL_Toma02, SL_Toma03)

NivelTomaTelemFailed <- NivelToma %>% filter(Flag != 1)
NivelTomaTelemFailed <- cbind(NivelTomaTelemFailed, rep(0, nrow(NivelTomaTelemFailed)))
names(NivelTomaTelemFailed)[4] = "Rank"


n = 1
for (i in 1:(nrow(NivelTomaTelemFailed)- 1)) {
  NivelTomaTelemFailed$Rank[i] = n
  if ((NivelTomaTelemFailed$Hora[i] + 300) != NivelTomaTelemFailed$Hora[i+1]){
    n = n+1
  }
}




# si se desea eliminar los telemetry failed del punto anterior
# TelemFailedRankingGroup <- NivelTomaTelemFailed %>% 
#   group_by(Rank) %>% 
#   summarise(Hora = min(Hora), minutos = 5*n()) %>% 
#   filter(minutos > 30)
# 
# ABorrar <- NivelTomaTelemFailed %>% filter(Rank %in% TelemFailedRankingGroup$Rank)%>% select(Hora)
# NivelToma <- NivelToma %>% filter(!Hora %in% (ABorrar$Hora))
# NivelToma %>% filter(Flag != 1)
# 

