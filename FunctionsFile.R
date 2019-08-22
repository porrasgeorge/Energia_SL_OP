
Calc_periodo <- function(fecha_hora){
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
