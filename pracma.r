library(pracma)

pracma_generator <- function (serie) {

  pracma_df <- data.frame(pracma_hs=double(), pracma_hrs=double(),
                          pracma_he=double(), pracma_hal=double(),
                          pracma_ht=double())
  hurts_test <- hurstexp(serie, d = 128) 
  
  
  serie <- data.frame(pracma_hs=c(hurts_test$Hs), pracma_hrs=c(hurts_test$Hrs),
                      pracma_he=c(hurts_test$He), pracma_hal=c(hurts_test$Hal),
                      pracma_ht=c(hurts_test$Ht))
  
  pracma_df <- rbind(pracma_df, serie)

  return(pracma_df)
}