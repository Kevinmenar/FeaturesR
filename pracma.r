library(pracma)

series_file_list <- list(Train)

pracma_df <- data.frame(pracma_hs=double(), pracma_hrs=double(),
                        pracma_he=double(), pracma_hal=double(),
                        pracma_ht=double())

pracma_generator <- function (file) {
  for (series in series_file_list) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      serie <- ts(serie, start=c(2000, 1), frequency=12)
      
      hurts_test <- hurstexp(serie, d = 128) 
      
      
      serie <- data.frame(pracma_hs=c(hurts_test$Hs), pracma_hrs=c(hurts_test$Hrs),
                          pracma_he=c(hurts_test$He), pracma_hal=c(hurts_test$Hal),
                          pracma_ht=c(hurts_test$Ht))
      
      pracma_df <- rbind(pracma_df, serie)
    }
  }
  tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
  tmp <- cbind(tmp, pracma_df)
  write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
}

pracma_generator(series_file_list)