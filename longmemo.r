library(longmemo)

series_file_list <- list(Train)

longmemo_df <- data.frame(whittle_test=double())

longmemo_generator <- function (file) {
  for (series in file) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      whittle_test <- WhittleEst(serie)
      
      
      serie <- data.frame(whittle_test=c(whittle_test$n))
      
      longmemo_df <- rbind(longmemo_df, serie)
      
    }
  }
  tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
  tmp <- cbind(tmp, longmemo_df)
  write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
}

# longmemo_generator(series_file_list)
