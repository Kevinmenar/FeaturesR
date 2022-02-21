library(liftLRD)

series_file_list <- list(Train)

liftLRD_df <- data.frame(lift_LRD_test=double())

lift_generator <- function(file) {
  for (series in file) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      serie <- ts(serie, start=c(2000, 1), frequency=12)
      
      lift_LRD_test <- liftHurst(serie, tradonly=TRUE)
      
      
      serie <- data.frame(lift_LRD_test=c(lift_LRD_test))
      
      liftLRD_df <- rbind(liftLRD_df, serie)
    }
  }
  tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
  tmp <- cbind(tmp, liftLRD_df)
  write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
}

# lift_generator(series_file_list)