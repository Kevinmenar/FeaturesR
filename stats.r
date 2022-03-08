library(stats)

# series_file_list <- list(Train)

stats_df <- data.frame(shapiro_test=double())

stats_generator <- function (file) {
  for (series in file) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      if(length(serie) < 5000){
        shapiro_test <- shapiro.test(serie)
        
        serie <- data.frame(shapiro_test=c(shapiro_test$p.value))
        
        stats_df <- rbind(stats_df, serie)
      } else {
        print("Stats, shapiro: Las series no deben exceder los 5000 datos")
      }
    }
  } 
  tmp <- read.csv("~/MetaLearning/Asistencia/FeaturesR/out.csv")
  tmp <- cbind(tmp, stats_df)
  write.csv(tmp, "~/MetaLearning/Asistencia/FeaturesR/out.csv")
}

# stats_generator(series_file_list)
