library(PerformanceAnalytics)

# series_file_list <- list(Train)

performance_analytics_df <- data.frame(kurtosis_fisher=double(), skewness_fisher=double())

performance_analytics_generator <- function (file) {
  for (series in file) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      dates <- seq(as.Date("2000-01-01"), length=length(serie), by="months")
      serie <- xts(x=serie, order.by=dates)
      
      kurtosis_fisher <- kurtosis(serie, na.rm = TRUE, method = "fisher")
      
      skewness_fisher <- skewness(serie, na.rm = TRUE, method = "fisher")
      
      serie <- data.frame(kurtosis_fisher=kurtosis_fisher, skewness_fisher=skewness_fisher)
      
      performance_analytics_df <- rbind(performance_analytics_df, serie)
    }
  }
  tmp <- read.csv("~/MetaLearning/Asistencia/FeaturesR/out.csv")
  tmp <- cbind(tmp, performance_analytics_df)
  write.csv(tmp, "~/MetaLearning/Asistencia/FeaturesR/out.csv")
}

# performance_analytics_generator(series_file_list)