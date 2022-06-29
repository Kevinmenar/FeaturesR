# series_file_list <- list(Train)
library(liftLRD)

lift_function <- function (serie) {
  liftLRD_df <- data.frame(lift_LRD_test=double())

  serie <- ts(serie, start=c(2000, 1), frequency=12)

  lift_LRD_test <- liftHurst(serie, tradonly=TRUE)
  
  serie <- data.frame(lift_LRD_test=c(lift_LRD_test[1]))
  
  liftLRD_df <- rbind(liftLRD_df, serie)

  return (liftLRD_df)
}
