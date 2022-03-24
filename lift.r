# series_file_list <- list(Train)

liftLRD_df <- data.frame(lift_LRD_test=double())

lift_function <- function (serie) {
  serie <- ts(serie, start=c(2000, 1), frequency=12)
      
  lift_LRD_test <- liftHurst(serie, tradonly=TRUE)
  
  serie <- data.frame(lift_LRD_test=c(lift_LRD_test))
  
  liftLRD_df <- rbind(liftLRD_df, serie)

  return (liftLRD_df)
}
