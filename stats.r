library(stats)

stats_generator <- function (serie) {
  stats_df <- data.frame(shapiro_test=double())
  if(length(serie) < 5000){
    shapiro_test <- shapiro.test(serie)
    
    serie <- data.frame(shapiro_test=c(shapiro_test$p.value))
    
    stats_df <- rbind(stats_df, serie)
  } else {
    print("Stats, shapiro: Las series no deben exceder los 5000 datos")
  }
  return (stats_df); 
}

