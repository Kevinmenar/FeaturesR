library(PerformanceAnalytics)


performance_analytics_generator <- function (serie) {
  performance_analytics_df <- data.frame(kurtosis_fisher=double(), skewness_fisher=double())

  dates <- seq(as.Date("2000-01-01"), length=length(serie), by="months")
  serie <- xts(x=serie, order.by=dates)
  
  kurtosis_fisher <- kurtosis(serie, na.rm = TRUE, method = "fisher")
  
  skewness_fisher <- skewness(serie, na.rm = TRUE, method = "fisher")
  
  serie <- data.frame(kurtosis_fisher=kurtosis_fisher, skewness_fisher=skewness_fisher)
  
  performance_analytics_df <- rbind(performance_analytics_df, serie)

  return (performance_analytics_df);
}
