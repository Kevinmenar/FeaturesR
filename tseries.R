library(tseries)

# series_file_list <- list(Train)

t_series_df <- data.frame(adf=double(), kpps_level=double(),
                          kpps_trend=double(), pp=double(),
                          pacf_ts=double())

tserie_function <- function(serie) {
  out <- tryCatch(
    {
      adf <- adf.test(serie,alternative="stationary", k=12)
      kpps_level <- kpss.test(serie, null = "Level")
      kpps_trend <- kpss.test(serie, null = "Trend")
      pp <- pp.test(serie, alternative = "stationary")
      pacf_ts <- pacf(serie, lag = 12)
      
      O_serie <- data.frame(adf=c(adf$p.value), kpps_level=c(kpps_level$p.value),
                            kpps_trend=c(kpps_trend$p.value), pp=c(pp$p.value))
      return(O_serie)
    },
    error=function(cond) {
      t_series_df <- data.frame(adf=double(), kpps_level=double(),
                                kpps_trend=double(), pp=double(),
                                pacf_ts=double())
      message(paste("This serie has a problem:", serie))
      message("Here's the original error message:")
      message(cond)
      return(t_series_df)
    }
  )    
  return(out)
}
tseries_generator <- function (file) {
  for (series in file) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      serie <- ts(serie, start=c(2000, 1), frequency=12)
      
      serie <- tserie_function(serie)
      
      t_series_df <- rbind(t_series_df, serie)
    }
  }
  tmp <- read.csv("~/MetaLearning/Asistencia/FeaturesR/out.csv")
  tmp <- cbind(tmp, t_series_df)
  write.csv(tmp, "~/MetaLearning/Asistencia/FeaturesR/out.csv")
}

# tseries_generator(series_file_list)
