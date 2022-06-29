library(tseries)

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

tseries_generator <- function (serie) {
      t_series_df <- data.frame(adf=double(), kpps_level=double(),
                          kpps_trend=double(), pp=double(),
                          pacf_ts=double())

      serie <- ts(serie, start=c(2000, 1), frequency=12)
      
      serie <- tserie_function(serie)
      
      t_series_df <- rbind(t_series_df, serie)

      return (t_series_df); 
}

# tseries_generator(series_file_list)
