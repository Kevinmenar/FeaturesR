library(fractal)

fractal_generator <- function (serie) {
  fractal_df <- data.frame(fractal_spectral_lag_window=double(), fractal_spectral_multitaper=double(),
                         fractal_spectral_wosa=double(), fractal_spectral_direct=double(),
                         fractal_ACVF_alpha=double(), fractal_ACVF_beta=double())
      
  fractal_spectral_lag_window <- hurstSpec(serie, sdf.method="lag window")
  fractal_spectral_multitaper <- hurstSpec(serie, sdf.method="multitaper")
  fractal_spectral_wosa <- hurstSpec(serie, sdf.method="wosa")
  fractal_spectral_direct <- hurstSpec(serie, sdf.method="direct")
  
  
  
  serie <- data.frame(fractal_spectral_lag_window=c(fractal_spectral_lag_window),
                      fractal_spectral_multitaper=c(fractal_spectral_multitaper),
                      fractal_spectral_wosa=c(fractal_spectral_wosa), 
                      fractal_spectral_direct=c(fractal_spectral_direct))
  
  fractal_df <- rbind(fractal_df, serie)

  return(fractal_df)
}