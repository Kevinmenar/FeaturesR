library(fractal)

series_file_list <- list(Train)

fractal_df <- data.frame(fractal_spectral_lag_window=double(), fractal_spectral_multitaper=double(),
                         fractal_spectral_wosa=double(), fractal_spectral_direct=double(),
                         fractal_ACVF_alpha=double(), fractal_ACVF_beta=double())

fractal_generator <- function (list) {
  for (series in list) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      fractal_spectral_lag_window <- hurstSpec(serie, sdf.method="lag window")
      fractal_spectral_multitaper <- hurstSpec(serie, sdf.method="multitaper")
      fractal_spectral_wosa <- hurstSpec(serie, sdf.method="wosa")
      fractal_spectral_direct <- hurstSpec(serie, sdf.method="direct")
      ## fractal_block_aggabs <- hurstBlock(x, method="aggabs")
      ## fractal_block_higuchi <- hurstBlock(x, method="higuchi")
      ## fractal_ACVF <- hurstACVF(x)
      
      
      
      serie <- data.frame(fractal_spectral_lag_window=c(fractal_spectral_lag_window),
                          fractal_spectral_multitaper=c(fractal_spectral_multitaper),
                          fractal_spectral_wosa=c(fractal_spectral_wosa), 
                          fractal_spectral_direct=c(fractal_spectral_direct))
      
      fractal_df <- rbind(fractal_df, serie)
    }
  } 
  tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
  tmp <- cbind(tmp, fractal_df)
  write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
}

fractal_generator(series_file_list)