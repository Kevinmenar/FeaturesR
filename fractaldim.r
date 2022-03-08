

library(fractaldim)
library(TSEntropies)
library(wavelets)

# series_file_list <- list(Train)

stats_df <- data.frame(estim_hallwood=double(), estim_dctII=double(), estim_wavelet=double(),
                       estim_variogram=double(), estim_madogram=double(), estim_rodogram=double(), 
                       estim_periodogram=double(), ApEn=double(), FastSampEn_R=double(), FastApEn_R=double())

fractaldim_generator <- function (file) {
  for (series in file) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      
      
      #Hallwood
      estim_hallwood <- fd.estim.hallwood(serie)
      #DCT
      estim_dctII <- fd.estim.dctII(serie)
      #Wavelet Requires wavelet package
      estim_wavelet <- fd.estim.wavelet(serie)
      #Variogram
      estim_variogram <- fd.estim.variogram(serie)
      #Madogram
      estim_madogram <- fd.estim.madogram(serie)
      #Rodogram
      estim_rodogram <- fd.estim.rodogram(serie)
      #Periodogram
      estim_periodogram <- fd.estim.periodogram(serie)
      
      #Entropy
      #Approximate
      ApEn <- ApEn(serie)
      #Fast Sample
      FastSampEn_R <- FastSampEn_R(serie)
      #Fast Aprox
      FastApEn_R <- FastApEn_R(serie)
      
      print("estim_hallwood")
      print(estim_hallwood)
      print("ApEn")
      print(ApEn)
    }
  } 
  write.csv(all_characteristics, "out.csv", append = TRUE)
}

# fractaldim_generator(series_file_list)
