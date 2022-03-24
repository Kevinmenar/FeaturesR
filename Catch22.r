catch22_function <- function(serie, list_size, i) {
  serie_out <- catch22_all(serie, catch24 = FALSE)
  serie_out <- as.data.frame(t(serie_out))
  names(serie_out) <- serie_out[1,]
  serie_out <- serie_out[-1,]
  return (serie_out)
}
