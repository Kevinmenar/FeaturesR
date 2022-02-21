library(Rcatch22)

series_file_list <- list(Train)

processed_list <- list()

catch22_generator <- function(file) {
  for (series in file) {
    list_size <- length(processed_list)
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      serie <- catch22_all(serie)
      serie <- as.data.frame(t(serie))
      names(serie) <- serie[1,]
      serie <- serie[-1,]
      processed_list[[list_size + i]]<- serie
    }
  }
  
  serie_out <- processed_list[[1]]
  
  for (i in 1:length(processed_list)) {
    if(i < length(processed_list)) {
      serie_out <- rbind(serie_out, processed_list[[i+1]])
    }
  } 
  tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
  tmp <- cbind(tmp, serie_out)
  write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
}

catch22_generator(series_file_list)