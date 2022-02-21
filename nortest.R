library(nortest)

series_file_list <- list(Train)

nortest_df <- data.frame(lillie_test=double(), anderson_darling=double(),
                         pearson_test=double(), cramer_test=double(),
                         shapiro_francia=double())

nortest_generator <- function (file) {
  for (series in file) {
    for (i in 1:nrow(series)) {
      sub_serie <- subset(series, select = -c(Tipo,Serie))
      serie <- unname(unlist(sub_serie[i,]))
      serie <- serie[!is.na(serie)]
      
      if(length(serie) < 5000){
        lillie_test <- lillie.test(serie)
        anderson_darling <- ad.test(serie)
        pearson_test <- pearson.test(serie)
        cramer_test <- cvm.test(serie)
        shapiro_francia <- sf.test(serie)
        
        serie <- data.frame(lillie_test=lillie_test$p.value, anderson_darling=anderson_darling$p.value,
                            pearson_test=pearson_test$p.value, cramer_test=cramer_test$p.value,
                            shapiro_francia=shapiro_francia$p.value)
        
        nortest_df <- rbind(nortest_df, serie)
      } else {
        print("Nortest: Las series no deben exceder los 5000 datos")
      }
    }
  }
  tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
  tmp <- cbind(tmp, nortest_df)
  write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\out.csv")
}

# nortest_generator(series_file_list)