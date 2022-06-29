library(nortest)

nortest_generator <- function (serie) {
  nortest_df <- data.frame(lillie_test=double(), anderson_darling=double(),
                         pearson_test=double(), cramer_test=double(),
                         shapiro_francia=double())
  
  if(length(serie) < 5000) {
    lillie_test <- lillie.test(serie)
    anderson_darling <- ad.test(serie)
    pearson_test <- pearson.test(serie)
    cramer_test <- cvm.test(serie)
    shapiro_francia <- sf.test(serie)
    
    serie <- data.frame(lillie_test=lillie_test$p.value, anderson_darling=anderson_darling$p.value,
                        pearson_test=pearson_test$p.value, cramer_test=cramer_test$p.value,
                        shapiro_francia=shapiro_francia$p.value)
    
    nortest_df <- rbind(nortest_df, serie)
  }
  return(nortest_df);
}
