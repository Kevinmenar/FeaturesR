library(longmemo)

longmemo_generator <- function (serie) {

    longmemo_df <- data.frame(whittle_test=double())

    whittle_test <- WhittleEst(serie)
    
    serie <- data.frame(whittle_test=c(whittle_test$n))
    
    longmemo_df <- rbind(longmemo_df, serie)

    return(longmemo_df)
      
}
