#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

#install.packages('nortest')
#install.packages('stats')
#install.packages('liftLRD')
#install.packages('pracma')
#install.packages('fractal')
#install.packages('tsfeatures')
#install.packages('tseries')
#install.packages('longmemo')
#install.packages('PerformanceAnalytics')
#install.packages('WeightedPortTest')
#install.packages('fractaldim')
#install.packages('TSEntropies')
#install.packages('ForeCA')
#install.packages('pracma')
#install.packages('anomalize')
#install.packages('tidyverse')
#install.packages('wavelets')
#install.packages("Rcatch22")

# source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\main.R")

library(foreach)
library(doParallel)
library(Rcatch22)
library(liftLRD)

#source("nortest.R")
#source("stats.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\lift.R") # -> Cambiar path
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\Catch22.R")
#source("pracma.R")
# source("fractal.R")
#source("tsfeatures.R")
#source("tseries.R")
#source("longmemo.R")
#source("PerformanceAnalytics.R")


Train <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\Train.csv", header=T)
file <- list(Train)
numCores <- detectCores()

out_frame <- data.frame()
registerDoParallel(numCores)

for (series in file) {
  foreach (i = 1:nrow(series), .packages=c('Rcatch22', 'liftLRD')) %dopar% {
  #for (i in 1:nrow(series)) {

    sub_serie <- subset(series, select = -c(Tipo,Serie))
    serie <- unname(unlist(sub_serie[i,]))
    serie <- serie[!is.na(serie)]
    tmp <- data.frame()

    catch_22_df <- catch22_function(serie, list_size, i)
    lift_df <- lift_function(serie)
    
    tmp <- cbind(tmp, catch_22_df)
    tmp <- cbind(tmp, lift_df)

    out_frame <- rbind(out_frame, tmp)
  }
}

tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\out.csv")
tmp <- cbind(tmp, out_frame)
write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\out.csv")

#nortest_generator(file)
#stats_generator(file)
#lift_generator(file)
#catch22_generator(file)
#pracma_generator(file)
#fractal_generator(file)
#tsFeatures_generator(file)
#tseries_generator(file)
#longmemo_generator(file)
#performance_analytics_generator(file)
