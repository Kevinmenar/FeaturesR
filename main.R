# Para instalación en Kabré

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

library(stats)
library(fractal)
library(pracma)
library(rlang)
library(dplyr)
library(foreach)
library(doParallel)
library(Rcatch22)
library(liftLRD)
library(tseries)
library(longmemo)
library(nortest)
library(PerformanceAnalytics)

library(tsfeatures)
library(WeightedPortTest)
library(fractaldim)
library(TSEntropies)
library(ForeCA)

# Todas las rutas se encuentran absolutas, debido a un error con windows y las particiones. Para el ambiente de kabré se puede utilizar el direccionamiento dinámico ej: ./nortest.R

source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\nortest.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\stats.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\lift.R") # -> Cambiar path
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\Catch22.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\pracma.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\fractal.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\tseries.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\longmemo.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\PerformanceAnalytics.R")
source("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\tsfeatures.R")

Train <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\Train.csv", header=T)
file <- list(Train)
numCores <- detectCores()

out_frame <- data.frame()
registerDoParallel(numCores)

for (series in file) {
  results_list  <- foreach (i = 1:nrow(series), .packages=c('Rcatch22', 'liftLRD', 'tseries', 'longmemo', 'nortest', 'PerformanceAnalytics', 'stats', 'pracma', 'fractal', 'tsfeatures', 'WeightedPortTest', 'fractaldim', 'TSEntropies', 'ForeCA')) %dopar% {
  #for (i in 1:nrow(series)) {
    sub_serie <- subset(series, select = -c(Tipo,Serie))
    serie <- unname(unlist(sub_serie[i,]))
    serie <- serie[!is.na(serie)]
    tmp <- data.frame(id = c (0:0))

    # El único hiperparamentro utilizado fue el lag. Este es de 12
    catch_22_df <- catch22_function(serie, list_size, i)
    lift_df <- lift_function(serie)
    tseries_df <- tseries_generator(serie)
    longmemo_df <- longmemo_generator(serie)
    nortest_df <- nortest_generator(serie)
    performance_analytics <- performance_analytics_generator(serie)
    stats_df <- stats_generator(serie)
    pracma_df <- pracma_generator(serie)
    fractal_df <- fractal_generator(serie)

    tmp <- cbind(tmp, catch_22_df)
    tmp <- cbind(tmp, lift_df)
    tmp <- cbind(tmp, tseries_df)
    tmp <- cbind(tmp, longmemo_df)
    tmp <- cbind(tmp, nortest_df)
    tmp <- cbind(tmp, performance_analytics)
    tmp <- cbind(tmp, stats_df)
    tmp <- cbind(tmp, pracma_df)
    tmp <- cbind(tmp, fractal_df)

    tmp
  }
    out_frame <- bind_rows(results_list)
}

tmp <- read.csv("D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\out.csv")
tmp <- cbind(tmp, out_frame)
write.csv(tmp, "D:\\Klaus\\Docs\\University\\Asistencia\\4000\\FeaturesScripts\\out.csv")

