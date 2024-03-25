#!/usr/bin/env Rscrip

renv::load('/home/reichmut/Workspace/modelling')

arg = commandArgs(trailingOnly=TRUE)

source("~/Workspace/modelling/sourceFunctions.R")

name = arg[1]
dist = as.numeric(arg[2])

print(name)
print(dist)

periods = c('2041_2060','2079_2098','1971_1990','2061_2080','2021_2040',)

path = "/work/reichmut/Modelling/"
dist=str_pad(dist, 2, pad = "0")
################################################################################
############################## Prediction of model #############################
################################################################################

raster = raster('/data/satellite/forestProjection/sdm/Modelling/Europe_Modelling_all.tif')
#### cleaning soil parameters - according to meta data -1 classes are useful - remaining class is fill class of no data
for (i in periods){
  predictionBlock <<- try(read.csv(paste0('/data/satellite/forestProjection/sdm/Modelling/modellingDataFrame',i,'.csv')), silent=TRUE)
      if("try-error" %in% class(predictionBlock)) return()
  modelPredict(name, dist, i, path)
}