#!/usr/bin/env Rscript

renv::load('/home/reichmut/Workspace/modelling')

arg = commandArgs(trailingOnly=TRUE)

suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(blockCV))

how = arg[1]
range=as.integer(arg[2])
path = "/work/reichmut/Modelling/"
reference = read.csv('/data/satellite/forestProjection/sdm/Modelling/modellingDataFrame1971_1990.csv')

biomodTable = function(ref, block, how, range){
  ##this will allow the division of all reference data into blocks

  ref['Block'] = block$folds_ids
  write.csv(ref,paste0(path,'modellingDataFrame1971_1990_block_',how,'_',range,'.csv'), row.names = FALSE)

  #print('Block data distribution distribution')
  for (i in seq(1,5,1)){
    sink(file=paste0(path,'Block_summary_',i,'_',how,range,'.txt'))
    print(summary(referenceBlock$Block==i)[3])
    sink()
  }
}

if (how=='random'|how=='systematic'){
  print(how)  
  pa_data <- sf::st_as_sf(cbind(reference[,c(1,2,13)]), coords = c("x", "y"), crs = 3035)
  block <- cv_spatial(pa_data, size=range, selection=how, iteration=100, hexagon=FALSE, offset=0)
  #write_sf(pa_data, paste0(path,'blocks_',how,'_',range,'.shp'))
  saveRDS(block, file=paste0(path,'blocks_',how,'_',range,'.rds'))
  pa_data['blocks'] = block$folds_ids
  biomodTable(reference, block, how, range)

  png(paste0(path,'blocks_',how,'_',range,'.png'))
  plot(block)
  dev.off()

} else if (how=='predefined'){
  print(how)
  block = readRDS('/data/satellite/forestProjection/sdm/Modelling/blocks_random_600000.rds')
  poly = block$blocks
  poly = st_as_sf(poly, crs = 3035)
  pa_data <- sf::st_as_sf(cbind(reference[,c(1,2,13)]), coords = c("x", "y"), crs = 3035)
  st_crs(poly) = 3035
  st_crs(pa_data) = 3035
  blockrndm <- cv_spatial(x=pa_data, selection="predefined", user_blocks=poly, folds_column='folds', hexagon=FALSE)
  pa_data['blocks'] = blockrndm$folds_ids
  biomodTable(reference, blockrndm, 'random', range)
  
} else{
print(how)
print('oops something is wrong')
}