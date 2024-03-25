#!/usr/bin/env Rscrip

print(.libPaths())
x1=Sys.time()
arg = commandArgs(trailingOnly=TRUE)
suppressPackageStartupMessages(library(MASS))
source("sourceFunctions.R")
species = arg[1]
algo = str_split(arg[2],',')
block = seq(1,5,1)
print(species)
dist = as.numeric(arg[3])
path = "/path/to/Folder/"

print(dist)

referenceBlock = read.csv('/pathToInputData/modellingDataFrame1971_1990_block_random_600000.csv')

#important as prediction fails later on with different column names
##rename species_rpp to without _rpp
colnames(referenceBlock) = colnameChange(referenceBlock, '_rpp')
colnames(referenceBlock) = colnameChange(referenceBlock, '_1971_1990')
#### cleaning soil parameters - according to meta data -1 classes are useful - remaining class is fill class of no data


#NA remove in predictors
referenceBlock = referenceBlock %>% drop_na(all_of(pred))

referenceBlock[species][referenceBlock[species]<0]<-NA
##minimum cutoff as 0.02251 and below are fill values

referenceBlock[species][referenceBlock[species]>=0.02249 & referenceBlock[species]<=0.02251]<-NA
referenceBlock[species] = round(referenceBlock[,species], digits=2)
#referenceBlock[species] = round(referenceBlock[species], digits=2)
referenceBlock = referenceBlock %>% drop_na(all_of(species))

##rename preds to without _1971_1990
##generate all combinations to iterate through
paramList=expand.grid(species, block, algo[[1]], dist, stringsAsFactors = FALSE)
#make nested list for calling in parallel computing
iterate_list <- split(paramList, seq(nrow(paramList)))

purrr::walk(.x=1:nrow(paramList), .f=modelIteration)
