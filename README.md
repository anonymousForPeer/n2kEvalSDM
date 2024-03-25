# n2kEvalSDM

This code is for modelling, projecting and evalutating species distribution models for 41 tree species. The code is written for use on a slurm HPC but should be runnable on a desktop machine.
Change arguments according to the R script you want to run. 

The orchestrateSpecies.sh is starting various R scripts for 
* separating into spatial blocks (blockSeparation.r)
* modelling (modelling.r)
* predicting (prediction.r)
* evaluating for Natura 2000 areas (compare.r)


## Spatial blocking
* uncomment blockSeparation.r in orchestrateSpecies.sh
* run ```bash orchestrateSpecies.sh``` in terminal

## Modelling
* Change model accroding to the one you want to run in orchestrateSpecies.sh
* uncomment modelling.r
* run ```bash orchestrateSpecies.sh``` in terminal

## Prediction
* uncomment prediction.r in orchestrateSpecies.sh
* run ```bash orchestrateSpecies.sh``` in terminal

## Evaluating species dstribution changes for Natura 2000 sites
* uncomment compare.r in orchestrateSpecies.sh
* keep "ABC" or change to either or if you want to evaluate for only Birds (A) or Habitats Directive (B) or both (C) areas
* run ```bash orchestrateSpecies.sh``` in terminal


In all codes the paths are changed. So please chnage paths in all files according to your preferences.
