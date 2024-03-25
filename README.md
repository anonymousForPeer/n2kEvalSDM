# n2kEvalSDM

This code is for modelling, projecting and evalutating species distribution models for 41 tree species. The code is written for use on a slurm HPC but should be runnable on a desktop machine.
Change arguments according to the R script you want to run. 

The orchestrateSpecies.sh is starting various R scripts for 
* separating into spatial blocks (blockSeparation.r)
* modelling (modelling.r)
* predicting (prediction.r)
* evaluating for Natura 2000 areas (compare.r)
* 
