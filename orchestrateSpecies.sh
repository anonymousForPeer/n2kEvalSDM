#!/bin/bash

species=("Picea.abies" "Fagus.sylvatica" "Quercus.sp"
"Quercus.robur" "Quercus.petraea" "Larix.decidua" 
"Abies.alba" "Corylus.avellana" "Pinus.halepensis" "Pseudotsuga.menziesii" 
"Quercus.suber" "Abies.sp" "Pinus.mugo" "Quercus.cerris" 
"Robinia.pseudoacacia" "Acer.campestre" "Fraxinus.excelsior" 
"Pinus.nigra" "Quercus.faginea" "Salix.caprea" "Acer.pseudoplatanus" 
"Fraxinus.ornus" "Pinus.pinaster" "Quercus.frainetto" "Sorbus.aucuparia" 
"Alnus.glutinosa" "Pinus.pinea" "Quercus.ilex" "Tilia.sp" "Alnus.incana" 
"Betula.sp" "Picea.sitchensis" "Populus.nigra" "Quercus.pubescens"
"Carpinus.betulus" "Populus.tremula" "Quercus.pyrenaica"
"Pinus.sylvestris" "Castanea.sativa" "Prunus.avium" "Pinus.contorta")

model='gam' #('gam','glm','brt')
dist=20
for spec in ${species[*]}
do
  echo $spec  # -a 26,45,85  -necessary for compare
  Rscript --vanilla blockSeparation.r predefined 600000 #random
  #Rscript --vanilla modelling.r "$species" "$model" "$dist" #20
  #Rscript --vanilla prediction.r "$species" 20 #"$dist"
  #Rscript --vanilla compare.r "$species" "$dist" "ABC"
echo ' '
done