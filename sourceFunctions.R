#!/usr/bin/env Rscript

renv::load('/home/reichmut/Workspace/modelling')

suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(dismo))
suppressPackageStartupMessages(library(gbm))
suppressPackageStartupMessages(library(gam))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(data.table))


pred <- c('bs_top', #'cec_top', #'bs_top', #'oc_top'
          'growing_degree_days',
          'waterDeficit',
          'ConradsContinentalityIndex',
          'P15_PrecipSeasonality')

modelIteration = function(iter){
  ##separate list to variables
  species<<-iterate_list[[iter]]$Var1
  block<<-iterate_list[[iter]]$Var2
  algo<<-iterate_list[[iter]]$Var3
  dist<<-iterate_list[[iter]]$Var4
  distTwo = str_pad(dist, 2, pad = "0")
  filename=paste0(species,'_block_',block,'_',algo,'_', distTwo)

  ##apply autocorrelation - autocorrelation distance - not larger than 30km
  test=referenceBlock %>% filter(Block==block)

  train=referenceBlock %>% filter(Block!=block)

  train = train %>% drop_na(all_of(species))

  train = autocorrelation(train, dist)
  train = train %>% filter(autocorr==1)
  print(nrow(train))
  presenceTrain=as.numeric(rownames(train[which(train[,species]!=0),]))

  
  trainBefore=train

  if (length(presenceTrain)<=200){
    print(paste0('Model ', algo, ' has not enough train data for Block ', block))
    return()

  }else{
    model <- try(modelling(species=species, modeltype=algo, predictors=pred, train=train, path=path, filename=filename), silent=FALSE)
    if("try-error" %in% class(model)) return()
    saveRDS(model, file=paste0(path,'model/train/model_',filename,'.rds'))

    sink(file=paste0(path,'model/coefficients/summary_',filename,'.txt'))
    print(summary(model))
    sink()

    presenceTest=as.numeric(rownames(test[which(test[,species]>0),]))
    

    if (length(presenceTest)<=200){
      print(paste0('Model ', algo, ' has not enough test data for Block ', block))
      return()
    }else{
      result=evaluation(test, train, species, model, filename, modeltype=algo)
 
      t <- try(histogram(trainBefore, train, result, filename, species), silent=FALSE)#TRUE) 
      if("try-error" %in% class(t)) return()
    }
  }
}

##building the model
modelling = function(species, modeltype, predictors, train, path, filename){
  ##generic predictors input
  print(modeltype)
  train = train[,c(species,predictors)]
  form = modelPreds(predictors)
  if (modeltype=='glm'){

    model_results <- glm(formula=form, family=binomial(link = "cloglog"), data=train)

  } else if (modeltype=='brt'){
    n.trees=100
    step.size=n.trees
    max.trees=10000 #10000
    tc=1
    lr=0.01
    model_results <- gbm_step_ar(gbm.y=1,gbm.x=2:ncol(train), bag.fraction=0.75, data=train, max.trees = max.trees, n.trees = n.trees,
                               step.size = step.size, learning.rate=lr, tree.complexity=tc,silent = TRUE, keep.data=FALSE)#, site.weights=dataWeights) #, family='binomial'
    
  } else if (modeltype=='gam'){
    model_results <- gam(formula=form, family=binomial(link = "cloglog"), data=train)
  }
  return(model_results)
}

##evaluating the input - output and test data need to be formatted same way
evaluation = function(test, train, species, model, filename, modeltype){
  ##limiting the unbalanced relationship between absence and presence

  presence_test = filter(test, get(species)>0)
  absence_test = filter(test, get(species)==0 | is.na(get(species)))
  
  print(paste0(filename, ' test data ', nrow(test)))
  print(paste0(filename, ' train data ', nrow(train)))
  fit=predict(model, type='response')
  predi <- try(predict(model, test[,pred], type="response"), silent=FALSE)
  if("try-error" %in% class(predi)) return()

  png(paste0(path,'model/plots/scatter_',filename,'.png'), width = 1080, height = 780, units = "px", pointsize = 12)
  plot(test[,species], predi, main=paste0('Scatter test vs. prediction data ', filename))
  dev.off()

  ##using threshold from TSS maximising
  ##applies a prediction withon the function - therefore important to state the type - otherwise log values
  thresh_test <- try(threshold(dismo::evaluate(p=presence_test[,pred], a=absence_test[,pred], model=model, type='response')), silent=FALSE)#TRUE)
  if("try-error" %in% class(thresh_test)) return()
    
  eval_test = dismo::evaluate(p=presence_test[,pred], a=absence_test[,pred], model=model, type='response', tr=thresh_test$spec_sens)
  
  if (modeltype!='brt'){
    D2 <- (model$null.deviance - model$deviance) / model$null.deviance
    AIC = model$aic  
  }else{
    ##dont change this - it works
    cv.deviance.mean = model$cv.statistics[[1]]
    weight=sum(train[,species]*rep(1, nrow(train)))/sum(rep(1, nrow(train)))
    u_i <- rep(weight, nrow(train))
    mean.total.deviance = calc.deviance(train[,species], u_i, calc.mean = TRUE)
    D2=(mean.total.deviance-cv.deviance.mean)/mean.total.deviance
    y=model$data$y
    n=y
    rank=length(model$var.names)+1
    ##without warranty - the use of degrees of freedom (rank) is very controversial in brt
    AIC=aic(y, n, fit, rep(1, length(model$data$y)), rank)
  }
  
  predi = round(predi, digits=2)

   if (any(is.na(test[,species]))==TRUE){
    testcor = cor(test[,species],predi, use="complete.obs")
  }else{
    testcor = cor(test[,species],predi)
  }

  ##Kolmogorov-Smirnoff test to check how the test and the prediction distribution vary
  ##two sided is chosen as we want to check if they are different
  
  kolsmir=ks.test(test[,species], predi, alternative = "two.sided")
  combineTestPredi = data.frame(test[,species],predi)
  combineTestPredi = combineTestPredi %>% drop_na()
  
  rmse_test = sqrt(mean(((combineTestPredi[,1]*10000)-(combineTestPredi[,2]*10000))^2))/10000
  traincor = cor(train[,species],fit)
  rmse_train = sqrt(mean(((train[,species]*10000)-(fit*10000))^2))/10000
  
  result <- list()
  result['Species'] = species
  result['Model'] = modeltype
  result['Block'] = block
  result['dist'] = dist
  result['train_data'] = nrow(train)
  result['test_data'] = nrow(test)
  result['D2'] = round(D2, digits=4)
  result['AIC'] = round(AIC, digits=2)
  result['RMSE_train'] = round(rmse_train, digits=4)
  result['RMSE_test'] = round(rmse_test, digits=4)
  result['Corr_train'] = round(traincor, digits=2)
  result['Corr_test'] = round(testcor, digits=2)
  result['pValue_KS_Test'] = kolsmir$p.value
  result['Kappa'] = round(eval_test@kappa, digits=2)
  result['TSS'] = round((eval_test@TPR+eval_test@TNR)-1, digits=2)
  result['Threshold'] = round(thresh_test$spec_sens, digits=10)
  result['Sensitivity'] = round(eval_test@TPR, digits=2)
  result['Specificity'] = round(eval_test@TNR, digits=2)
  result['filename'] = filename

  result = as.data.frame(result)
  write.csv(result, paste0(path,'model/stats/statistics_',filename,'.csv'), row.names = FALSE)
  return(list(predictData=predi,testData=test[,species], fitData=fit))
}

##predict after successful modelling
modelPredict = function(species, dist, period, path){
  filename = paste0(species,'_',period,'_',dist)
  print(filename)
  
  maxTSS = read.csv(paste0(path,'model/prediction/selected_',dist,'.csv'))
  library(dplyr)
  selectBlock = maxTSS %>% filter(Species==species) %>% select(filename)
  print(selectBlock)
  ##read model that revealed best TSS for species and dist
  modelFile = paste0(path,'model/train/model_',selectBlock$filename,'.rds')
  #modelFile = paste0('/data/satellite/forestProjection/sdm/Modelling/model/train/model_',selectBlock$filename,'.rds')
  model <<- try(readRDS(modelFile), silent=TRUE)
      if("try-error" %in% class(model)) return()
  ##needs to be global variable otherwise not able to use further
  if (period=='1971_1990'){
    predictRCP('26', period, path, filename)
  }else{
    rcps = list('45','26','85') 
    for (r in rcps){
      predictRCP(r, period, path, filename)
    }
  }
}

predictRCP = function(rcp, period, path, filename){
  suppressPackageStartupMessages(library(dplyr))
  filename = filename
  if (period=='1971_1990'){
    quantile = predictionBlock
    colnames(quantile)=colnameChange(quantile, paste0('_',period)) 
    predictIter(quantile, period, '', '', path, filename)
  }else{
    quan = c(paste0('rcp',rcp,'_quantile50'), paste0('rcp',rcp,'_quantile5'), paste0('rcp',rcp,'_quantile95'))

    for (q in quan){
      quantile = predictionBlock %>%
        dplyr::select(starts_with('x'), starts_with('y'),contains('bs_top'), contains(paste0(q,'_')))
      colnames(quantile)=colnameChange(quantile, paste0('_',q,'_', period)) #paste0('_',period))
      predictIter(quantile, period, q, rcp, path, filename)
    }
  }
}

predictIter = function(quantile, period, q, rcp, path, filename){
  quantile = quantile %>% drop_na()
  #print(colnames(quantile))
  prediction = predict(model, newdata=quantile, type="response")
  prediction = round(prediction, digits = 2)
  predictionXY = as.data.frame(cbind(quantile[,c(1,2)],prediction))
  sf_test <- sf::st_as_sf(predictionXY, coords = c("x", "y"), crs = raster::crs(raster))
  ##it truly is necessary for rasterization
  poly = as(sf_test, Class='Spatial')
  raster_final=rasterFromXYZ(poly, crs=raster::crs(raster))
  if (period=='1971_1990'){
    writeRaster(raster_final, filename=paste0(path,'model/prediction/reference/tif/Prediction_',filename,'.tif'), overwrite=TRUE)
  }else{
    writeRaster(raster_final, filename=paste0(path,'model/prediction/rcp',rcp,'/tif/Prediction_',filename,'_',q,'.tif'), overwrite=TRUE)
  }
  print(paste0('finished ', period, ' ', q))
  raster_final=NULL
  sf_test=NULL
  poly=NULL
  prediction=NULL
  gc()
}



colnameChange = function(data, pattern){
  columnNames = unlist(str_split(colnames(data), pattern))
  columnNames=columnNames[columnNames != ""]
  return(columnNames)
}

modelPreds = function(pred){
  str=list()
  for (i in seq(1,length(pred),1)){
    str[[i]] = paste0('get(pred[',i,']) + I(get(pred[',i,'])^2)')
  }
  s=paste(str, collapse = ' + ')
  return(as.formula(paste0('get(species) ~ ', s)))
}

simplifyColnames = function(input){
  str=list()
  for (i in seq(1,length(input),1)){
    str[[i]] = paste0('x',i)
  }
  s=paste(str, collapse = ',')
  l=paste0('y,',s)
  return(unlist(str_split(l, ',')))
  }
  
autocorrelation = function(dataframe, dist){
  ##calculate pixels that can be used for modelling based
  ## on autocorrelation distance - maximum of 20km - or too few data points
  ## dist in km
  ##optional to do clhs before - to only use good data
  xmin = round(min(dataframe$x))
  xmax = round(max(dataframe$x))
  ymax = round(max(dataframe$y))
  ymin = round(min(dataframe$y))

  xrange = seq(xmin,xmax,dist*1000)
  yrange = seq(ymax,ymin, dist*(-1000))

  xy = expand.grid(xrange, yrange, stringsAsFactors = FALSE)
  colnames(xy) = c('x', 'y')
  xy=cbind(xy, autocorr=replicate(nrow(xy),1))
  x=round(dataframe$x)
  y=round(dataframe$y)
  autocorrelat = left_join(data.frame(x,y), xy)
  autocorrelat = autocorrelat %>% mutate(autocorr= replace_na(autocorr,0))
  dataframe['autocorr'] = autocorrelat$autocorr
  return(dataframe)
}


aic <- function(y, n, mu, wt, rank) {
  ##function from family.R
  ##necessary for calculating aic also for brt
  ##n seems to be the y data - it is a global variable without explanations
  ##if n set to 0 then weights are used - safer this way
  ##testing this reproduced glm aic
  m <- if(any(n > 1)) n else wt

  (-2*sum(ifelse(m > 0, (wt/m), 0)*
           dbinom(round(m*y), round(m), mu, log=TRUE))) + 2*rank
}


histogram = function(trainBefore, train, result, filename, species){
  png(paste0(path,'model/plots/histogram_',filename,'.png'), width = 1780, height = 2780, units = "px", pointsize = 12)
  par(mfrow=c(2,2))
  hist(trainBefore[,species], main='Histogram train data', xlab='Train probabilities', xlim=c(0,1),cex.lab = 2, cex.axis=2)
  #hist(trainBeforePresence[,species], main='Histogram continuous train data > 0', xlab='Train probabilities >0', xlim=c(0,1),cex.lab = 2, cex.axis=2)
  hist(result$fitData, main='Histogram model fit data', xlab='Fitted values', xlim=c(0,1),cex.lab = 2, cex.axis=2)
  #hist(result[,2], main='Histogram test data', xlab='Test values', xlim=c(0,1),cex.lab = 2, cex.axis=2)
  plot(result$predictData, result$testData, main='Scatter fit vs. prediction data', xlab='Prediction values', ylab='Fitted values', xlim=c(0,1), ylim=c(0,1),cex.lab = 2, cex.axis=2)
  abline(lm(result$predictData ~ result$testData), col = '#1db14e', lwd = 3)
  hist(result$predictData, main='Histogram prediction data', xlab='Prediction values', xlim=c(0,1),cex.lab = 2, cex.axis=2)
  #plot(result[,4], main='Residuals', xlab='Prediction values', cex.lab = 2, cex.axis=2)
  dev.off()
}


rasterToPng = function(filename, rcp, period){
 if (period=='1971_1990'){
   raster = raster(paste0(path,'model/prediction/reference/tif/Prediction_',filename,'.tif'))
   output = paste0('model/prediction/reference/plot/Prediction_',filename,'.pdf')
 }else{
   raster = raster(paste0(path,'model/prediction/rcp',rcp,'/tif/Prediction_',filename,'.tif'))
   output = paste0('model/prediction/rcp',rcp,'/plot/Prediction_',filename,'.pdf')
 }   
  maxval = raster@data@max
  print(maxval)
  limits=c(0,maxval)
  breaks=c(0,round(maxval*0.25,digits=2), round(maxval*0.5, digits=2), round(maxval*0.75, digits=2), maxval)
  rasterPoints=as.data.frame(rasterToPoints(raster))
  colnames(rasterPoints)[3] = 'value'
  g=ggplot() +
  geom_raster(data=rasterPoints, aes(x = x, y = y, fill=value))+
  coord_sf(crs = 3035)+
  scale_fill_gradient(low = "lightgrey", high = "#025002", na.value = NA, limits=limits, breaks=breaks)+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+#, breaks = seq(2000000, 5000000, 500000))+
  theme_bw()+
  theme(plot.margin=margin(t = 1, r = 0, b = 0, l = 0, 'mm'),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, vjust=0.5, hjust=0.5, size=21, margin = margin(t = .3, unit = "cm")),
        axis.text.x = element_text(size=21, margin = margin(t = .3, unit = "cm")), 
        legend.text = element_text(size=26, margin = margin(t = .3, unit = "cm")),
        legend.title=element_blank(),
        legend.position='bottom', 
        legend.direction='horizontal', 
        legend.key.width = unit(4.5, 'cm'),
        legend.key.height = unit(1, 'cm'))
    pdf(paste0(path,output), width = 10, height = 13)    
    print(g)
    dev.off()
  }

evalNatura = function(r){
  species = iterate_list[[r]]$Var1
  time = iterate_list[[r]]$Var2
  dist = iterate_list[[r]]$Var3
  rcp = iterate_list[[r]]$Var4
  perc = iterate_list[[r]]$Var5
  sitetype = iterate_list[[r]]$Var6
  print(time)
  if (toupper(sitetype)=='A' | toupper(sitetype)=='B' | toupper(sitetype)=='C'){
    filenames <<- paste0(species,'_',time,'_',dist,'_rcp',rcp,'_quantile',perc,'_sitetype',sitetype)
    filenameDiff = paste0(species,'_',time,'_',dist,'_rcp',rcp,'_quantile',perc)
  }else{
    filenames <<- paste0(species,'_',time,'_',dist,'_rcp',rcp,'_quantile',perc)
    filenameDiff = paste0(species,'_',time,'_',dist,'_rcp',rcp,'_quantile',perc)
  }
  
  rasterTime =rast(paste0('/data/satellite/forestProjection/sdm/Modelling/prediction/rcp',rcp,'/tif/Prediction_',filenameDiff,'.tif'))
  maxRaster = minmax(reference)[2]
  rasterDiff = ifel(rasterTime==0 & reference==0, -999, (rasterTime-reference)/maxRaster)
  writeRaster(rasterDiff, paste0(path,'model/naturaEval/rasterDiff/rcp',rcp,'/',filenameDiff,'_diff.tif'), overwrite=TRUE)
  rasterDiff=rast(paste0(path,'model/naturaEval/rasterDiff/rcp',rcp,'/',filenameDiff,'_diff.tif'))
  polyRasterDiff=extractPoly(poly,rasterDiff)
  writeVector(polyRasterDiff, paste0(path,'model/naturaEval/polyExtract/rcp',rcp,'/',filenames,'_extract.shp'), overwrite=TRUE)
  polyRasterDiff = vect(paste0(path,'model/naturaEval/polyExtract/rcp',rcp,'/',filenames,'_extract.shp'))

}

extractPoly = function(poly, raster){
  raster = subst(raster, -999, NA)
  poly$mean = round(terra::extract(raster, poly, fun = mean, na.rm = TRUE, ID=FALSE), digits=4)
  poly$median = round(terra::extract(raster, poly, fun = median, na.rm = TRUE, ID=FALSE), digits=4)
  poly$mean[sapply(poly$mean, is.infinite)] <- NA
  v=na.omit(poly, "mean")
  return(v)
}

plotting = function(r){
  species=iterate_list[[r]]$Var1
  period=iterate_list[[r]]$Var2
  dist=iterate_list[[r]]$Var3
  rcp=iterate_list[[r]]$Var4
  perc=iterate_list[[r]]$Var5
  sitetype=iterate_list[[r]]$Var6
  stats = stats
  if (period=='1971_1990'){
    fileNames = paste0(species,'_',period,'_',dist)
  }else{
    fileNames = paste0(species,'_',period,'_',dist,'_rcp',rcp,'_quantile',perc)
  }
  
  if (toupper(sitetype)=='A' | toupper(sitetype)=='B' | toupper(sitetype)=='C'){
    filenames <<- paste0(fileNames,'_sitetype',sitetype)
  }else{
    filenames <<- fileNames
    
  }

  try(plottingRasterDiff(species, period, rcp, perc), silent=FALSE)
  try(plotDiff(period, filenames, rcp, stats), silent=FALSE) #maps of natura 2000 areas
  try(rasterToPng(filenames, rcp, period), silent=FALSE) ##plot predictions
  data=extractData(filenames, rcp, perc, sitetype, period) #boxplots
}

plotDiff = function(period, filename, rcp, stats){
  if (period != '1971_1990') {
    dats = st_read(paste0(path,'model/naturaEval/polyExtract/rcp',rcp,'/',filename,'_extract.shp'))
    dat = st_simplify(dats, preserveTopology = TRUE, dTolerance = 8000)
    
  }else{
    return()
  }
  for (stat in stats){

    low='#8E0152' #"#562456"
    high='#276419' #"#325f2c"
    mid='#FFFFBF' #"#FFFFD8"
    midpoint=0
    limits=c(-1,1)
      g=ggplot() +
      geom_raster(data=rasterPoints, aes(x = x, y = y), fill = "lightgrey")+
      geom_sf(data = dat[stat], aes(colour = get(stat), fill=get(stat)))+
      coord_sf(crs = 3035)+
      scale_colour_gradient2(low = low, mid=mid, high = high,na.value = NA, midpoint=midpoint, limits=limits)+  
      scale_fill_gradient2(low = low, mid=mid, high = high, na.value = NA, midpoint=midpoint, limits=limits)+ 
      scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+#, breaks = seq(2000000, 5000000, 500000))+
      theme_bw()+ 
      theme(plot.margin=margin(t = 1, r = 0, b = 0, l = 0, 'mm'),
            axis.title.x=element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y = element_text(angle = 90, vjust=0.5, hjust=0.5, size=21, margin = margin(t = .3, unit = "cm")),
            axis.text.x = element_text(size=21, margin = margin(t = .3, unit = "cm")), 
            legend.text = element_text(size=26, margin = margin(t = .3, unit = "cm")),
            #panel.grid.major = element_blank(), 
            #panel.grid.minor = element_blank(), 
            legend.title=element_blank(),
            legend.position='bottom', 
            legend.direction='horizontal', 
            legend.key.width = unit(4.5, 'cm'),
            legend.key.height = unit(1, 'cm'))
    pdf(paste0(path,'model/naturaEval/plot/polyDifference/rcp',rcp,'/',filename,'_',stat,'.pdf'), width = 10, height = 13)              
    print(g)
    dev.off()
  }  
  return()
}

plottingRasterDiff=function(species, period, rcp, perc){
  fileNames = paste0(species,'_',period,'_20_rcp',rcp,'_quantile',perc)
  rasterDiff=raster(paste0(path,'model/naturaEval/rasterDiff/rcp',rcp,'/',fileNames,'_diff.tif'))
  rasterPoints=as.data.frame(rasterToPoints(rasterDiff))
  colnames(rasterPoints)[3] = 'value'
  low='#8E0152' #"#562456"
  high='#276419' #"#325f2c"
  mid='#FFFFBF' #"#FFFFD8"
  midpoint=0
  limits=c(-1,1)
    g=ggplot(data=rasterPoints) +
    geom_raster(aes(x = x, y = y), fill = 'lightgrey')+
    geom_raster(aes(x = x, y = y, fill = value))+
    coord_sf(crs =3035)+
    scale_colour_gradient2(low = low, mid=mid, high = high,na.value = NA, midpoint=midpoint, limits=limits)+  
    scale_fill_gradient2(low = low, mid=mid, high = high, na.value = NA, midpoint=midpoint, limits=limits)+ 
    scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+#, breaks = seq(2000000, 5000000, 500000))+
    theme_bw()+ 
    theme(plot.margin=margin(t = 1, r = 0, b = 0, l = 0, 'mm'),
          axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text.y = element_text(angle = 90, vjust=0.5, hjust=0.5, size=21, margin = margin(t = .3, unit = "cm")),
          axis.text.x = element_text(size=21, margin = margin(t = .3, unit = "cm")), 
          legend.text = element_text(size=26, margin = margin(t = .3, unit = "cm")),
          #panel.grid.major = element_blank(), 
          #panel.grid.minor = element_blank(), 
          legend.title=element_blank(),
          legend.position='bottom', 
          legend.direction='horizontal', 
          legend.key.width = unit(4.5, 'cm'),
          legend.key.height = unit(1, 'cm'))
  pdf(paste0(path,'model/naturaEval/plot/rasterDifference/rcp',rcp,'/',fileNames,'_diff.pdf'), width = 10, height = 13)#, res=300)              
  print(g)
  dev.off()
}

extractData=function(filename, rcp, perc, sitetype, period){
  if (endsWith(filename,'_sitetype')==TRUE){
      ##combine all extracted natura2000 areas
      files <- list.files(paste0(path,'model/naturaEval/polyExtract/rcp',rcp,'/'), pattern=".shp", full.names = TRUE) 
      f = files[grepl(filename, files, fixed = TRUE)]
      inter <- lapply(f, st_read)
      dat = do.call(rbind, inter)
  }else if (period != '1971_1990'){
      dat = st_read(paste0(path,'model/naturaEval/polyExtract/rcp',rcp,'/',filename,'_extract.shp'))
  }else{
    return()
  }
  listi = list()
  for (stat in stats){
      data=data.frame(dat)
      data = rename(data,'selector':=!!stat)
      data = rename(data, 'sitetype'='SITETYPE')
      data = data %>% select(selector, MS, m_massive, name_mm, clim, climGroup, sitetype)
      data$period = rep(str_replace(period,'_', '-'),times=nrow(data))
      data$rcp = rep(as.numeric(rcp)/10,times=nrow(data))
      data$perc = rep(perc,times=nrow(data))
      data$stat = rep(stat,times=nrow(data))
      data = rename(data,'value'='selector')
      listi[[stat]] = data
    }  
    datas = do.call(rbind, listi)
    return(datas)
}