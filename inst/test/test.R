# TODO: Add comment
# 
# Author: ecor
###############################################################################
rm(list=ls())
library(geotopbricks)
library(geotopAnalysis)


source('/home/ecor/Dropbox/R-packages/geotopAnalysis/R/GEOtop_readValidationData.R')

#'  ## TO DO 
wpath <- '/home/ecor/activity/2016/eurac2016/Incarico_EURAC/Simulations/B2/B2_BeG_017_DVM_001' 
load(file.path(wpath, "obs", "observation.RData"))
out <- GEOtop_ReadValidationData(wpath = wpath, obs = observation, save_rData = TRUE)



#' 



