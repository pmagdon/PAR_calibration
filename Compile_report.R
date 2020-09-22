# Script for compiling the calibration reports

library(brew)
library(tools)
library(filesstrings)
brew("calibration.brew", "calibration.tex",chdir = TRUE)

texi2dvi("calibration.tex", pdf = TRUE, clean = TRUE)


get_results<-function(model){
  factor<-model$coefficients
  id<-names(model$coefficients)
  return(data.frame(id=id,factor=factor))
}

factors=lapply(names(results),FUN = function(x) get_results(results[[x]]))

output<-do.call(rbind.data.frame,factors)

write.csv(output,file='CalibrationFactors.csv',row.names = F)

