#functions and objects

#@@@@@@@@@@@
#requirements and misc
#@@@@@@@@@@@

library(ggplot2);  library(reshape2); library(haven); library(scales)
library(lme4); library(plm); library(merTools); library(knitr); library(dplyr)

#load plotting theme for b-w presentation style--
source('H:/projects/proposal/r_study/code/themes.R',echo=TRUE)


#@@@@@@@@@@@@@@@@@@@@@
#Functions/objects 
#@@@@@@@@@@@@@@@@@@@@@

rnd = function(db,rd){
  # rounds input to preserve leading zeros
  #
  # Args:
  #   db: an object with numeric types
  #   rd: length to round (including leading zeros, default=3)
  #
  # Returns:
  #   an object of of db translated to characters with leading zeros
  
  if(missing(rd)){rd=3}
  rdl=paste0('%.',rd,'f')
  return(sprintf(rdl,round(db,digits=rd)))
}

sig = function(pv){
  # returns stars based on pvalue
  #
  # Args:
  #   pv: a p-value
  #
  # Returns:
  #   a string with stars for values * <.05 **<.01 *** < .001
  s=rep(' ',length(pv))
  s[pv<.001] = '***'
  s[pv<.01 & pv>=.001] = '**'
  s[pv<.05 & pv>=.01] = '*'
  s[pv<.1 & pv>=.05] = '+'
  
  return(s)
  
}


mnsd = function(x){
  #returns rounded mean and sd for vector 
  return(
    c(rnd(mean(x,na.rm=TRUE)),
      paste0('(',rnd(sd(x,na.rm=TRUE)),')')
    )#end combine
  )#end return
}

