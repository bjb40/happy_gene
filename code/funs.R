#functions and objects

#@@@@@@@@@@@
#requirements and misc
#@@@@@@@@@@@

library(ggplot2);  library(reshape2); library(haven); library(scales)
library(lme4); library(plm); library(merTools); library(knitr); library(dplyr)
library(survival); library(survminer)

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

mktab = function(mod){
  tab = as.data.frame(coef(summary(mod)))
  tab$se = paste0('(',rnd(tab$`Std. Error`),')')
  tab$sig = sig(tab$`Pr(>|t|)`)
  return(tab %>% select(Estimate,se,sig))
}


descriptives = function(df,vars){
  #helper function that returns summary mean, sd, and n
  #df is dataframe
  #vars is character vector of varialbe names
  d=list(  
    mean = df %>% 
      select(one_of(vars)) %>%
      summarize_all(mean,na.rm=TRUE),
    sd = df %>% 
      select(one_of(vars)) %>%
      summarize_all(sd,na.rm=TRUE),
    min = df %>%
      select(one_of(vars)) %>%
      summarize_all(min,na.rm=TRUE),
    max = df %>%
      select(one_of(vars)) %>%
      summarize_all(max,na.rm=TRUE),
    propmiss = df %>% 
      select(one_of(vars)) %>%
      summarize_all(funs(sum(is.na(.))/nrow(analyze)))
  )
  
  res = do.call(cbind,lapply(d,t))
  
  colnames(res) = names(d)
  
  return(res)
  
}


bi.test = function(df,vars,slice){
  #returns a dataframe of t.test summary values by a single dimenstion -- slice
  #df is dataframe
  #vars is character vector of variables to test
  #slice is character object indicating column to slice by
  #NOTE: slice must have only 2 vaues!
  #returns t-test for difference in means for continuous; chi-square test
  #for difference in proporitons for bivariate
  
  #separate df into lists
  df = df[,c(vars,slice)]
  df = split(df,as.factor(unlist(df[,slice])))
  
  res = data.frame(mean1 = numeric(), 
                   mean2=numeric(), 
                   diff = numeric(), 
                   tval = numeric(), 
                   df= numeric(), 
                   pval = numeric())
 
  for(i in v){
    v1 = unlist(df[[1]][,i])
    v2 = unlist(df[[2]][,i])
    
    if(length(table(v1))>2){
      r = t.test(v1,v2)
    } else{
      
      #r = list(estimate=c(NA,NA),statistic=NA,parameter=NA,p.value=NA)
      
      r = prop.test(rbind(rev(table(v1)),rev(table(v2))))
      
    }
      rr = data.frame(mean1=r$estimate[1],
                      mean2=r$estimate[2],
                      diff = r$estimate[1]-r$estimate[2],
                      stat = r$statistic,
                      df = r$parameter,
                      pval = r$p.value)
    
      res = rbind(res,rr)
  }
   
  row.names(res) = v
  
  return(res)
  
}
