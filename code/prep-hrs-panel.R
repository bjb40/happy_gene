#Dev with R 3.2.1
#Script for project (Study 2) analyzing Recessions using HRS
#This is the parent script that subsets data.

#clear cache
rm(list=ls())

#@@@@@@@@@@@@@@
#Generals
#@@@@@@@@@@@@@@
#load universals configuration file

st = proc.time()[3]

source("code/config~.R",
       echo =T, print.eval = T, keep.source=T)

hrsdir = "H:/Academic Projects/Data Files/HRS/"

#@@@@@@@@@
#Creat subset of HRS data for analysis
#@@@@@@@@@

rawzip = paste0(hrsdir,"randpstata.zip")
zipdat = unzip(rawzip,"randpstata/rndhrs_p.dta")

library(foreign)
rawhrs = read.dta(zipdat, convert.factors = FALSE)

#load and merge gene data
library(haven); library(reshape2); library(dplyr)
pgsdat = read_dta('H:/Academic Projects/Data Files/HRS/PGENSCORE/PGENSCOREA_R.dta')
colnames(pgsdat) = tolower(colnames(pgsdat))
pgsdat$rahhidpn = paste0(pgsdat$hhid,pgsdat$pn)

rawhrs = merge(rawhrs,pgsdat,by='rahhidpn',all.x=TRUE) 

rawhrs$has_pgs = ifelse(!is.na(rawhrs$version),TRUE,FALSE)


#lmited to wave 6 (2002) to wave 11 (2012) 
waves = 6:11
years = c(rep(NA,5),2002,2004,2006,2008,2010,2012)

#this function builds character arrays based on
#rand data and the waves selected
makeseq = function(pre,w,post){
  return(paste(pre,w,post,sep=''))
}

#select variables to subset
vars = c(
  #independant id variable
  'hhidpn','rahhidpn',
  #key dependant (growth) variable
  makeseq('r',waves,'cesd'),
  makeseq('r',waves,'drinkn'),
  
  #interview date month and year
  makeseq('r',waves,'iwendm'),
  makeseq('r',waves,'iwendy'),
  
  #alt growth variable percentages (leaving retirement,suriviving,continue working)
  #self reported probability of working after 62 and 65 respectivley on 10 point scale (multiplied by 10)
  makeseq('r',waves,'work62'),
  makeseq('r',waves,'work65'),
  makeseq('r',waves,'shlt'), #self-reported health
  
  #@@time variable baseline to be calculated from birthyear
  'rabyear',
  
  #@@time-invariant variables associated with depressive symptoms
  
  #gender 1=male, 2=female
  'ragender','has_pgs',
  
  #education
  'raedyrs',
  
  #race 1=white, 2=Black, 3=other
  'raracem',
  
  #@@time varying controls
  
  #income (household; rand imputed: dollars)--may need separate SSI
  makeseq('h',waves,'itot'),
  
  #wealth (household; rand imputed: dollars; includes all but second house? 
  #(there are multiple options))
  makeseq('h',waves,'atota'),
  
  #current marital status: married/cohab (1-3), sep/div(4-6), widowed (7) nm (8)
  makeseq('r',waves,'mstat'),
  
  #work status (ft or pt, unemployed, retired, homemaker)
  #.A=presumed retired, .Q not asked, .T worked last 2 year: 1=FT,2=PT,3=UE,
  #4=part ret, 5=retired, 6=disabled, 7=not in labor force
  makeseq('r',waves,'lbrf'),
  
  #retirement plans rwrplnyr (plan) rwplnya (thinks)
  #per notes rwplnya is year thnks will stop working and is more comprehensive (taken from similar qs)
  #rwsayret (consideres self retired)
  makeseq('r',waves,'rplnyr'),
  makeseq('r',waves,'rplnya'),
  makeseq('r',waves,'sayret'),
  
  #retirement dates rwretyr and rwretmon
  makeseq('r',waves,'retyr'),
  makeseq('r',waves,'retmon'),
  
  #work full time, want, etc
  #cool risk aversion and related questions, too

  #region
  makeseq('r',waves,'cendiv'),
  
  #@@weights
  makeseq('r',waves,'wtresp')
)

#subset dataframe
subdat = subset(rawhrs,select=c(vars))

#remove raw data from memory and working directory
rm(rawhrs)
unlink("randpstata",recursive=TRUE)

#save raw subset in "output" directory
write.csv(subdat, file=paste0(outdir,'private~/subhrs.csv'))


#@@@@@@@@@@@@@@@@@@@@@
#Output "spot checks" for subset accuracy
#@@@@@@@@@@@@@@@@@@@@@

sink(paste(outdir,'private~/hrs-output-mean.txt',sep=''))
print(Sys.Date(),quote="F")
summary(subdat)  
sink()


#@@@@@@@@@@@@@@@@@@@@@
#Recode, subset to workable size, and transition to "long"
#1. build a wave-specific set and append
#@@@@@@@@@@@@@@@@@@@@@

#summarize variables and initialize clean dataset
#fixed
fvars = c('hhidpn','rahhidpn','rabyear','ragender','raedyrs','raracem','has_pgs')
#respondent change variables
rcvars = c('cendiv','cesd','mstat','drinkn','lbrf','work62','work65',
           'rplnyr','rplnya','sayret','retyr','retmon',
           'iwendm','iwendy','wtresp')

#household change variables
hcvars = c('itot','atota')

longdat = as.data.frame(setNames(replicate(length(c(fvars,rcvars,hcvars))+1,numeric(0),
                                           simplify=F),
                                 c(fvars,rcvars,hcvars,'wave')))


for(w in waves){
  
  print(paste('Constructing wave',w))
  vars = c(fvars,paste('r',w,rcvars,sep=''),paste('h',w,hcvars,sep=''))
  #print(vars)
  
  tempdat=subset(subdat,
                 select=vars)
  
  tempdat$wave=w
  colnames(tempdat)=colnames(longdat)
  
  #append for spot cheking
  sink(paste(outdir,'private~/hrs-output-mean.txt',sep=''),append=T)
  cat('\n\n@@@@@@@@@@@@@@@@@@@\nWAVE ')
  cat(w)
  cat(' changed to long\n@@@@@@@@@@@@@@@@@@@@@\n\n')
  print(summary(tempdat)) 
  sink()
  
  longdat=rbind(longdat,tempdat)
  
}

rm(subdat, tempdat)

#@@@@@@@@@@@@@@@@@@@@@
#Recode, subset to workable size, and transition to "long"
#2. recodes
#@@@@@@@@@@@@@@@@@@@@@

longdat$age=years[longdat$wave] - longdat$rabyear
longdat$male=longdat$ragender
longdat$male[longdat$ragender==2] = 0
longdat$black=NA
longdat$black[longdat$raracem %in% c(1,3)] = 0
longdat$black[longdat$raracem ==2] = 1
longdat$orace=NA
longdat$orace[longdat$raracem %in% c(1,2)] = 0
longdat$orace[longdat$raracem ==3] = 1
longdat$marr=NA
longdat$marr[longdat$mstat %in% c(1:2)] = 1
longdat$marr[longdat$mstat %in% c(3:8)] = 0
longdat$ret = NA
longdat$ret[longdat$lbrf == 4:5] = 1 #includes "partly retired, i.e. ret with pt job
longdat$ret[longdat$lbrf %in% c(1:3,6:7)] = 0
longdat$emp = NA
longdat$emp[longdat$lbrf %in% c(3:7)] = 0 
longdat$emp[longdat$lbrf %in% c(1:2)] = 1
longdat$unemp = NA
longdat$unemp[longdat$lbrf %in% c(1:2,4:5)] = 0
longdat$unemp[longdat$lbrf %in% c(3,6:7)] = 1

#prep ya recession cohorts based on age range
yarange = c(18,22)
recessions = read.csv(paste(outdir,'recession.csv',sep=''))
r_cohort = as.numeric(rep(NA,nrow(longdat)))
r_intens = as.numeric(rep(0,nrow(longdat))) #measure intensity of recession

for(i in 1:nrow(recessions)){
  brange = cbind(recessions[i,1] - max(yarange),recessions[i,2]-min(yarange))
  print(c(i, paste0(recessions[i,1],'-',recessions[i,2]), brange[1],brange[2]))
  r_cohort[longdat[,'rabyear'] >= brange[1] & longdat[,'rabyear'] <= brange[2]] = 1
  r_intens[longdat[,'rabyear'] >= brange[1] & longdat[,'rabyear'] <= brange[2]] = recessions[i,'length']/12
}

r_cohort[is.na(longdat[,1])==F & is.na(r_cohort) ==T] = 0

longdat= cbind(longdat,r_cohort,r_intens)
#colnames(longdat) = c(colnames(longdat),'r_cohort','r_intense')
rm(r_cohort,recessions,r_intens)

longdat$greatmod = NA #did person come of age during great moderation
longdat$greatmod[longdat$rabyear <= 1986] = 0
longdat$greatmod[longdat$rabyear > 1987] = 1

#@@@@@@@@@
#Add BLS unemployment data by region
#@@@@@@@@@

#CHANGE NOT IN US TO MISSING: very few
longdat$cendiv[longdat$cendiv == 11] = NA

load(paste0(outdir,'blsdat.RData'))
#blsdat = read.csv(paste0(outdir,"blsdat.csv"))

#initialize current month "cm" and prior monh "pm" unemployment rate "uer"
longdat$cm_uer = NA
longdat$pm_uer = NA

for(i in 1:nrow(longdat)){
  
  if(longdat[i,'iwendy'] > 2001 & longdat[i,'iwendy'] < 2013 
      & is.na(longdat[i,'iwendy'])==F
      & is.na(longdat[i,'iwendm'])==F
      & is.na(longdat[i,'cendiv'])==F
    ) {

    yr = longdat[i,'iwendy']
    m = longdat[i,'iwendm']
    pm = m-1
    r = longdat[i,'cendiv']
    missing = any(is.na(c(yr,m,pm,r)) == T)
    if(yr == 2012 & m == 12){missing = T}
    
    if (missing == F){
      
      longdat[i,'cm_uer'] = blsdat$value[
        blsdat$year == yr &
          blsdat$period == m &
          blsdat$region == r
        ][1]
      
      #update prior month and year for January interviews
      if(m==1){
        pm = 12
        yr = yr-1
      }
      
      
      longdat[i,'pm_uer'] = blsdat$value[
        blsdat$year == yr &
          blsdat$period == pm &
          blsdat$region == r
        ][1]
      if(i%%750 == 0){  print(c(i,m,yr,r,longdat[i,"cm_uer"],longdat[i,"pm_uer"]))}
      
    }
  }
}

#create recession indicator
longdat$recession = NA
  longdat$recession[longdat$iwendy >=2007 & longdat$iwendy <= 2009] = 1
  longdat$recession[longdat$iwendy < 2007 | longdat$iwendy > 2009] = 0

#create cohorts indicator (11 indicators dropping lowest and highest 2.5%)
cyrs = seq(1910,1960,by=5)
longdat$cohort = NA
longdat$cohort[longdat$rabyear<cyrs[1]] = 1

for(c in 1:length(cyrs)){
  longdat$cohort[longdat$rabyear>cyrs[c]] = c
}

#create cohort mean age
cohort_age = aggregate(longdat$age,by=list(longdat$cohort),FUN=function(x) mean(x,na.rm=T))
for(c in 1:length(cyrs)){
  longdat$cohort_age[longdat$cohort==c] = cohort_age$x[cohort_age$Group.1 ==c]
}  

rm(cohort_age)

#create individual mean and mean deviations for time-varying variables
#tvar = c('y',paste('x',1:length(x),sep=''))
#bugsdat[,paste('c',tvar,sep='')] = as.numeric(NA)
#for(i in 1:nrow(imean)){
#  for(j in tvar){
#    bugsdat[bugsdat['id']==i,paste('c',j,sep='')] = bugsdat[bugsdat['id']==i,j] - imean[imean['Group.1']==i,j]
#  }
#}
#cat('\n\tPrint individual and centered means for 5 random individuals to confirm coding.\n\n')
#rn=sample(unique(bugsdat$id))
#print(bugsdat[bugsdat$id %in% c(sample(unique(bugsdat$id),5)),c('id','age',tvar,paste('c',tvar,sep=''))])
#cat('\n\tProportion of individuals with change in time-varying dummy variables any time:\n')
#apply(imean[,c(5:ncol(imean))],2,function(j) 1-sum(j ==0 | j==1)/length(j))
#cat('\n\tProportion of observations with change on dep variable: ')
#print(1-sum(bugsdat$cy==0)/nrow(bugsdat))
#cat('\n\tProportion of individuals with change on dep variable across at least 1 wave: ')
#print(1-length(unique(bugsdat[bugsdat$cy==0,'id']))/length(unique(bugsdat$id)))


#print reports

sink(paste(outdir,'private~/hrs-output-mean.txt',sep=''),append=T)
cat('\n\n@@@@@@@@@@@@@@@\nCleaning of data\n@@@@@@@@@@@@@@@\n\n')
table(longdat$ragender,longdat$male,dnn=c('ragender','male'))
cat('\nRace dummy series (ref=white)\n')
table(longdat$raracem,longdat$orace,dnn=c('raracem','orace'))
cat('\n')
table(longdat$raracem,longdat$black,dnn=c('raracem','black'))
cat('\nMarried dummy\n')
table(longdat$mstat,longdat$marr,dnn=c('mstat','marr'))
cat('\n')
cat('\n Labor Force dummy series (inclusive)\n')
table(longdat$lbrf,longdat$emp,dnn=c('lbrf','ret'))
cat('\n')
table(longdat$lbrf,longdat$emp,dnn=c('lbrf','emp'))
cat('\n')
table(longdat$lbrf,longdat$unemp,dnn=c('lbrf','unemp'))
cat('\n')
cat('\n Recession Dummy (by year interveiew completed) \n')
table(longdat$recession,longdat$iwendy, dnn=c('recession','iwendy'))
cat('\n Cohort indicator (5 years by birth year) \n')
table(longdat$rabyear,longdat$cohort, dnn=c('rabyear','cohort'))
sink()
cat('\n Cohort mean age indicator (5 years by birth year) \n')
table(longdat$rabyear,longdat$cohort_age, dnn=c('rabyear','cohort_age'))
sink()

#@@@@@@@@@@@@@@@@@@@@@
#Recode, subset to workable size, and transition to "long"
#3. arbitrary limit to make size workable (can be removed for substantive anlaysis)
#@@@@@@@@@@@@@@@@@@@@@

#delete cases with no CESD info and delete original variables in favor of renamed variables
cleandat=longdat[is.na(longdat$cesd)==F,!(names(longdat) %in% c('ragender','mstat','lbrf'))]

#print overview of deletions
sink(paste(outdir,'cleanhrs-overview.txt',sep=''))
print(Sys.Date(),quote=F)
cat('\n                             \t\tPersons\t\tObservations')
cat('\nTotals                      ',length(unique(longdat$hhidpn)),nrow(longdat),sep='\t\t')
cat('\nMissing on DV                ',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
#iobs = data.frame(aggregate(cleandat$hhidpn, by=list(cleandat$hhidpn), length))
#cleandat=cleandat[cleandat$hhidpn %in% iobs[iobs[2]>4,1],]
#cat('\nIndividuals observed 5+ waves',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
#cleandat=cleandat[cleandat$orace==0,]
#cat('\nDelete Orace                ',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
#cleandat=cleandat[cleandat$rabyear==(2008-65),]
#cat('\nLimit 65 at recession (2008)',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
#create random subsample of 300 from remaining
#samp = sample(unique(cleandat$hhidpn),size=300,replace=F)
#cleandat=cleandat[cleandat$hhidpn %in% samp,]
#cat('\n300 random sample (person)  ',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
sink()

#@@@@@@@@@@@@@@@@@@@@@
#Write clean dataset
#@@@@@@@@@@@@@@@@@@@@@

#write.csv(cleandat,file=paste0(outdir,'private~/cleanhrs.csv'),na='.')
save(cleandat,file=paste0(outdir,'private~/cleandat.RData'))


#@@@@@@@@@@@@@@@@@@@@@
#output descriptive statistics for clean set
#@@@@@@@@@@@@@@@@@@@@@

#attach(cleandat)
agg=aggregate(cleandat%>%dplyr::select(-hhidpn,-rahhidpn),by=list(cleandat$wave), FUN=function(x) 
  c(mn=mean(x, na.rm=T),sdev=sd(x, na.rm=T),min=min(x,na.rm=T),max=max(x,na.rm=T),n=length(x),nmiss=sum(is.na(x)) ) )
#detach(cleandat)

sink(paste(outdir,'cleanhrs-overview.txt',sep=''),append=T)
cat('\n\nDescriptives\n')
print(round(t(agg),digits=2))
sink()

rm(agg,longdat)

print(proc.time()[3] - st)



