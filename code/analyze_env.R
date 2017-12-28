#this file analyzes
##environment test

rm(list=ls())

source('code/config~.R',echo=TRUE)

#load cleandat
#this data was cleaned using pre-hrs-panel.R
cleandat.f = paste0(outdir,'private~/cleandat.RData')
if(file.exists(cleandat.f)){
  load(cleandat.f)
} else{
  print('Cleanding HRS data.\n\n') 
  source('code/prep-hrs-panel.R',echo=TRUE)
}


#NOTES: can work up descriptively as necessary
#2002 + significant gxe interaction; 1993 - 2000 not much...
#>=2006 something different; extremes usually have difference
#<2006 opposite relationshipo with and u/e rate
#limited to recession 2006 to 2010, only low-end spectrum is okay
#prior to recession has big differences across the whole spectrum
#no effect outside of recession, except for unemployment (but marginal...)

analyze = cleandat %>% filter(iwstat==1) %>% #limit to in-wave respondents
  mutate(recession=ifelse(iwendy %in% c(2001,2007,2008,2009),1,0),
         lnwlth = log(atota + abs(min(atota,na.rm=TRUE)) + 1),
         lninc = log(itot + abs(min(itot,na.rm=TRUE))+ 1),
         negwlth = ifelse(atota<0,1,0)) 


######
#cross-sections hows efffect (positive) only with retirees
######

analyze$laborcat = factor(analyze$unemp + analyze$ret*2 + 1,
                          labels=c('emp','unemp','ret'))
print(table(analyze[,c('laborcat','unemp','ret')]))

cx = lapply(split(analyze, analyze[,'laborcat']),function(x)
  lm(cesd~pm_uer+iwendy+male+age+I(age^2)+marr+lnwlth+negwlth+lninc,data=x)
)

print(lapply(cx,summary))

####individual f/e; full dataset + HOUSMAN TEST
form = cesd~pm_uer+emp+ret+marr+age+lnwlth+negwlth+lninc

#not sure why 2012 isn't getting factored 
plm1 = plm(form, 
           data=analyze,index='hhidpn',model='within')
print(summary(plm1))

plm2 = plm(form, 
           data=analyze,index='hhidpn',model='random')
print(summary(plm2))

hausman = phtest(form,data=analyze,index='hhidpn')
print(hausman)

######
######measuring the deltas---small effects

analyze.panel = analyze %>% 
  group_by(hhidpn) %>%
  summarize(idrinkn=mean(drinkn,na.rm=TRUE),
            icesd = mean(cesd,na.rm=TRUE),
            iuer = mean(pm_uer, na.rm=TRUE),
            iret = mean(ret,na.rm=TRUE),
            iunemp = mean(unemp,na.rm=TRUE),
            iatota = mean(atota,na.rm=TRUE),
            iwlth = mean(lnwlth,na.rm=TRUE),
            inegwlth= mean(negwlth,na.rm=TRUE),
            iinc = mean(lninc,na.rm=TRUE),
            iitot = mean(itot, na.rm=TRUE)) %>%
  ungroup

analyze = merge(analyze,analyze.panel,by='hhidpn') %>%
  group_by(hhidpn) %>%
  mutate(md_drinkn = drinkn - idrinkn,
         md_cesd = cesd - icesd,
         md_uer = pm_uer - iuer,
         md_ret = ret - iret,
         md_unemp = unemp - iunemp,
         md_atota = atota - iatota,
         md_itot = itot - iitot,
         md_wlth = lnwlth-iwlth,
         md_negwlth= negwlth - inegwlth,
         md_inc = lninc - iinc) %>%
  ungroup

#Replication of GSS panel model (but using cesd, and log wealth and income)
#(need to add education!!)
hlm = lmer(cesd~r_cohort + r_intens +
             iuer + iret + iunemp + iwlth + inegwlth + iinc +
             md_uer+md_ret+md_unemp+md_wlth+md_negwlth+md_inc
           +factor(raracem)+
       factor(iwendy)+male+age + marr +
         (1|hhidpn),data=analyze)

print(summary(hlm))
hlm.sim = FEsim(hlm,n.sims=200)
yrs=hlm.sim
plotFEsim(hlm.sim)

####
#gene effects

#load and merge gene data
#african american set --12,270 combined
pgsdat_aa = read_dta('H:/Academic Projects/Data Files/HRS/PGENSCORE/PGENSCOREA_R.dta')
pgsdat_aa$black=1
#european set
pgsdat_e = read_dta('H:/Academic Projects/Data Files/HRS/PGENSCORE/PGENSCOREE_R.dta')
pgsdat_e$black=0

pgsdat = rbind(pgsdat_aa,pgsdat_e)

###for anitoly, print polygenic score correlations (raw) 

pgs = pgsdat %>% dplyr::select(starts_with('pgs'))
pgs = cor(pgs)
pgs.titles = c('General Cognition',
               'BMI',
               'Height',
               'Diastolic BP',
               'Systolic BP',
               'Mean Art. Pressure',
               'Pulse Pressure',
               'Depression (MDD)',
               'Schizophrenia',
               'Education',
               'Smoker',
               'Alzheimers',
               'Neuroticism',
               'Subjective Well-Being',
               'Waist Circumfrence',
               'Waist-to-Hip Ratio',
               'Depressive Symptoms'
)

colnames(pgs) = rownames(pgs) = pgs.titles
mcors = melt(pgs)

corplot=  qplot(x=Var2,y=ordered(Var1,levels=rev(sort(unique(Var1)))),
                data= mcors)  + 
  geom_tile(aes(fill=value)) +
  scale_fill_gradient2(limits=c(-1,1),low=muted('red'),high=muted('blue'),mid='white',midpoint=0) +
  theme(axis.line=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title='Correlations of Unadjusted Polygenetic Scores Provided by HRS.',
       caption=paste0('All Observations, n=',nrow(pgsdat)))

print(corplot)

ggsave(paste0(outdir,'pgs_correlations.pdf'))


colnames(pgsdat) = tolower(colnames(pgsdat))
pgsdat$rahhidpn = paste0(pgsdat$hhid,pgsdat$pn)

print(dim(analyze))
print(table(analyze$has_pgs))

adjpgs = function(pgs,pc,df){
  #This function returns pc adjusted polygenetic scores.
  #
  #Input: pgs=polygenetic score; character 
  #       pc=list of principle components; character
  #       df = dataframe; object name
  #
  #Output: Standardized residuals as in Price et al 2006 (nature genetics)
  
  frm = as.formula(paste0(pgs,'~',paste0(pc,collapse='+')))
  
  res = residuals(lm(frm,data=df))
  
  res=(res-mean(res))/sd(res)
  return(res)
  
}

pcs = colnames(pgsdat)[3:12]

#limit to nonhispanic whites
pgsdat=pgsdat %>% filter(black==0)

#correlations are close to 1; i.e. .97
pgsdat$pgs.symp = adjpgs('pgs_depsymp_ssgac16',pcs,pgsdat)
pgsdat$pgs.mdd = adjpgs('pgs_mdd_pgc13',pcs,pgsdat)
pgsdat$pgs.swb = adjpgs('pgs_pgs_wellb_ssgac16',pcs,pgsdat)
pgsdat$pgs.neu = adjpgs('pgs_pgs_neuro_ssgac16',pcs,pgsdat)

######
#pgsdat$pgs.swb1 = pgsdat$pgs.swb
#pgsdat$pgs.swb = pgsdat$pgs.symp ### same analysis but with depressive symptoms...


analyze = merge(analyze,pgsdat,by='rahhidpn',all.x=TRUE,all.y=FALSE)

print(dim(analyze))
print(table(analyze$has_pgs))

#limit to nonhispanic whites
analyze=analyze %>% filter(!is.na(analyze$version) &
                             analyze$raracem==1)

#####
#interaciton with pgs 

hlm.gxe = lmer(cesd~(r_cohort + r_intens):pgs.swb +
             (iuer + iret + iunemp + iwlth + inegwlth + iinc):pgs.swb +
             (md_uer+md_ret+md_unemp+md_wlth+md_negwlth+md_inc):pgs.swb +
             iwendy+male+age + marr +
             (1|hhidpn),data=analyze)

print(summary(hlm.gxe))
hlm.sim = FEsim(hlm.gxe,n.sims=200)
yrs=hlm.sim
plotFEsim(hlm.sim)

#######
#descriptive table


######
#some descriptive plots

uetrend = ggplot(analyze,aes(x=iwendy,y=pm_uer)) +
  geom_line(stat='summary',fun.y='mean',size=2) +
  geom_point(aes(x=(iwendy+iwendm/12)),alpha=0.01) +
  theme_classic() +
  labs(title='Observed Unemployment Rates and Mean',
       caption='U/E from the month prior to interview.') +
  xlab('') + ylab('Regional Unemployment Rate.')

print(uetrend)

ggsave(paste0(outdir,'uetrend.pdf'))


#####
#reproduce plot for presentation
ggplot(analyze,aes(x=iwendy,y=pm_uer)) +
  geom_line(stat='summary',fun.y='mean',size=2,color='white') +
  geom_point(aes(x=(iwendy+iwendm/12)),color='grey',alpha=0.1) +
  theme_present() +
  labs(caption='U/E from the month prior to interview.') +
  xlab('') + ylab('Regional Unemployment Rate.')

ggsave(paste0(draftimg,'uetrend.png'),
       bg='transparent',
       height=5.5,width=12)

#CESD trends (very little)

ggplot(analyze,aes(x=iwendy,y=cesd)) +
  geom_line(stat='summary',fun.y='mean',size=2) +
  geom_point(aes(x=(iwendy+iwendm/12)),alpha=0.01) +
  theme_classic()

mlts = melt(analyze[,c('md_uer','iuer','pgs.swb','iret','md_ret','iunemp','md_unemp',
                       'iatota','md_atota','iitot','md_itot')])

ggplot(mlts,aes(x=value)) +
  geom_histogram() +
  facet_wrap(~variable,scales='free',nrow=2) +
  theme_classic()

ggsave(paste0(outdir,'misc_hist.pdf'))

#######
#more descriptives

#id pgs quartiles (for plotting bivariate relationships)
analyze$pgs.quart = cut(analyze$pgs.swb,quantile(analyze$pgs.swb,
                                                 probs=seq(0,1,by=0.25),
                                                 na.rm=TRUE))

analyze$pgs.quart5 = cut(analyze$pgs.swb,quantile(analyze$pgs.swb,
                                                 probs=seq(0,1,by=0.2),
                                                 na.rm=TRUE))

analyze$md_uer.quart10 = cut(analyze$md_uer,quantile(analyze$md_uer,
                                                   probs=seq(0,1,by=0.1),
                                                   na.rm=TRUE))

analyze$md_uer.quart = cut(analyze$md_uer,quantile(analyze$md_uer,
                                                   probs=seq(0,1,by=0.25),
                                                   na.rm=TRUE))
analyze$uer_up = analyze$md_uer>0

#mean cesd by pgs quantile
ianalyze = analyze %>% 
  group_by(hhidpn) %>%
  summarize(cesd = mean(cesd,na.rm=TRUE),
            pgs.quart5 = mean(as.numeric(pgs.quart5),na.rm=TRUE)) %>%
  ungroup

ianalyze = ianalyze %>%
  group_by(pgs.quart5) %>%
  summarize(n=sum(!is.na(cesd)),
            prop_cesd = sum(cesd>1)/n,
            sd_prop = sqrt((prop_cesd*(1-prop_cesd))/n)) %>%
  ungroup %>%
  filter(!is.na(pgs.quart5))
  
#cesd change from mean by pgs quantile
imeans = ggplot(ianalyze,aes(x=pgs.quart5,y=prop_cesd)) +
  geom_bar(stat='identity',fill='grey') +
  geom_errorbar(aes(ymax=prop_cesd + 1.34*sd_prop,ymin=prop_cesd - 1.34*sd_prop),width=0) +
  theme_classic() + xlab('PGS Quintile') + ylab('Proportion with Individual Mean CESD > 1')

print(imeans)
ggsave(paste0(draftimg,'indiv_pgs.pdf'))

#####
#observation level
subanalyze = analyze %>%
  group_by(pgs.quart5,uer_up) %>%
  summarize(mean_cesd = mean(md_cesd,na.rm=TRUE),
            prop_cesd = mean(md_cesd>0,na.rm=TRUE),
            n = sum(!is.na(md_cesd)),
            sd_cesd = sd(md_cesd,na.rm=TRUE),
            sd_prop = sqrt((prop_cesd*(1-prop_cesd))/sum(!is.na(md_cesd))))

subanalyze = subanalyze[complete.cases(subanalyze),]

deltas=subanalyze %>% 
  group_by(pgs.quart5) %>%
  summarize(max=max(prop_cesd),
            min=min(prop_cesd),
            ps=sum(prop_cesd*n)/sum(n),
            pd = abs(diff(prop_cesd)),
            se=sqrt(ps*(1-ps)/sum(n)),
            z=pd/se,
            pval=(1-pnorm(abs(z)))*2,
            star=sig(pval))

deltaviz = ggplot(subanalyze) +
  geom_line(aes(x=pgs.quart5,y=prop_cesd,group=uer_up),alpha=0.2) +
  geom_point(aes(x=pgs.quart5,y=prop_cesd,group=uer_up,shape=uer_up),size=2) +
  geom_text(data=deltas,aes(x=pgs.quart5,y=max-pd/2,label=star)) +
  theme_classic() +
  ylab('Proportion with Increasing CESD') + xlab('PGS Quintile') +
  scale_x_discrete(labels=paste0('Q',c(1:5))) +
  scale_shape_discrete(labels=c('Stable or Declining U/E','Increasing U/E')) +
  guides(shape=guide_legend(title=NULL)) +
  theme(legend.position='bottom')
  
print(deltaviz)
ggsave(paste0(draftimg,'prop_deltas.pdf'))

#rearrange deltaviz for presentation

ggplot(subanalyze) +
  geom_line(aes(x=pgs.quart5,y=prop_cesd,group=uer_up),size=0.75,color='grey',alpha=0.5,lty=2) +
  geom_point(aes(x=pgs.quart5,y=prop_cesd,group=uer_up,shape=uer_up),size=5,color='white') +
  geom_text(data=deltas,aes(x=pgs.quart5,y=max-pd/2,label=star),size=7,color='white') +
  theme_present() +
  ylab('Proportion with Increasing CESD') + xlab('PGS Quintile') +
  scale_x_discrete(labels=paste0('Q',c(1:5))) +
  scale_shape_discrete(labels=c('Stable or Declining U/E','Increasing U/E')) +
  guides(shape=guide_legend(title=NULL)) +
  theme(legend.position='bottom')

ggsave(paste0(draftimg,'deltaviz.png'),
       bg='transparent',
       height=5.5,width=12)

######
#ENVIRONMENTAL FIXED EFFECT

plm.e = plm(cesd~age + iwendy + (ret + unemp)*pm_uer + lnwlth + lninc + negwlth + marr ,
            index='hhidpn',model='within',data=analyze)

res.e = as.data.frame(coef(summary(plm.e)))
res.e$effname = factor(row.names(res.e))
levels(res.e$effname) = c('Age','Log Inc.','Log Wealth',
                          'Married','Neg. Wealth','U/E',
                          'Retired','RetiredxUE','Unemp.','Unemp.xU/E')
colnames(res.e) = c('est','se','tv','pval','effname')

FE.env = ggplot(res.e,aes(y=est,x=effname,alpha=pval<0.05)) + 
  geom_point(size=2) +
  geom_abline(slope=0,intercept=0) +
  geom_errorbar(aes(ymax=est+1.96*se,ymin=est-1.96*se),width=0,size=1.25) +
  theme_classic() + 
  scale_x_discrete(limits=rev(levels(res.e$effname))) +
  coord_flip() + ylab('') + xlab('')

print(FE.env)
ggsave(paste0(draftimg,'fe.pdf'))

###men and women are same drinking and depression! BUT mdd is different
###drinking is more sensitive to unemp, and males with higher MDD drink more
#should change to include spouse's employment status... for drinking

#can interperet pgs.swb as an **adaptation** gene... 
#(if you include iwendy in the fixed effect, and if you include iuer in random effects
#UNLESS it's mortality selectivity...

#note that the PGS interaction effect changes over time, i.e. disappears closer to recession
#taking out iwendy and regressing 2002+ (after .com bust) shows more sensitivity of high pgs.swb
#to unemployment rate

plm.gxe = plm(cesd~age + (ret + unemp)*pm_uer + marr + iwendy +
                (ret+unemp)*pm_uer + lninc+lnwlth+negwlth +
                (pm_uer + (ret+unemp)*pm_uer + iwendy):pgs.swb,
           index='hhidpn',model='within',data=analyze)

###

res.gxe = as.data.frame(coef(summary(plm.gxe)))
res.gxe$effname = factor(row.names(res.gxe))
#levels(res.gxe$effname) = c('Age','Log Inc.','Log Wealth',
#                          'Married','Neg. Wealth','U/E',
#                          'Retired','RetiredxUE','Unemp.','Unemp.xU/E')
colnames(res.gxe) = c('est','se','tv','pval','effname')

FE.gxenv = ggplot(res.gxe,aes(y=est,x=effname,alpha=pval<0.05)) + 
  geom_point(size=2) +
  geom_abline(slope=0,intercept=0) +
  geom_errorbar(aes(ymax=est+1.96*se,ymin=est-1.96*se),width=0,size=1.25) +
  theme_classic() + 
  scale_x_discrete(limits=rev(levels(res.gxe$effname))) +
  coord_flip() + ylab('') + xlab('')

print(FE.gxenv)

ggsave(paste0(draftimg,'FExgene.pdf'))


#####
#plot for presentation

ggplot(res.gxe[grep('U/E',res.gxe$effname),],
       aes(y=est,x=effname,color=pval<0.05)) + 
  geom_point(size=2) +
  geom_abline(slope=0,intercept=0,color='white') +
  geom_errorbar(aes(ymax=est+1.96*se,ymin=est-1.96*se),width=0,size=1.25) +
  theme_present() + 
  scale_color_brewer('p<0.05',palette='Blues',direction=-1) +
  #scale_x_discrete(limits=
  #                   rev(levels(res.gxe$effname)[grep('U/E',levels(res.gxe$effname))])) +
  coord_flip() + ylab('') + xlab('')

ggsave(paste0(draftimg,'FExgene.png'),
       bg='transparent',
       height=5.5,width=12)


#######
#sliced by selected wave
wv=7
analyze = analyze %>%
  mutate(period = ifelse(wave>=wv,paste(wv,'+'),paste(wv,'-')))


plm.gxe2 = lapply(split(analyze,analyze[,'period']), function(x)
  plm(cesd~age + (ret + unemp)*pm_uer + marr +
                (ret+unemp)*pm_uer + lnwlth + lninc + negwlth +
                iwendy + (pm_uer + (ret+unemp)*pm_uer):pgs.swb,
              index='hhidpn',model='within',data=x)
)

mktab = function(mod){
  tab = as.data.frame(coef(summary(mod)))
  tab$se = paste0('(',rnd(tab$`Std. Error`),')')
  tab$sig = sig(tab$`Pr(>|t|)`)
  return(tab %>% select(Estimate,se,sig))
}

###the effect of both pgs and retirment changes...
yr=min(analyze$iwendy[analyze$wave==wv])

plm.split = kable(do.call(cbind,lapply(plm.gxe2,mktab)),
                  caption=paste0('Split to before',
                                 yr,
                                 'and including and after',
                                 yr,
                                 '.'))

#######
#plot

res.gxe2 = do.call(rbind,lapply(plm.gxe2,function(x)
                                as.data.frame(coef(summary(x)))))
colnames(res.gxe2) = c('est','se','tv','pval')
res.gxe2$effname = factor(substr(rownames(res.gxe2),start=5,stop=100))
res.gxe2$period = substr(rownames(res.gxe2),start=1,stop=3)

res.gxe2$alpha = cut(1-res.gxe2$pval,
                     breaks=c(1,.999,.99,.95,.9,0),
                     labels=rev(c('0.001','0.01','0.05','0.1','1')))

gxe = grepl('swb',levels(res.gxe2$effname))
sel = levels(res.gxe2$effname)[gxe]
res.gxe2 = res.gxe2 %>% 
  filter(effname %in% sel)

res.gxe2$effname = factor(res.gxe2$effname,
                          exclude=levels(res.gxe2$effname)[!gxe])

levels(res.gxe2$effname) = c('U/E Rate','Retirement','Retirement x U/E',
                             'Unemp','Unemp x U/E')

FE.gxenv2 = ggplot(res.gxe2,
                   aes(y=est,x=effname,
                       alpha=alpha,shape=period)) + 
  geom_abline(slope=0,intercept=0) +
  geom_errorbar(aes(ymax=est+1.96*se,ymin=est-1.96*se),width=0,size=1.25) +
  geom_point(size=2,alpha=1) +
  theme_classic() + 
  scale_x_discrete(limits=rev(levels(res.gxe2$effname))) +
  coord_flip() + ylab('') + xlab('') +
  scale_shape_manual('',values=c(0,1),
                     labels=c(paste(min(analyze$iwendy),'to',yr-1),
                              paste(yr,'to',max(analyze$iwendy)))
                     ) +
  scale_alpha_discrete('p<',
                       range=c('0.1','0.5','0.9')) +
  guides(alpha = guide_legend(override.aes = list(linetype=1))) +
  labs(title='Interaction with PGS for SWB.')

print(FE.gxenv2)


######
#work62

wk = analyze %>% group_by(hhidpn) %>%
      mutate(iwork65 = mean(work65, na.rm=TRUE),
             md_work65 = work65 - iwork65,
             irplnya = mean(rplnya,na.rm=TRUE),
             md_rplnya= rplnya - irplnya) %>%
      ungroup

hist(wk$md_work65)

plm.gxe3 = plm(drinkn~age + pm_uer + marr +
        + lnwlth + lninc + negwlth +
        iwendy + rplnya + (iwendy + pm_uer + rplnya):pgs.swb,
      index='hhidpn',model='within',
      data=analyze %>% 
        filter(unemp==0 & ret==0)) 

summary(plm.gxe3)

#note that NEU has most effect on rcohort/rintens ...
###this works just fine! --- need to fix to log income/etc

#######
#model from r_study ---- factors test

analyze = analyze %>% 
  group_by(hhidpn) %>%
  mutate_at(vars(c('lninc','lnwlth','negwlth')),
            funs(i=mean(.,na.rm=TRUE),
                 md=.-mean(.,na.rm=TRUE))) %>%
  ungroup
  
hlm.fac = lmer(cesd~(iuer + md_uer + factor(r_intens) + recession) +
                  male  + raedyrs + age +
                  marr + lninc_i + lnwlth_i + negwlth_i +
                  lninc_md + lnwlth_md + negwlth_md +
                  md_unemp + md_ret + iunemp + iret +
                  (1|hhidpn),
                data=analyze %>% filter(rabyear>1920) )

print(summary(hlm.fac))
hlm.sim = FEsim(hlm.fac,n.sims=200)
yrs=grepl('factor',hlm.sim$term)
hlm.sim=hlm.sim[yrs,]
hlm.sim$term = droplevels(hlm.sim$term)
plt = plotFEsim(hlm.sim) + 
  scale_x_discrete(limits=levels(hlm.sim$term))
print(plt)

#######
#model from r_study -- excluding great depression

hlm.gxed = lmer(cesd~(iuer + md_uer + r_cohort + r_intens + recession):pgs.swb +
                 male  + raedyrs + age +
                 marr + lninc_i + lnwlth_i + negwlth_i +
                 lninc_md + lnwlth_md + negwlth_md +
                 md_unemp + md_ret + iunemp + iret +
                 (1|hhidpn),
               data=analyze %>% filter(rabyear>1920) )

print(summary(hlm.gxed))
hlm.sim = FEsim(hlm.gxed,n.sims=200)
plt = plotFEsim(hlm.sim) + 
  scale_x_discrete(limits=rev(hlm.sim$term))
print(plt)



####
#pgs plot for presentation

ggplot(analyze,aes(x=pgs.swb)) + 
  geom_histogram(fill='white') +
  theme_present() + 
  xlab('Risk Score') + 
  ylab('Count')

ggsave(paste0(draftimg,'pgs-swb.png'),
       bg='transparent',
       height=5,width=5)






###
#survival analysis

library(survival)

#per codebook, rand uses sas dates, i.e.
#number of days since 1/1/1960
sasdate=as.Date('1960-1-1')


#limit to individual, and calculate times
analyze.s = analyze %>%
  group_by(hhidpn) %>%
  summarize(birthd = sasdate + mean(rabdate,na.rm=TRUE),
         intvd = max(as.Date(paste0(iwendy,'-',iwendm,'-15')),na.rm=TRUE), #impute as to the 15th of the month
         deathd = sasdate + mean(randate,na.rm=TRUE),
         time = max(c(intvd,deaths),na.rm=TRUE), #last follow-up
         dead = mean(!is.na(randate)),
         pgs.swb = mean(pgs.swb,na.rm=TRUE),
         male = mean(male, na.rm=TRUE)) %>%
  ungroup

###
#prepare kaplan-meyer curves



#p1=plm(cesd~pm_uer+marr+age,data=analyze,index='hhidpn',model='within')
#p2=plm(cesd~pm_uer+pm_uer:pgs.swb+marr+age,data=analyze,index='hhidpn',model='within')

#predicts marriage!
#summary(glm(marr~symp,data=analyze,family=binomial(link='logit')))

#save space for loading
save.image('analyze_env~.RData')
