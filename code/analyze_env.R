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

library(ggplot2);  library(reshape2); library(haven); library(scales)
library(lme4); library(plm); library(merTools); library(knitr); library(dplyr)

#load plotting theme for b-w presentation style--
source('H:/projects/proposal/r_study/code/themes.R',echo=TRUE)

#NOTES: can work up descriptively as necessary
#2002 + significant gxe interaction; 1993 - 2000 not much...
#>=2006 something different; extremes usually have difference
#<2006 opposite relationshipo with and u/e rate
#limited to recession 2006 to 2010, only low-end spectrum is okay
#prior to recession has big differences across the whole spectrum
#no effect outside of recession, except for unemployment (but marginal...)

analyze = cleandat %>% 
  mutate(recession=ifelse(iwendy %in% c(2001,2007,2008,2009),1,0),
         lnwlth = log(atota + abs(min(atota)) + 1),
         lnincome = log(itot + abs(min(itot))+ 1),
         negwlth = ifelse(aotota<0,1,0)) #%>%
  #filter(iwendy<=2006)
  #filter(iwendy>=2006)
  #filter(iwendy >= 2006 & iwendy < 2010)
  #filter(iwendy>2002)

######
#cross-sections hows efffect (positive) only with retirees
######
#props

colSums(analyze[,c('emp','unemp','ret')], na.rm=TRUE)/nrow(analyze)

cx.ret = lm(cesd~pm_uer+
             factor(iwendy)+male+age + 
             I(age^2) + 
             marr + atota + itot,dat=analyze %>% filter(ret==1))

print(summary(cx.ret))

cx.emp = lm(cesd~pm_uer+
             factor(iwendy)+male+age + 
             I(age^2) +
             marr + atota + itot,dat=analyze %>% filter(emp==1))

print(summary(cx.emp))

cx.unemp = lm(cesd~pm_uer+
              factor(iwendy)+male+age + 
              I(age^2) +
              marr + atota + itot,dat=analyze %>% filter(unemp==1))
print(summary(cx.unemp))


####individual f/e
form = cesd~pm_uer+marr+age+atota+itot

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

#you do this enough across contexts: make a function....


analyze.panel = analyze %>% 
  group_by(hhidpn) %>%
  summarize(idrinkn=mean(drinkn,na.rm=TRUE),
            icesd = mean(cesd,na.rm=TRUE),
            iuer = mean(pm_uer, na.rm=TRUE),
            iret = mean(ret,na.rm=TRUE),
            iunemp = mean(unemp,na.rm=TRUE),
            iatota = mean(atota,na.rm=TRUE),
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
         md_itot = itot - iitot) %>%
  ungroup




#basic
hlm = lmer(cesd~pm_uer+factor(raracem)+
       factor(iwendy)+male+age + 
       I(age^2) + unemp + ret +
       marr + itot + atota + (1|hhidpn),dat=analyze)

print(summary(hlm))
hlm.sim = FEsim(hlm,n.sims=200)
yrs=hlm.sim
plotFEsim(hlm.sim)

#print('Scope of Effect')
#print(0.0402021*range(analyze$md_uer,na.rm=TRUE))

hlm2 = lmer(cesd~iuer+iret+iunemp +factor(raracem)+
               md_uer+md_ret+md_unemp +
               iatota + iitot + 
               md_atota + md_itot +
               factor(iwendy) + male + age + 
               I(age^2) + marr + 
               (1|hhidpn),
             data=analyze)

print(summary(hlm2))
hlm2.sim = FEsim(hlm2,n.sims=200)
yrs=grepl('factor',hlm2.sim$term)
plotFEsim(hlm2.sim[!yrs,])

analyze$r_intens.f = as.factor(analyze$r_intens)
levels(analyze$r_intens.f) = strtrim(levels(analyze$r_intens.f),3)

"
#recession exposures
hlm2a = lmer(cesd~r_intens.f*iuer+iret+iunemp + 
               r_intens.f+md_uer+md_ret+md_unemp + 
               factor(iwendy) + male + age + 
               I(age^2) + marr + 
               (1|hhidpn),
             data=analyze)

#rcohort baseline benefit; benefit to unemployment (definition in the sample?)
#people are more sensitive higher average unemployment rates
#equal to 5 percentage points! (need to separate great depression, though)

hlm2a = lmer(cesd~r_cohort*(iuer+iret+iunemp) +
               r_cohort*(md_uer+md_ret+md_unemp) +
               factor(iwendy) + male + age + 
               I(age^2) + marr + 
               (1|hhidpn),
             data=analyze)

#replicates gss model...i think
hlm2aa= lmer(cesd~r_cohort+r_intens+iuer+iret+iunemp +
               md_uer+md_ret+md_unemp +
               factor(iwendy) + male + age + 
               I(age^2) + marr + 
               (1|hhidpn),
             data=analyze)

#generate new data for prediction interval
mns = dplyr::select(analyze,iuer,iret,iunemp,md_uer,md_ret,md_unemp,iwendy,male,age,marr) %>%
    summarize_each(funs(mean(.,na.rm=TRUE)))

ndat = data.frame(r_intens=(5:35)/10,
                  r_cohort=1)
ndat=rbind(ndat,data.frame(r_intens=0,r_cohort=0))

ndat = cbind(ndat,
            mns[rep(1,nrow(ndat)),])
ndat$iwendy=2007
ndat$hhidpn = unlist(rep(analyze[1,'hhidpn'],nrow(ndat)))

pred=predictInterval(hlm2aa,ndat,
  which='fixed',n.sims=2000)

pred$r_intens = ndat$r_intens

#intervals are wrong per me and Scott's paper... interested in the delta...
ggplot(pred,aes(x=r_intens,y=fit)) + 
  geom_point() +
  geom_linerange(aes(ymin=lwr,ymax=upr),alpha=0.25) +
  geom_hline(yintercept=pred$fit[pred$r_intens==0],lty=2)


             
hlm2a.sim = FEsim(hlm2a,n.sims=500)
rc=grepl('r_',hlm2a.sim$term)
ue=grepl('uer',hlm2a.sim$term)
plotFEsim(hlm2a.sim[ue,])
"

#interactions not significant
hlm2b = lmer(cesd~iuer*(iret+iunemp) +factor(raracem)+
              md_uer*(md_ret+md_unemp) +
              factor(iwendy) + male + age + 
              I(age^2) + marr + iatota + iitot + 
               md_atota + md_itot +
              (1|hhidpn),
            data=analyze)

hlm2b.sim=FEsim(hlm2b,n.sims=200)
interact=grepl('ue',hlm2b.sim$term)
yr = grepl('iwendy',hlm2b.sim$term)
#plotFEsim(hlm2b.sim[interact,])
plotFEsim(hlm2b.sim[!yr,])

hlm2bs = data.frame(coef(summary(hlm2b)))
hlm2bs$effname = row.names(hlm2bs)
hlm2bs = mutate_at(hlm2bs,vars(Estimate,Std..Error,t.value),funs(rnd(.)))

sink(paste0(outdir,'env-eff.md'))
  kable(hlm2bs)
sink()

#print(summary(hlm2b))

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


####analyze gxe interaction
analyze=analyze %>% filter(!is.na(analyze$version) &
                             analyze$raracem==1)


View(analyze %>% 
       group_by(iwendy) %>%
       summarize(bcohort=mean(rabyear,na.rm=TRUE),
                 rexp = mean(r_cohort,na.rm=TRUE),
                 rinten = mean(r_intens,na.rm=TRUE),
                 age = mean(age,na.rm=TRUE)))

######
#some plots

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
#descriptives

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
  group_by(pgs.quart5,uer_up,recession) %>%
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
#fe tests

plm(cesd~age + (ret + unemp)*pm_uer + marr ,
    index='hhidpn',model='within',data=analyze)

plm.e = plm(cesd~age + (ret + unemp)*pm_uer + marr ,
            index='hhidpn',model='within',data=analyze)

plm.a = plm(cesd~pm_uer + age + ret + unemp + marr + atota + itot,
            index='hhidpn',model='within',data=analyze)
summary(plm.a)

#plm.r = plm(cesd~pm_uer + recession + ret + unemp + marr + atota + itot,
#            index='hhidpn',model='within',data=analyze)
#summary(plm.r)

#plm.rxg = plm(cesd~pgs.swb*(pm_uer + recession) + ret + unemp  + marr + atota + itot,
#            index='hhidpn',model='within',data=analyze)
#summary(plm.rxg)


res.e = as.data.frame(coef(summary(plm.e)))
res.e$effname = factor(row.names(res.e))
levels(res.e$effname) = c('Age','Married','U/E','Retired','RetiredxUE','Unemp.','Unemp.xU/E')
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

plm.gxe = plm(cesd~age + (ret + unemp)*pm_uer + marr +
                (pm_uer + (ret+unemp)*pm_uer):pgs.swb,
           index='hhidpn',model='within',data=analyze)

###

res.gxe = as.data.frame(coef(summary(plm.gxe)))
res.gxe$effname = c('Age','Retired','Unemployed',
                    'U/E','Married','U/ExRet',
                    'U/ExUnemp','U/ExPGS','RetxPGS',
                    'UnepxPGS','U/ExRetxPGS','U/ExUnempxPGS')
res.gxe$effname = factor(res.gxe$effname) 
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

ggplot(res.gxe[grep('U/E',res.gxe$effname),],aes(y=est,x=effname,color=pval<0.05)) + 
  geom_point(size=2) +
  geom_abline(slope=0,intercept=0,color='white') +
  geom_errorbar(aes(ymax=est+1.96*se,ymin=est-1.96*se),width=0,size=1.25) +
  theme_present() + 
  scale_color_brewer('p<0.05',palette='Blues',direction=-1) +
  scale_x_discrete(limits=
                     rev(levels(res.gxe$effname)[grep('U/E',levels(res.gxe$effname))])) +
  coord_flip() + ylab('') + xlab('')

ggsave(paste0(draftimg,'FExgene.png'),
       bg='transparent',
       height=5.5,width=12)

######
#regression analysis
env.mod = lmer(cesd~iuer + md_uer +
       iret + md_ret + iunemp + md_unemp +
       male + age + 
       I(age^2) + male + marr + iatota + iitot + 
       md_atota + md_itot +
       (1|hhidpn),
     data=analyze )




print(summary(env.mod))
hlm.sim = FEsim(env.mod,n.sims=200)
levels(hlm.sim$term)[c(7,14)] = c('Between Person U/E','Within Person U/E')
#sel=grepl('uer',hlm.sim$term)
plotFEsim(hlm.sim[2:3,]) + ylab('') + xlab('') +
  ylim(-0.3,0.3) +
  labs(caption='With all controls:gender, age (squared), marital status, HH income, and HH wealth.')

ggsave(paste0(outdir,'env_eff.pdf'))

plotFEsim(hlm.sim)
ggsave(paste0(outdir,'fullenv.pdf'))

ge.mod = lmer(cesd~(iuer + md_uer)*pgs.swb +
                 iret + md_ret + iunemp + md_unemp +
                  male + age + 
                 I(age^2) + marr + iatota + iitot + 
                 md_atota + md_itot +
                 (1|hhidpn),
               data=analyze)


print(summary(ge.mod))
hlm.sim = FEsim(ge.mod,n.sims=200)
sel=grepl('uer|pgs',hlm.sim$term)
levels(hlm.sim$term)[c(7,8,15,16,18)] = c('Between Person U/E',
                                          'Between Person U/E x PGS',
                                          'Within Person U/E',
                                          'Within Person U/E x PGS',
                                          'PGS (SWB)')
plotFEsim(hlm.sim[sel,]) + ylab('') + xlab('') +
  ylim(-0.3,0.3) +
  labs(caption='With all controls:gender, age (squared), marital status, HH income, and HH wealth.')

ggsave(paste0(outdir,'gxe_eff.pdf'))

plotFEsim(hlm.sim)
ggsave(paste0(outdir,'fullgxe.pdf'))

######
#regression analysis

hlm.env = lmer(cesd~iuer + r_cohort + r_intens +
               md_uer +
               factor(iwendy) + male + age + 
               I(age^2) + marr + iatota + iitot + 
               md_atota + md_itot +
               (1|hhidpn),
             data=analyze %>% filter(r_intens<3))

print(summary(hlm.env))
hlm.sim = FEsim(hlm.env,n.sims=200)
yrs=grepl('factor',hlm.sim$term)
plotFEsim(hlm.sim[!yrs,])


hlm.enva = lmer(md_cesd~iuer + r_cohort + r_intens +
                 md_uer*(unemp+ret) +
                 factor(iwendy) + male + age + 
                 I(age^2) + marr + iatota + iitot + 
                 md_atota + md_itot +
                 (1|hhidpn),
               data=analyze %>% filter(r_intens<3))

print(summary(hlm.enva))


hlm.gxe = lmer(cesd~(iuer + md_uer+r_cohort+r_intens)*pgs.swb+
                male + age + 
                 I(age^2) + marr + iatota + iitot + 
                 md_atota + md_itot +
                 (1|hhidpn),
               data=analyze )

print(summary(hlm.gxe))
hlm.sim = FEsim(hlm.gxe,n.sims=200)
yrs=grepl('factor|marr|male|age',hlm.sim$term)
tots=grepl('tot',hlm.sim$term)
plotFEsim(hlm.sim[!yrs,])
plotFEsim(hlm.sim[tots,])

hlm.envd = lmer(md_cesd~(iuer + md_uer + r_cohort + r_intens) +
                  factor(iwendy) + male + age + 
                  I(age^2) + marr + iatota + iitot + 
                  md_atota + md_itot +
                  (1|hhidpn),
                data=analyze %>% filter(r_intens<3))

print(summary(hlm.envd))
hlm.sim = FEsim(hlm.envd,n.sims=200)
yrs=grepl('factor',hlm.sim$term)
plotFEsim(hlm.sim[!yrs,])

#note that NEU has most effect on rcohort/rintens ...

hlm.gxed = lmer(cesd~(iuer + md_uer + r_cohort + r_intens + recession)*pgs.swb +
                  male + age + 
                   marr + iatota + iitot + 
                  md_atota + md_itot + md_unemp + md_ret + iunemp + iret +
                  (1|hhidpn),
                data=analyze %>% filter(r_intens<3))

print(summary(hlm.gxed))
hlm.sim = FEsim(hlm.gxed,n.sims=200)
yrs=grepl('factor',hlm.sim$term)
plt = plotFEsim(hlm.sim[!yrs,])
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


#######
#model from r_study

###
#survival analysis

library(survival)

#load cleandat and use iwstat to identify wave of death...


#p1=plm(cesd~pm_uer+marr+age,data=analyze,index='hhidpn',model='within')
#p2=plm(cesd~pm_uer+pm_uer:pgs.swb+marr+age,data=analyze,index='hhidpn',model='within')

#predicts marriage!
#summary(glm(marr~symp,data=analyze,family=binomial(link='logit')))



