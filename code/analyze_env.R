##environment test

rm(list=ls())

source('code/config~.R',echo=TRUE)

#load cleandat
cleandat.f = paste0(outdir,'private~/cleandat.RData')
if(file.exists(cleandat.f)){
  load(cleandat.f)
} else{
  print('Cleanding HRS data.\n\n') 
  source('code/prep-hrs-panel.R',echo=TRUE)
}

library(ggplot2);  library(reshape2);
library(lme4); library(plm); library(merTools); library(dplyr)

#limit to white 

analyze = cleandat %>% filter (black==0, orace==0)

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
form = cesd~pm_uer+marr+age+factor(iwendy)+atota+itot

#not sure why 2012 isn't getting factored 
plm1 = plm(form, 
           data=analyze,index='hhidpn',model='within')
print(summary(plm1))

plm2 = plm(form, 
           data=analyze,index='hhidpn',model='random')
print(summary(plm2))

hausman = phtest(form,data=analyze,index='hhidpn')

######
######measuring the deltas---small effects

#you do this enough across contexts: make a function....


analyze.panel = analyze %>% 
  group_by(hhidpn) %>%
  summarize(iuer = mean(pm_uer, na.rm=TRUE),
            iret = mean(ret,na.rm=TRUE),
            iunemp = mean(unemp,na.rm=TRUE),
            iatota = mean(atota,na.rm=TRUE),
            iitot = mean(itot, na.rm=TRUE)) %>%
  ungroup

analyze = merge(analyze,analyze.panel,by='hhidpn') %>%
  group_by(hhidpn) %>%
  mutate(md_uer = pm_uer - iuer,
         md_ret = ret - iret,
         md_unemp = unemp - iunemp,
         md_atota = atota - iatota,
         md_itot = itot - iitot) %>%
  ungroup

#basic
hlm = lmer(cesd~pm_uer+
       factor(iwendy)+male+age + 
       I(age^2) + unemp + ret +
       marr + itot + atota + (1|hhidpn),dat=analyze)

print(summary(hlm))
hlm.sim = FEsim(hlm,n.sims=200)
yrs=hlm.sim
plotFEsim(hlm.sim)

#print('Scope of Effect')
#print(0.0402021*range(analyze$md_uer,na.rm=TRUE))

hlm2 = lmer(cesd~iuer+iret+iunemp +
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
levels(analyze$r_intens.f) = strtrim(levels(analyzer$r_intens.f),3)

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
hlm2b = lmer(cesd~iuer*(iret+iunemp) +
              md_uer*(md_ret+md_unemp) +
              factor(iwendy) + male + age + 
              I(age^2) + marr + 
              (1|hhidpn),
            data=analyze)

hlm2b.sim=FEsim(hlm2b,n.sims=200)
interact=grepl('ue',hlm2b.sim$term)
plotFEsim(hlm2b.sim[interact,])

#print(summary(hlm2b))

