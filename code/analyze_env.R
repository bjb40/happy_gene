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

library(ggplot2); library(dplyr); library(reshape2); library(lme4); library(plm)

#limit to white 

analyze = cleandat %>% filter (black==0, orace==0)

cx.ret = lm(cesd~pm_uer+
             factor(iwendy)+male+age + 
             I(age^2) + 
             marr,dat=analyze %>% filter(ret==1))

print(summary(cx.ret))

cx.emp = lm(cesd~pm_uer+
             factor(iwendy)+male+age + 
             I(age^2) +
             marr,dat=analyze %>% filter(emp==1))

print(summary(cx.emp))

cx.unemp = lm(cesd~pm_uer+
              factor(iwendy)+male+age + 
              I(age^2) +
              marr,dat=analyze %>% filter(unemp==1))
print(summary(cx.unemp))


######measuring the deltas---small effects

analyze.panel = analyze %>% 
  group_by(hhidpn) %>%
  summarize(iuer = mean(pm_uer, na.rm=TRUE),
            iret = mean(ret,na.rm=TRUE),
            iunemp = mean(unemp,na.rm=TRUE)) %>%
  ungroup

analyze = merge(analyze,analyze.panel,by='hhidpn') %>%
  group_by(hhidpn) %>%
  mutate(md_uer = pm_uer - iuer,
         md_ret = ret - iret,
         md_unemp = unemp - iunemp) %>%
  ungroup

hlm = lmer(cesd~iuer+md_uer+
       factor(iwendy)+male+age + 
       I(age^2) + unemp + ret +
       marr + (1|hhidpn),dat=analyze)

print(summary(hlm))

#print('Scope of Effect')
#print(0.0402021*range(analyze$md_uer,na.rm=TRUE))

hlm2 = lmer(cesd~iuer+iret+iunemp +
               r_cohort*(md_uer+md_ret+md_unemp) +
               factor(iwendy) + male + age + 
               I(age^2) + marr + 
               (1|hhidpn),
             data=analyze)

print(summary(hlm2))

####individual f/e
plm1 = plm(cesd~pm_uer+marr+age+factor(iwendy), 
           data=analyze,index=c('hhidpn','iwendy'))
print(summary(plm1))


#rcohort baseline benefit; benefit to unemployment (definition in the sample?)
#people are more sensitive higher average unemployment rates
#equal to 5 percentage points!
hlm2a = lmer(cesd~r_cohort*(iuer+iret+iunemp) +
               r_cohort*(md_uer+md_ret+md_unemp) +
               factor(iwendy) + male + age + 
               I(age^2) + marr + 
               (1|hhidpn),
             data=analyze)


#interactions not significant
hlm2b = lmer(cesd~iuer*(iret+iunemp) +
              md_uer*(md_ret+md_unemp) +
              factor(iwendy) + male + age + 
              I(age^2) + marr + 
              (1|hhidpn),
            data=analyze)

print(summary(hlm2))

