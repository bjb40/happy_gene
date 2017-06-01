##environment test

library(ggplot2); library(dplyr); library(reshape2); library(lme4)

#limit to white 2006 on (can go backwards as necessary)

analyze = cleandat %>% filter (black==0, orace==0, iwendy>=2006)

cxlin = lm(cesd~pm_uer+r_cohort+
             factor(iwendy)+male+age + 
             I(age^2) + 
             marr,dat=analyze %>% filter(ret==1))

print(summary(cxlin))

cxlin2 = lm(cesd~pm_uer+r_cohort+
             factor(iwendy)+male+age + 
             I(age^2) + 
             marr,dat=analyze %>% filter(emp==1))

print(summary(cxlin2))


######measuring the deltas---small effects

analyze.panel = analyze %>% 
  group_by(hhidpn) %>%
  summarize(iuer = mean(pm_uer, na.rm=TRUE)) %>%
  ungroup

analyze = merge(analyze,analyze.panel,by='hhidpn') %>%
  group_by(hhidpn) %>%
  mutate(md_uer = pm_uer - iuer,
         d_uer = pm_uer - lag(iuer)) %>%
  ungroup

hlm = lmer(cesd~iuer+md_uer+
       factor(iwendy)+male+age + 
       I(age^2) + unemp + ret +
       marr + (1|hhidpn),dat=analyze)

print(summary(hlm))

print('Scope of Effect')
print(0.0402021*range(analyze$md_uer,na.rm=TRUE))

#washes out --- whateer the interactions are
#hlm2 = lmer(cesd~iuer+md_uer*unemp + md_uer*ret +
#             factor(iwendy)+male+age + 
#             I(age^2) + unemp + ret +
#             marr + (1|hhidpn),dat=analyze)

print(summary(hlm2))

