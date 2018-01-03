---
author:
 - Bryce Bartlett
 - Fang Fang
 - Anitoly Yashin

institute:
 - "PhD Candidate (Sociology) Duke University, Social Science Research Institute, Duke Population Research Institute, Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"

date: 1/3/2018
title: "Heterogeneity in Gene-Environment Associations of Depressive Symptoms and Changing Economic Conditions"
csl: ./citations/jshb.csl
bibliography: ./citations/happy_gene.bib
---

#Abstract



#Introduction

This study tests the genetic vulnerability or resistance to depressive events in the Health and Retirement Study (HRS). We approach this in the framework of the stress process model, which suggests excess genes confer excess vulnerability to negative outcomes when exposed to stress. We also test an opposing theory, that genes confer excess resilience to negative outcomes when exposed to stress. Combining data from the Health and Retirement Study (HRS) with information on regional unemployment rates from the Bureau of Labor Statistics (BLS), we test sensitivities to changing economic conditions between 1993 and 2015 through changes in depressive symptoms. Then, using a polygenic score from a large published Genome Wide Association Study (GWAS), we test whether individuals' reaction to the changing economic conditions vary with polygenic risk scores (PGS). Using white non-Hispanic adults with genetic data in the HRS (n=9,946), we find that  individuals with higher subjective well-being PGS have lower levels of depressive symptoms. We also find that gene by environment interactions behave differently depending on the period. First, in the later periods (including the Great Recession), higher genetic predispositions to subjective well-being are associated with higher vulnerability to unemployment rates. However, in the earlier periods (preceing the Great Recession), higher genetic predispositions to subjective well-being are associated with an increased protective effect to retirement. This protective effect disappers in the later period.

#Background

The stress process model has been used to explain the development of all sorts of adverse health events--including continuous measures of psychological distress and depressive symptoms which do not reach clinically diagnosable levels [@gayman_multiple_2013]. In its most basic formulation, the stress process model suggests that organisms respond negatively to negative stimuli from the environment [@pearlin_stress_1981]. Heterogeneity in negative responses depend on mechanisms that can either buffer or exacerbate the stress response, including potential gene-by-environment interactions [@schnittker_geneenvironment_2010]. Studies have found that economic conditions related to "job churn" generate poor health outcomes [@strully_job_2009]. Importantly poor economic conditions, like rising unemployment rates, are often associated with large-scale changes in mental health and psychological well-being [@lam_is_2014].

A number of studies find conflicting results, however. Burgard and Kalousova [-@burgard_effects_2015] outline a number of conflicting mechanisms whereby a recesive period may be beneficial (such as reducing hours and job-performance stress) or harmful (such as increasing job uncertainty).

This study asks whether changes in depressive symptoms associated with changes in the economic environment---regardless of direction---differ across polygenic risk scores for subjective well-being. We look at changes across the economic environment across a number of contexts. We analyze whether polygenic risk scores exlplain some of the variation in stress response by looking at both direct (employment status) and indirect (unemployment rates) changes to the economic envrionment, inlcuding short-term and longer-term changes.

## Short-term Changes to the Economic Environment. 

The theory of genetic heterogeneity in vulnerability to stress response is known as the  diathesis-stress model [@domingue_genetic_2016]. Studies have found evidence of genetic vulnerability to stress, such as the death of a spouse [@domingue_genetic_2016]. Reports of psychological distress represent an exceedingly complex phenotype [@conley_promise_2009], and there are multiple genetic markers we can possibly use. Like Domingue, et al. we use positive subjective well-being (*i.e.* happiness) as our genetic measure. Employment and working status makes up a large part of an individual's day. Accordingly, a change to employment status (even a beneficial change) is also associated with dramatic differences in daily activities, in identity, and in social engagement. Apart from these direct changes to the economic environment, a commonly reported measure of economic health is the unemployment rate. This rate is also related to feelings of uncertainty in employment status, even when employment status has not changed. 

This provides hypotheses for both direct and indirect environmental exposures:

Hypothesis 1a: Low polygenic risk scores for subjective well-being are associated with vulnerability to direct economic impacts, like changing employment status to unemployed or retired.

Hypothesis 1b: Low polygenic risk scores for subjective well-being are associated with vulnerability to indirect economic impacts, like changes to the unemployment rate.

## Longer-term Changes to the Economic Environment.

In addition to general patterns related to recessions, the past twenty years have revealed a number of changes in work and retirement in the United States Kalleberg [-@kalleberg_good_2011] provides a detailed description of the rise of "precarious employment" beginning in the 1970s, and accelerating in the 1990s and 2000s. These include significant changes in the structure of retirement benefits whereby employers have shifted economic uncertainty to workers by substituting defined benefit plans (pensions) with defined contribution plans (like the 401k) [@kalleberg_good_2011, pp. 126, 188]. This, together with additional pressure on public retirement systems like social security, leads to increasing age-of-retirement. In addition to retirement strucutres, the nature of work itself has also changed, and these changes often lead to declines in well-being [@schnittker_diagnosing_2008].

These changes, and in particular the Great Recession of 2007 provide an opportunity to test theories of gene-environment interaction known as "social push" and "social trigger" [@boardman_trends_2010]. Each theory predicts statistical significance of gene-environment interactions, but for different reasons. The social trigger theory is a causal theory. It predicts that particular social environments are necessary to "trigger," *i.e.*, engage the gene to develop a particular phenotype. In contrast, social push is non-causal. Instead, it theorizes that extreme environments "push" certain phenotypes without regard to genetic predispositions [@boardman_social_2012]. Only when these environments are relaxed do genetic influences become important to phenotype expression. Because complex social phenomena are subject to change---sometimes dramatic change---both social push and social trigger hypotheses can occur over time. For example, Boardman, *et al.* [-@boardman_trends_2010] show that smoking heritability is null when smoking was common in the 1960s, but had impact both before and after this precipitous rise in smoking.

In terms of changes to the economic environment over the last twenty years, the key change in economic terms is increasing varability. This is true in terms of economic outcomes, such as the increasing concentration of wealth and income. It is also true in terms of increasing levels of uncertainty, such as higher variability in unemployment rates triggered by the Great Recession. Under these conditions, the social trigger hypothesis suggests that these changing conditions introduce new effects of genetics, *i.e.* the earlier period suggests a null gene-environment interaction. The social push hypothesis suggests exactly the opposite. In other words, it suggests that the genetic vulnerabiliy to changing economic conditions is observable in the first period, but that the increased pressures introduced by variabilities in economic conditions "pushes" the whole population towards depression, and eliminates observable genetic influences on depression.

More formally, this leads to the following two hypotheses:

Hypothesis 2a (trigger): The earlier period (prior to 2004) displays a null gene-environment interaction effect with depressive symptoms; the later period does not.

Hypothesis 2b (push): The later period (2004 and after) displays a null gene-environment interaction effect with depressive symptoms; the later period does not.

#Data

To examine these hypotheses, we use the Health and Retirment Study (HRS) [@university_of_michigan_health_2016]. The HRS is sponsored by the National Institute on Aging (grant number NIA U01AG009740) and is conducted by the University of Michigan. It is a probability-based sample of non-institutionalized adults 50 years and older. The HRS started in 1992 and is conducted every two years, although responses for some waves occur in the year prior or subsequent to the succeeding wave. As older cohorts age, the HRS refreshes the panel with samples of younger cohorts. In 2006 and 2008, the HRS collected genomic data from respondents, which we merge into prior data [@university_of_michigan_health_2017]. Because some key measures are different in the first wave and successive waves, we use waves 2-12 (1994-2014) which span a twenty year period, as noted above, these waves also include a handful of responses recieved in 2013 and 2015. We limit our analysis to non-hispanic whites as prior studies in gene-environment interactions testing the stress process [@domingue_genetic_2016].

##Outcome: Depressive Symptoms

We measure *depressive symptoms* using the Center for Epidemeological Studies Depression 8 item (CESD-8) scale. This scale is a sum of the following dichotomous measures: felt depressed; felt everything was an effort; sleep was restless; could not get going; felt lonely; enjoyed life (reverse coded); felt sad; was happy (reverse coded).

##Direct Economic Impacts

We measure direct economic impacts by changes in employement status. We measure employment status using a dummy variable series for employed (reference), unemployed, and retired.

##Indirect Economic Impacts

As outlined in the hypotheses above, we operationalize indirect measures of economic impacts in two ways. First, we measure by survey year. In addition, we slice the time period in two, and analyze both the earlier period (waves 2-6) and the later period (waves 7-12) indpendently. Second, we include a measure of the regional unemployment rate in the month and year prior to the interview. We take this figure from the Bureau of Labor Statistics regional unemployment rate measures.

##Polygenic Risk Scores

We follow the procedure outlined in Dominigue, et al. [-@domingue_genetic_2016] to calculate our polygenic risk scores. First, we match SNPs in the HRS to SNPS in the GWAS conducted by Okbay et al. [-@okbay_genetic_2016] available through the Social Science Genetic Association Consortium. Beginning in June 2017, the HRS provides this with its data. Second, we sum the weighted estimates of well-being associated alleles from the GWAS to calculate a polygenic score. Third, we adjust the residulized PGS using 10 principal components, and normalize the PGS residual distribution, which we use as the measure of genetic predisposition to well-being.

##Controls

We include time-varying controls which are associated with depressive symptoms. These are age marital status, household income and household wealth. For wealth and income, we use imputations conducted by Rand [@phillip_pantoja_rand_2016]. For wealth and income measures we add the absolute value of the minimum value plus one (so no values are negative) and take the natural log of the measure. We also include a dummy indicator for whether the household's net worth is negative. Since we employ fixed effects models as described below, we do not include time-invariant characteristics such as gender, although we do include gender in our summary of descriptive statistics reported in Table 1.

#Analytic Strategy

Treating changes in unemployment rates as exogenous,  we employ a standard indiviudal fixed effects design to test our environmental models [@conley_promise_2009]. We then interact environmental conditions with the PGS. We do not include a direct effect of the PGS in our models because it is not identified.

Environment Model:


$$
(y_{it}-\bar{y}_{i\cdot}) = \delta(x_{it} - \bar{x}_{i\cdot} ) + \epsilon_{it}
$$


Gene-by-environment Interaction Model:



$$
(y_{it}-\bar{y}_{i\cdot}) = \delta( x_{it} - \bar{x}_{i\cdot}) +  \gamma[(x_{it}-\bar{x}_{i\cdot})*g_i] + \epsilon_{it}
$$


Where $y$ indicates the outcome (depressive symptoms), $i$ indexes individuals, and $t$ indexes time, $x$ is various time-varying covariates as described above, $g$ is the PGS calculated as described above, and $\epsilon$ is a normally distributed stochastic term with mean zero.

#Results

Table 1 reports descriptive statistics for the panels (83,233 person-wave observations). Approximately 9.9% of the total number of respondents (9,947) died over the twenty-year observation period, and were observed an average of 8.368 times (out of 11 possible). Survival does not appear strongly related to PGS scores for well-being (supplement, table S-1), so we do not adjust for selective mortality. Excluding mortality and attrition and using the RAND-imputed variables for wealth and income, missing data is negligible (around 3%) as identified in the final column of Table 1. In both periods, CESD levels are near the bottom of the range, between 1.105 at 1.236. In both periods, mean deviated CESD is cented near 0, with long tails; however, the later period, which includes the great recession shows higher standard deviations. A similar pattern occurs with the unemployment rate.


\[Table 1 about here.\]



Figure 1 slices the analytic dataset into qunitiles based on the PGS score (displayed on the x-axis). Proporitons of individuals showing higher than average depressive symptoms over the period are plotted with the y axis for both the earlier and later period. This figure shows a small, but meaningful gradient. The higher the PGS for subjective well-being, the lower the average number of depressive symptoms is across the observation period. There is a slightly more prounounced gradient in 2004-2015 than in 1993-2003, where higher proportions of individuals with low SWB PGS experiencing persistently higher levels of depressive symptoms.


[\Figure 1 about here.\]



Figure 2 depicts Mean levels of regional unemployment rates. The rates center around 6%, but evince wide variation between 2.2% and 12.2%. Figure 2 depicts the distribution of unemployment. Each dot respondents observation for monthly regional unemployment rate in the month prior to the interview date.The solid line indicates the wave-level averages. The Recessions of 2001 and 2007 are indicated by increasing unemployment rates (with the latter showing a dramatic increase).


\[Figure 2 about here.\]


Figure 3 provides preliminary evidence of a gene-by-environment interaction with unemployment rates and labor status. As with Figure 1, Figure 3 slices the x-axis across PGS quintiles, with PGS scores for higher subjective well-being increasing from left to right. They y axis displays the proportion of observations with increasing levels of depressive symptoms (a first-difference score greater than 0). These proportions are reported across times with increasing increasing regional unemployment (triangles) or a static/declining regional unemployment rates (circles). In addition, the figure is separated by employment status (vertical panels) and period (horizontal panels), and includes a horizontal reference line at a proportion of 0.30 for ease of comparison across facets. This figure shows an observable and consistent gradient in PGS scores for those employed only in the earlier period (1993-2003), which disappears in the later period. There is a consistent gradient in depressive symptoms for those who are retired: those with the highest subjective well-being PGS are less likey to have increasing depressive symptoms. This is the case across both periods, and without regard to changes in the unemployment rate. In contrast, there is little to no gradient for the unemployed. 

\[Figure 3 about here.\]

Table 2 shows the results of two regressions. The first model contains the environment model, and the second model contains the interactions with the PGS score. This table provides support for hypothesis 2b, showing a small, but statistically significant interaction effect with the interview year. The main effect of interview year is an increase in depressive symptoms over time (0.150). However, this effect is attenuated---though slightly---with increases PGS (-0.003 for each standard deviation increase). Table 2 shows no evidence of an interaction between PGS an direct or indirect exposure to changes in economic context.

\[Table 2 about here.\]


Table 3 reports additional gene-by-environment interactions, but conducts analyses independently from the earlier and later periods. The effects between the earlier and later period show some significant differences. In terms of environmental effects, there is an increase in the importance of the unemployment rate across employment contexts (the interactions with retirement, unemployed and unemployment rates). In the later period, the interaction between the unemployment rate and the polygenic score for subjective well-being reverses sign from negative to positive, and become statistically different from zero in the later period. 

\[Table  3 about here.\]

Figure 4 displays the relevant interaction effects from the models of Table 3. The panel shows estimated effect sizes and 95% confidence intervals shaded by p-values. The earlier period is represented by a square, and the later period by a circle. In the earlier period, a rising unemployment rate and retirement had negative associations with (though neither are statistically different from zero at p<0.05). In the later period, however, these effects reverse, and the variance of the effect diminishes by about half. The interaction of the unemployment rate with the PGS has a positive impact. Similarly, the association of the unemployment rate for those who have retired (Retirement x U/E) also reverses in direction. Chow tests in a pooled model confirm that these effects are different from one another (even when they are not statistically distinguishable from 0 in the independently run models).

#Discussion and Conclusion

This study analyzed gene-environment associations with depression and changing economic conditions. While there are clear and consistent differences in levels of depressive symptoms across polygenic risk score values (depicted in Figure 1), there is little evidence that these are consistently sensitive to changes in employment conditions, whether direct or indirect. Instead, we find evidence that the gene-environment impacts reverse in sign over a twenty year period from 1993 to 2015, confirming hypothesis 2. 

Specifically, we find weak evidence of social push for retirement. In the earlier period (1993-2003), which is characterized by lower variation in unemployment rates, the interaction of the PGS and retirement has a negative association with depressive symptoms. In other words, retiring in the earlier period was associated with *better* mental health. This effect disappers in the later period. This evidence suggests that higher economic uncertainty and lower retirement security overwhelms what was once a positive effect of genetic endowments to the retierment transition, and pushes the population to higher general levels of depressive symptoms.

Opposingly, we find evidence of a social trigger for unemployment rates among those who remain employed. The earlier period displays a null association with the interaction of the PGS and unemployment rates, but the later period shows a *negative* association. While this is a social trigger, it is paradoxically those with higher PGS scores for subjective well-being that are more vulnerable to rising unemployment rates in the later period. In other words, those predisposed to higher levels of subjective well-being were also more vulnerable to the dramatic increases in unemployment rates occuring in the later period (2004-2015). 

There are a few limitaitons to the study. First, we chose a polygenic score that has been used in these contexts previously; this is a complex phenotype and there may be other, or better PGS relationships to test. In addition, the GWAS sample used to generate the PGS may impact the results in unknown ways. Second, while we see no significant evidence of selective mortality, we have not analyzed the possibility of differential retirement selectivity across the periods identified. Different labor statuses are associated with different levels of depression, and they also show different associations and sensitivity with changes in the unemployment rate. Retirement is a partially voluntary status, but the pressures on the choice surrounding retirement are not equal across the observation period. Selective retirement, and changes in selective retirement, may affect the results.

Despite these limitations, this is an important first step in understanding the association between economic conditions and mental health, particularly in view of the slow rise of precarious employment and retirement since the 1970s, and the dramatic increases in economic uncertainty in the Great Recession of 2007. These findings suggest that genetic predispositions to happiness can confer vulnerability or resilience, depending on the context. In addition, they punctuate findings that social environments can change rapidly, highlighting the importance of environmental conditions on gene expression. 


#References

