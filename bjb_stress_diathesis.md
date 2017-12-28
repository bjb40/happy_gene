---
author:
 - Bryce Bartlett
 - Fang Fang
 - Anitoly Yashin
 - Arseniy Yashkin
 - Igor Akushevich
 - Deqing Wu

institute:
 - "PhD Candidate (Sociology) Duke University, Social Science Research Institute, Duke Population Research Institute, Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"
 - "Duke University Biodemography of Aging Research Unit"

date: 12/13/2017
title: "Genetic Heterogeneity in Vulnerability to Depression During Changing Economic Conditions"
csl: ./citations/jshb.csl
bibliography: ./citations/happy_gene.bib
---

#Abstract



Introduction

This study tests the genetic vulnerability or resistance to depressive events in the Health and Retirement Study (HRS). We approach this in the framework of the diathesis-stress model, which suggests excess genes confer excess vulnerability to negative outcomes when exposed to stress. We also test an opposing theory, that genes confer excess resilience to negative outcomes when exposed to stress. Combining data from the Health and Retirement Study (HRS) with information on regional unemployment rates from the Bureau of Labor Statistics (BLS), we test sensitivities to changing economic conditions between 1994 and 2014 through changes in depressive symptoms. Then, using a polygenic score from a large published Genome Wide Association Study (GWAS), we test whether individuals' reaction to the changing economic conditions vary with polygenic risk scores (PGS). Using white non-Hispanic adults with genetic data in the HRS (n=9,946), we find that  individuals with higher subjective well-being PGS have lower levels of depressive symptoms. We also find that gene by environment interactions behave differently depending on the period. First, in the later periods (including the Great Recession), higher genetic predispositions to subjective well-being are associated with higher vulnerability to unemployment rates. However, in the earlier periods (preceing the Great Recession), higher genetic predispositions to subjective well-being are associated with an increased protective effect to retirement. This protective effect ddisappers in the later period.

#Background

The stress process model has been used to explain the development of all sorts of adverse health events--including continuous measures of psychological distress and depressive symptoms which do not reach clinically diagnosable levels [@gayman_multiple_2013]. In its most basic formulation, the stress process model suggests that organisms respond negatively to negative stimuli from the environment [@pearlin_stress_1981]. Heterogeneity in negative responses depend on mechanisms that can either buffer or exacerbate the stress response, including potential gene-by-environment interactions [@schnittker_gene_2013]. Studies have found that economic conditions related to "job churn" generate poor health outcomes [@strully_job_2009]. Importantly poor economic conditions, like rising unemployment rates, are often associated with large-scale changes in mental health and psychological well-being [@lam_is_2014]. A number of studies find conflicting results, however. Burgard and Kalousova [-@burgard_effects_2015] outline a number of conflicting mechanisms whereby a recesive period may be beneficial (by reducing stress related to job performance) or harmful (by increasing stress related to finances).

In addition to these general patterns related to recessions, the past twenty years have revealed a number of changes in work and retirement in the United States Kalleberg [-@kalleberg_good_2011] provides a detailed description of the rise of "precarious employment" beginning in the 1970s, and accelerating in the 1990s and 2000s. These include significant changes in the structure of retirement benefits whereby employers have shifted economic uncertainty to workers by substituting defined benefit plans (pensions) with defined contribution plans (like the 401k) [@kalleberg_good_2011, pp. 126, 188]. In addition to retirement strucutres, the nature of work itself has also changed, and these changes often lead to declines in well-being [@schnittker_diagnosing_2008].

This study asks whether there is variation in the response to these generalized stressful conditions related to the economic environment is associated with differences in genetic risk scores. The theory of genetic heterogeneity in vulnerability to stress response is known as the  diathesis-stress model [@domingue_genetic_2016]. Studies have found evidence of genetic vulnerability to stress, such as the death of a spouse [@domingue_genetic_2016]. Reports of psychological distress represent an exceedingly complex phenotype [@conley_promise_2009], and there are multiple genetic markers we can possibly use. Like Domingue, et al. we use positive subjective well-being (*i.e.* happiness) as our genetic measure.

We present two sets of hypotheses below. One set regards direct economic impacts, and the other set is about indirect economic impacts:

Hypothesis 1: Low polygenic risk scores for subjective well-being are associated with vulnerability to direct economic impacts, like changing employment status.

Hypothesis 2a: Low polygenic risk scores for subjective well-being are associated higher vulnerability to indirect economic impacts, like changes in the regional unemployment rate.

Hypothesis 2b: Low polygenic risk scores for subjective well-being are associated  higher vulnerability to indirect economic impacts, including the persistent increase in job uncertainty over time, *i.e.* an interaction between the period and the polygenic risk score.

#Data

To examine these hypotheses, we use the Health and Retirment Study (HRS). The HRS is a probability-based sample of non-institutionalized adults 50 years and older. The HRS started in 1992 and is conducted every two years, although responses for some waves occur in the year prior or subsequent to the succeeding wave. As older cohorts age, the HRS refreshes the panel with samples of younger cohorts. In 2006 and 2008, the HRS collected genomic data from respondents. Because some key measures are different in the first wave and successive waves, we use waves 2-12 (1994-2014) which span a twenty year period, as noted above, these waves also include a handful of responses recieved in 2013 and 2015. We limit our analysis to non-hispanic whites as prior studies in gene-environment interactions testing the stress process [@domingue_genetic_2016].

##Outcome: Depressive Symptoms

We measure *depressive symptoms* using the Center for Epidemeological Studies Depression 8 item (CESD-8) scale. This scale is a sum of the following dichotomous measures: felt depressed; felt everything was an effort; sleep was restless; could not get going; felt lonely; enjoyed life (reverse coded); felt sad; was happy (reverse coded).

##Direct Economic Impacts

We measure direct economic impacts by changes in employement status. We measure employment status using a dummy variable series for employed (reference), unemployed, and retired.

##Indirect Economic Impacts

As outlined in the hypotheses above, we operationalize indirect measures of economic impacts in two ways. First, we measure by survey year. In addition, we slice the time period in two, and analyze both the earlier period (waves 2-6) and the later period (waves 7-12) indpendently. Second, we include a measure of the regional unemployment rate in the month and year prior to the interview. We take this figure from the Bureau of Labor Statistics regional unemployment rate measures.

##Polygenic Risk Scores

We follow the procedure outlined in Dominigue, et al. [-@domingue_genetic_2016] to calculate our polygenic risk scores. First, we match SNPs in the HRS to SNPS in the GWAS conducted by Okbay et al. [-@okbay_genetic_2016] available through the Social Science Genetic Association Consortium. Second, we sum the weighted estimates of well-being associated alleles from the GWAS to calculate a polygenic score. Third, we adjust the residulized PGS using 10 principal components and PLINK (command: pca), and normalize the PGS residual distribution, which we use as the measure of genetic predisposition to well-being.

##Controls

We include a handfull of time-varying controls which have are associated with depressive symptoms. These are age marital status, household income and household wealth. For wealth and income measures we add the absolute value of the minimum value (so no values are negative) and take the natural log of the measure. We also include a dummy indicator for whether the household's wealth is negative. Since we employ fixed effects models as described below, we do not include time-invariant characteristics such as gender, although we do include gender in our summary of descriptive statistics reported in Table 1.

#Analytic Strategy

Treating changes in unemployment rates as exogenous,  we employ a standard fixed effects design to test our environmental models [@conley_promise_2009]. We then interact environmental conditions with the PGS. We do not include a direct effect of the PGS in our models because it is not identified.

Environment Model:


$$
(\bar{y}_{i\cdot} - y_{it}) = \delta(\bar{x}_{i\cdot} - x_{it}) + \epsilon_{it}
$$


Gene-by-environment Interaction Model:



$$
(\bar{y}_{i\cdot} - y_{it}) = \delta(\bar{x}_{i\cdot} - x_{it}) +  \gamma[(\bar{x}_{i\cdot} - x_{it})*g_i] + \epsilon_{it}
$$


Where $y$ indicates the outcome, $i$ indexes individuals, and $t$ indexes time, $x$ is various time-varying covariates as described above, $g$ is the PGS calculated as described above, and $\epsilon is a normally distributed stochastic term with mean zero.

#Results

Table 1 reports descriptive statistics for the panels (83,233 person-wave observations). Tehre is relatively little missing data (less than 3%) as reported in teh final column. Approximately 9.9% of the total number of respondents (9,947) died over the tie of observation, and the mean waves observed is 8.368 (out of 11 total). Excluding mortality and attrition, missing data is negligible (3%) as identified in the final column of Table 1. CESD levels are near the bottom of the range at 1.186 and an almost equivalent standard deviation. Mean deviated CESD is cented on 0, with long tails. As reported on the bottom part of the table, the mean length of retirment in the sample is about half of the observed time (53.7%), with 10.1% average time unemployed.

Figure 1 slices the analytic dataset into qunitiles based on the PGS score (displayed on the x-axis). Individuals with mean levels of CESD higher than 1 (sample mean is 1.3) are plotted with the y axis. This figure shows a small, but meaningful gradient. The higher the PGS for subjective well-being, the lower the average number of depressive symptoms is across the observation period.

Mean levels of regional unemployment rates center are nearly 6%, but evince wide variation between 2.2% and 12.2%. Figure 2 depicts the distribution of unemployment. The dots each represent a respondent observation ofr monthly regional unemployment rate in the month prior to the interview date.The solid line indicates the wave-level averages. The Recessions of 2001 and 2007 are indicated by increasing unemployment rates (with the latter showing a dramatic increase).

Figure 3 provides preliminary evidence of a gene-by-environment interaction. As with Figure 1, Figure 3 slices the x-axis across PGS quintiles, with PGS scores for higher subjective well-being increasing from left to right. For the entire sample, Figure 3 plots proportions with higher than individual average levels of depressive symptoms (reported on the y axis). These proportions are reported for times when individuals experienced an increasing regional unemployment (triangles) or a static/declining unemployment rate (circles). The highest and lowest qunitiles show statistically signficant differences in proportions, where individuals on the upper and lower 20% of the PGS distributions being more sensitive to the differnece in unemployment rates.

Table 2 shows the results of two regressions. The first model contains the environment model, and the second model contains the interactions with the PGS score. This table provides support for hypothesis 2b, showing a small, but statistically significant interaction effect with the interview year. The main effect of interview year is an increase in depressive symptoms over time (0.150). However, this effect is attenuated---though slightly---with increases PGS (-0.003 for each standard deviation increase). Table 2 shows no evidence of an interaction between PGS an direct or indirect exposure to changes in economic context.

Table 3 reports additional gene-by-environment interactions, but .



#Discussion and Conclusion

id jason boardman

#References

