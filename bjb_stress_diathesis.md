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



#Introduction

This study tests the genetic vulnerability or resistance to depressive events in the Health and Retirement Study (HRS). We approach this in the framework of the diathesis-stress model, which suggests excess genes confer excess vulnerability to negative outcomes when exposed to stress. We also test an opposing theory, that genes confer excess resilience to negative outcomes when exposed to stress. Combining data from the Health and Retirement Study (HRS) with information on regional unemployment rates from the Bureau of Labor Statistics (BLS), we test sensitivities to changing economic conditions between 2007 and 2012 as changes in depressive symptoms. Then, using a polygenic score from a large published Genome Wide Association Study (GWAS), we test whether individuals' reaction to the changing economic conditions vary with polygenic risk scores (PGS). Using white non-Hispanic adults with genetic data in the HRS (n=9,946), we find (1) individuals with higher subjective well-being PGS have lower levels of depressive symptoms, but (2) individuals with higher subjective well-being PGS are more sensitive to changing economic conditions, as measured by the unemployment rate. 

#Background

The stress process model has been used to explain the development of all sorts of adverse health events--including continuous measures of psychological distress and depressive symptoms which do not reach clinically diagnosable levels [@gayman_multiple_2013]. In its most basic formulation, the stress process model suggests that organisms respond negatively to negative stimuli from the environment [@pearlin_stress_1981]. Heterogeneity in negative responses depend on mechanisms that can either buffer or exacerbate the stress response. Studies have found that economic conditions related to "job churn" generate poor health outcomes [@strully_job_2009]. Importantly, poor economic conditions associated with poor economic conditions, like rising unemployment rates, are associated with large-scale changes in mental health and psychological well-being [@lam_is_2014]. This generally operates through increasing job uncertainty. In addition to these general patterns, there has also been a secular increase in job uncertainty beginning in the late 1970s and accelerating after the recessions of the 2000s [@kalleberg_good_2011].

This study asks whether there is variation in the response to these generalized stressful conditions is associated with differences in genetic risk scores. The theory of genetic heterogeneity in vulnerability to stress response is known as the  diathesis-stress model [@domingue_genetic_2016]. Studies have found evidence of genetic vulnerability to stress, such as the death of a spouse [@domingue_genetic_2016]. Reports of psychological distress represent an exceedingly complex phenotype [@conley_promise_2009], and there are multiple genetic markers we can possibly use. Like Domingue, et al. we use positive subjective well-being (*i.e.* happiness) as our genetic measure.

We present two sets of hypotheses below. One set regards direct economic impacts, and the other set is about indirect economic impacts:

Hypothesis 1: Low polygenic risk scores for subjective well-being are associated with vulnerability to direct economic impacts, like unemployment. 

Hypothesis 2a: Low polygenic risk scores for subjective well-being are associated higher vulnerability to indirect economic impacts, like changes in the regional unemployment rate. 

Hypothesis 2b: Low polygenic risk scores for subjective well-being are associated  higher vulnerability to indirect economic impacts, like unemployment. 

#Data

To examine these hypotheses, we use the Health and Retirment Study (HRS). The HRS is a probability-based sample of non-institutionalized adults 50 years and older. The HRS started in 1992 and is conducted every two years. As older cohorts age, the HRS refreshes the panel with samples of younger cohorts. In 2006 and 2008, the HRS collected genomic data from respondents. Because some key measures are different in the first wave and successive waves, we use waves 2-12 (1994-2014) which span a twenty year period. We also limit our analysis to non-hispanic whites. 

##Outcome: Depressive Symptoms

We measure *depressive symptoms* using the Center for Epidemeological Studies Depression 8 item (CESD-8) scale. This scale is a sum of the following dichotomous measures: felt depressed; felt everything was an effort; sleep was restless; could not get going; felt lonely; enjoyed life (reverse coded); felt sad; was happy (reverse coded). 

##Direct Economic Impacts

We measure direct economic impacts by changes in employement status. We measure employment status using a dummy variable series for employed (reference), unemployed, and retired. 

##Indirect Economic Impacts

As outlined in the hypotheses above, we operationalize indirect measures of economic impacts in two ways. First, we measure by survey year. In addition, we conduct analyses over the period before the dot-com bust (1994-2000 and earlier) and after the dot-com bust (2002-2014). We also include a measure of the regional unemployment rate in the month and year prior to the interview. We take this figure from the Bureau of Labor Statistics regional unemployment rate measures.

##Polygenic Risk Scores

We follow the procedure outlined in Dominigue, et al. [-@domingue_genetic_2016] to calculate our polygenic risk scores. First, we match SNPs in the HRS to SNPS in the GWAS conducted by Okbay et al. [-@okbay_genetic_2016] available through the Social Science Genetic Association Consortium. Second, we sum the weighted estimates of well-being associated alleles from the GWAS to calculate a polygenic score. Third, we adjust the residulized PGS using 10 principal components and PLINK (command: pca), and normalize the PGS residual distribution, which we use as the measure of genetic predisposition to well-being.

##Controls

We include a handfull of time-varying controls which have been shown to associate with depressive symptoms. These are age marital status, household income and household wealth. Since we employ fixed effects models as described below, we do not include time-invariant characteristics such as gender, although we do include gender in our summary of descriptive statistics.

#Analytic Strategy

Treating changes in unemployment rates as exogenous,  we employ a standard fixed effects design to test our environmental models [@conley_promise_2009]. We then interact environmental conditions with the PGS. We do not include a direct effect of the PGS in our models because it is not identified.

Environment Model:


$$
(\bar{y}_{i\cdot} - y_{it}) = \delta(\bar{x}_{i\cdot} - x_{it}) + \epsilon_{it}
$$



Interaction Model:



$$
(\bar{y}_{i\cdot} - y_{it}) = \delta(\bar{x}_{i\cdot} - x_{it}) + \epsilon_{it}) + \gamma[(\bar{x}_{i\cdot} - x_{it})*g_i] + \epsilon_{it}
$$


Where $y$ indicates the outcome, $i$ indexes individuals, and $t$ indexes time, $x$ is various time-varying covariates as described above, and $g$ is the PGS calculated as described above.


#Results

``` see powerpoint; to be inserted later ```

#Discussion and Conclusion



#References

