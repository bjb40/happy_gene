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

The stress process model has been used to explain the development of all sorts of adverse health events--including continuous measures of psychological distress and depressive symptoms which do not reach clinically diagnosable levels [@gayman_multiple_2013]. In its most basic formulation, the stress process model suggests that organisms respond negatively to negative stimuli from the environment [@pearlin_stress_1981]. Heterogeneity in negative responses depend on mechanisms that can either buffer or exacerbate the stress response. Studies have found that economic conditions related to "job churn" generate poor health outcomes [@strully_job_2009]. Importantly, poor economic conditions associated with poor economic conditions, like rising unemployment rates, are associated with large-scale changes in mental health and psychological well-being [@lam_is_2014].

This study asks whether there is variation in the response to these generalized stressful conditions is associated with differences in genetic risk scores. The theory of genetic heterogeneity in vulnerability to stress response is known as the  diathesis-stress model [@domingue_genetic_2016]. While controversial, studies have found evidence of genetic vulnerability to stress, such as the death of a spouse [@domingue_genetic_2016]. Reports of psychological distress represent an exceedingly complex phenotype [@conley_promise_2009], and there are multiple genetic markers we can possibly use. Like Domingue, et al. we use positive subjective well-being (*i.e.* happiness) as our genetic measure.

The outcome we use is depressive symptoms, commonly used to test exposure to stress, and the environmental exposure is changes in the unemployment rate between 2002 and 2012 (inclusive of the Great Recession).

Hypothesis 1:

Hypothesis 2: 

There is  evidence that positive subjective well-being is also related to longevity [@diener_happy_2011]. accordingly, we also test whether PGS for high subjective well-being is related to survival.

Hypothesis 3: ```survival: if these models pan out; I think we can include either way```

#Data


##Polygenic Risk Scores

We follow the procedure outlined in Dominigue, et al. [-@domingue_genetic_2016] to calculate our polygenic risk scores. First, we match SNPs in the HRS to SNPS in the GWAS conducted by Okbay et al. [-@okbay_genetic_2016] available through the Social Science Genetic Association Consortium. Second, we sum the weighted estimates of well-being associated alleles from the GWAS to calculate a polygenic score. Third, we adjust the residulized PGS using 10 principal components and PLINK (command: pca), and normalize the PGS residual distribution, which we use as the measure of genetic predisposition to well-being.

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

