---
author: Bryce Bartlett
 -affilitaion: "PhD Candidate (Sociology) Duke University, Social Science Research Institute, Duke Population Research Institute, Biodemography of Aging Research Unit"
date: 6/1/2017
title: "Heterogeneity in Vulnerability and Resilience to Stress from Changing Economic Conditions"
csl: ./citations/jshb.csl
bibliography: ./citations/happy_gene.bib
---

#Introduction

This study tests the genetic vulnerability or resistance to depressive events in the health and retirement study. We approach this in the framework of the diathesis-stress model, which suggests excess genes confer excess vulnerability to negative outcomes when exposed to stress. We also test an opposing theory, that genees confer excess resilience ot negative outcomes when exposed to stress. Combining data from the Health and Retirement Study (HRS) with information on regional unemployment rates from the Beureau of Labor Statistics (BLS), we test sensitivities to changing economic conditions between 2007 and 2012 as changes in depressive symptoms. Then, using a polygenic score from a large published Genome Wide Association Study (GWAS), we test whether individuals' reaction to the changing economic conditions vary with polygenic scores. Using white non-hispanic adults in the HRS (n=```#```), we find ```results```. 

#Background

Originating in psychology, the stress process model has been used across social science to explain the development of all sorts of adverse health events--including continuous measures of psychological distress and depressive symptoms which do not reach clinically diagnosable levels [@gayman_multiple_2013]. In its most basic formulation, the stress process model suggests that organisms respond negatively to negative stimuli from the environment [@pearlin_stress_1981]. Heterogeneity in negative responses depend on mechanisms that can either buffer or exacerbate the stress response. 





In this case, I would propose constructing the polygenic score in an identical manner to Domingue, *et al.* (Dan Belsky tells me that this can occur given a simple request; and it is a request that he is willing to make for us). Alternatives also include testing of candidate genes (table of most-studied candidates under proposal 2), or building our own polygenic score out of the HRS. Instead of the extreme distress of spousal death, I propose modeling the association with short-term fluctuations in the unemployment rate, and/or the experience of a recession in young adulthood. This builds on previous work of mine that does not include biomarkers, which is currently drafted and under review. It is also similar to work that Conley [-@conley_promise_2009] proposed several years ago regarding larger economic variation as exogenous environmental treatments that can elucidate gene-environment interactions. 

Short-term fluctuations in well-being (measured by both satisfaction and depression) follow short-term differences in economic circumstances (as measured by the unemployment rate). Late last week (5/18-5/19), I conducted some basic analyses combining the HRS (using depressive symptoms) and the BLS (for unemployment rates), and there is an effect of short-term unemployment rates on depressive symptoms: when unemployment rates go up or down, depressive symptoms follow. The question is whether these short-term effects are distributed across genetic endowments. Moreover, do gene effects indicate a buffer or a sensitivity? Do they vary across positive and negative stimuli? Are there thresholds? These are basic questions with little literature to suggest specific hypotheses.

#Data

#Analytic Strategy

For the model, I use both individual fixed effects, and a random effects model. While the ranodm effects model may be more biased, it is also more efficient. To minimize bias, but take advantage of efficiency, I use the "hybrid" model of the following form:

Level 1:

$$
y_{it} =  \alpha_i +  \delta (\bar{x}_{i \cdot} - x_{it}) + \sum_1^P \beta_{1p} a^p + \sum_1^Y \beta_{2y}
$$

Level 2:

$$
\alpha_i = \lambda z_i + \gamma \bar{x}_{i \cdot} + \epsilon_i
$$


Where $y_{it}$ is the value for individual $i$'s depressive symptoms at time $t$. $x$ is a matrix of time-varying variables, including regional unemployment rate, employment status, household income, and household wealth. This also includes coefficients for interactions between unemployment rate and unemployment status, because the main . $z$ is a matrix of fixed individual characteristics of gender. Age ($a$) is modeled as a polynomial function of rank $P$. In the models below, I present a quadratic effect. We also include a dummy variable series for the year ($y$) the interview is conducted. This hybrid model identifies the *within-person* effects ($\delta$), which indicate the predicted effect of a change in the relevant variable $x$ on any individual $i$ with respect to depressive symptoms. It also identifies the *between-person* effects at level 2, including a vector of effects for constant individual characteristics along with a constant (intercept) effect ($\lambda$) and the between-person effects of time-varying variables ($\gamma$). The random errors of level two across individuals ($\epsilon_i$) are assumed to be distributed normally with mean 0 and variance $\tau^2$ [@raudenbush_hierarchical_2002].

A great example of a simple and straightforward study along these lines is Dominigue, et al. [-@domingue_genetic_2016], which finds genetic robustness to the death of a spouse. They use a very straightforward approach. (1) They match SNPs in the HRS to SNPS in the GWAS conducted by Okbay et al. [-@okbay_genetic_2016] available through the Social Science Genetic Association Consortium. (2) They sum the weighted estimates of well-being associated alleles from the GWAS to calculate a polygenic score. (3) They adjusted the residulized PGS using 10 principal components and PLINK (command: pca), and normalized the PGS residual distribution, which they used as the measure of genetic predisposition to well-being. They then used a discontinuity regression design centered on the loss of a spouse, and find faster decay in depressive symptoms for those with higher PGS for well-being, concluding that the PGS represents a genetic robustness to negative stressors.

#Results


Hybrid model (environment):

|Estimate |Std..Error |t.value |effname            |
|:--------|:----------|:-------|:------------------|
|5.884    |0.332      |17.715  |(Intercept)        |
|-0.013   |0.015      |-0.869  |iuer               |
|0.604    |0.156      |3.873   |iret               |
|1.745    |0.209      |8.336   |iunemp             |
|0.033    |0.009      |3.637   |md_uer             |
|0.094    |0.026      |3.540   |md_ret             |
|0.305    |0.030      |10.203  |md_unemp           |
|0.262    |0.122      |2.155   |factor(iwendy)2003 |
|-0.027   |0.021      |-1.288  |factor(iwendy)2004 |
|-0.031   |0.140      |-0.224  |factor(iwendy)2005 |
|0.112    |0.023      |4.778   |factor(iwendy)2006 |
|0.360    |0.109      |3.304   |factor(iwendy)2007 |
|0.057    |0.024      |2.420   |factor(iwendy)2008 |
|0.151    |0.125      |1.213   |factor(iwendy)2009 |
|-0.057   |0.040      |-1.422  |factor(iwendy)2010 |
|0.024    |0.045      |0.544   |factor(iwendy)2011 |
|0.032    |0.031      |1.032   |factor(iwendy)2012 |
|-0.114   |0.024      |-4.722  |male               |
|-0.115   |0.009      |-12.922 |age                |
|0.001    |0.000      |11.144  |I(age^2)           |
|-0.660   |0.021      |-31.212 |marr               |
|0.015    |0.023      |0.657   |iuer:iret          |
|-0.038   |0.029      |-1.308  |iuer:iunemp        |
|0.013    |0.017      |0.739   |md_uer:md_ret      |
|-0.001   |0.021      |-0.036  |md_uer:md_unemp    |


#Discussion and Conclusion

#References

