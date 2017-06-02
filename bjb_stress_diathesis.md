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

A great example of a simple and straightforward study along these lines is Dominigue, et al. [-@domingue_genetic_2016], which finds genetic robustness to the death of a spouse. They use a very straightforward approach. (1) They match SNPs in the HRS to SNPS in the GWAS conducted by Okbay et al. [-@okbay_genetic_2016] available through the Social Science Genetic Association Consortium. (2) They sum the weighted estimates of well-being associated alleles from the GWAS to calculate a polygenic score. (3) They adjusted the residulized PGS using 10 principal components and PLINK (command: pca), and normalized the PGS residual distribution, which they used as the measure of genetic predisposition to well-being. They then used a discontinuity regression design centered on the loss of a spouse, and find faster decay in depressive symptoms for those with higher PGS for well-being, concluding that the PGS represents a genetic robustness to negative stressors.

#Results

#Discussion and Conclusion

#References

