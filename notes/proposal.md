---
author: Bryce Bartlett
date: 5/20/2017
title: "Genetics and Subjective Well-being, Depression and/or Cognition Project Proposal"
csl: ./citations/jshb.csl
bibliography: ./citations/happy_gene.bib
---

#Introduction

This document outlines a proposal for three separate gene/environment studies using the HRS on subjective well-being/Depression. Broadly, this study can include measures of life satisfaction, happiness, cognition, depression, substance abuse, and other psychological or internalized processes. Many of these were recently used in a large GWAS [@okbay_genetic_2016], which has been cross-applied to the HRS [@domingue_genetic_2016]. 

Proposed study 1 is a fairly direct extension of the work by Domingue, et al. [-@domingue_genetic_2016]. Domingue, et al. find that different rates of decay in elevated depressive symptoms after a spouse dies are correlated with polygenic scores (PGS) calculated from the GWAS mentioned above. This proposal anticipates an identical design, but instead of using spousal death as the exogenous environmental treatment, it will use variability in economic conditions (unemployment rate, the great recession, and perhaps exposure to recessive periods in the past).

Proposed study 2 addresses the long-term problem of understanding why there is a gendered difference in depression between men and women. Explanations include biological differences between men and women, differences in the level of exposure to stressful events leading to depression, and gendered responses to stress, which postulates that women are prone to internalizing disorders (like depression) while men are prone to externalizing disorders (like substance abuse). Notably, the social environment regarding gender has changed dramatically over the past several decades [@pampel_cohort_2011]. This provides an opportunity to examine population heterogeneity in gene-by-environment interactions in a manner similar to Boardman, *et al.* [-@boardman_trends_2010], and to test larger theories for the sources of population heterogeneity in gene-by-environment interactions as summarized in that work (social control, social trigger, and social push).

Study 3 addresses polygenic of depression and subjective well-being with cognition. For may years, studies have linked depression and cognitive processes with depression and well-being. Underlying differences in well-being is *comparative* and *counterfactual* thinking. For depression it is the *perception* of support and disadvantage, and not the objective measure of support and disadvantage that is most important [@george_still_2010]. Moreover, there are regularized life-course shapes to subjective well-being and age. These regularized patterns are often explained by regularized changes in cognitive assessments of life, *i.e.* people become happier and less depressed with age, because they engage in more beneficial cognitive comparisons [@george_still_2010]. While large studies of inter genetic correlation show only a weak (and not statistically significant) correlation between major depression and Alzheimer's disease [@bulik-sullivan_atlas_2015] it is possible (and perhaps likely) that subclinical manifestations of the diseases will be more strongly correlated.

The next section discusses each of the proposed studies in more detail.

#Proposed Studies

##Proposal 1: Heterogeneous Sensitivity to Short Term Economic Conditions

The first study is a simple test of genetic vulnerability or resistance to depressive events. This has been characterized in some corners as the "diathesis-stress" model, but crosses disciplinary boundaries and is a vulnerability and resilience model. Studies have found inconsistent results using both candidate gene models and polygenic score models.

A great example of a simple and straightforward study along these lines is Dominigue, et al. [-@domingue_genetic_2016], which finds genetic robustness to the death of a spouse. They use a very straightforward approach. (1) They match SNPs in the HRS to SNPS in the GWAS conducted by Okbay et al. [-@okbay_genetic_2016] available through the Social Science Genetic Association Consortium. (2) They sum the weighted estimates of well-being associated alleles from the GWAS to calculate a polygenic score. (3) They adjusted the residulized PGS using 10 principal components and PLINK (command: pca), and normalized the PGS residual distribution, which they used as the measure of genetic predisposition to well-being. They then used a discontinuity regression design centered on the loss of a spouse, and find faster decay in depressive symptoms for those with higher PGS for well-being, concluding that the PGS represents a genetic robustness to negative stressors.

In this case, I would propose constructing the polygenic score in an identical manner to Domingue, *et al.* (Dan Belsky tells me that this can occur given a simple request; and it is a request that he is willing to make for us). Alternatives also include testing of candidate genes (table of most-studied candidates under proposal 2), or building our own polygenic score out of the HRS. Instead of the extreme distress of spousal death, I propose modeling the association with short-term fluctuations in the unemployment rate, and/or the experience of a recession in young adulthood. This builds on previous work of mine that does not include biomarkers, which is currently drafted and under review. It is also similar to work that Conley [-@conley_promise_2009] proposed several years ago regarding larger economic variation as exogenous environmental treatments that can elucidate gene-environment interactions. 

Short-term fluctuations in well-being (measured by both satisfaction and depression) follow short-term differences in economic circumstances (as measured by the unemployment rate). My study was conducted in the General Social Survey; I have replicated the study in the HRS (using depressive symptoms), and there is an effect of short-term unemployment rates on depressive symptoms. The question is whether these short-term effects are distributed across genetic endowments. Moreover, do gene effects indicate a buffer or a sensitivity? Do they vary across positive and negative stimuli? Are there thresholds? These are basic questions with little literature to suggest specific hypotheses.

##Proposal 2: Heterogeneity in Genetic Correlates of Gendered differences in Depressive Symptoms by Cohort.

A number of consistent patterns in psychological well-being are related to gender. First and foremost, women report higher prevalence of affective disorders like Major Depressive Disorder, whether measured continuously or dichotomously [@aneshensel_social_1991; @kessler_prevalence_2005; @seedat_cross-national_2009]. In contrast, men report higher prevalence of substance abuse disorders such as alcoholism and drug addiction [@simon_revisiting_2002]. Explanations for these differences include (1) fundamental biological differences; (2) differential exposure to stress across genders; (3) differential errors in diagnosis across genders; and (4) differentials in gendered responses to stress across gender, *i.e.* women use internalizing strategies, while men use externalizing strategies.

A recent review in psychology provides a very good overview of this literature, including a summary (and references) to genomic studies in this area [@kuehner_why_2017]. The following is a list of candidate genes which have been investigated for gendered differences (original studies referenced in the review). 

**Candidate Genes**

|Gene|Summary|
|:---|:------|
| 5-HTTLPR | Most-studied, including meta-analysis of 78 studies. Shows differences in externalizing & internalizing propensities, and their are different distributions of a "short" allele frequency in a promoter gene across gender|
| FKBP5 | including SNP rs1360780 --- involved in the stress system|
| CRHR1 | including SNP rs110402 --- protective effect related to childhood trauma |

The review notes that the studies are still sparse and inconsistent, particularly because genetic factors appear to be pleiotropic. The review does not note any studies regarding polygenic risk scores, despite the fact that the pleiotropy and the complexity of the trait suggests that this is perhaps a more reasonable approach as explained by Belsky and Israel [-@belsky_integrating_2014]. In addition to the failure to use a broader strategy, prior research has also ignored possible heterogeneity in gendered social structures across various cohorts.

An instructive illustration of the potential importance of heterogeneity in social structures across cohorts is found in Boardman *et al.* [@boardman_trends_2010]. That study presents evidence of differences in gene-environment interactions across cohorts related to the Surgeon General's warning about smoking in 1964. Before and after this period, there was a great deal of change in the population prevalence of smoking. Similarly (though perhaps not as dramatic), there were also significant changes in gendered institutions, including a civil rights act outlawing gender discrimination in 1963, and an important law on gender equality in higher education (Title IX) in 1972. These co-occur with large changes in attitudes about gender egalitarianism around the same time [@pampel_cohort_2011], although structural changes have still lagged behind to some degree. 

There are three potential effects of these sorts of social environments on the ability to identify genetic correlates of complex behavior [@boardman_trends_2010]. First, is the idea of a *Social Trigger.* In this case, genetic differences occur only in the presence of norms. With respect to gender differences in subjective well-being, this effect is predicted by "Sex Role Theory". More than forty years ago Gove and Tudor [-@gove_adult_1973] proposed sex role theory as an explanation for th e gender discrepancy in mental health. Gove and Tudor hypothesized that gendered differences in psychological well-being arose out of women's restriction to a very narrow set roles: primarily wife and mother. This was more true for the oldest cohorts, and less true for younger cohorts. To the extent the theory is correct, we should find evidence of social trigger (*i.e.* gendered effects in the gene-by-environment interaction) for older cohorts, but not for younger cohorts. This seems to be the strongest theoretical hook.

Second, is the phenomenon of *Social Control.* In this case, norms eliminate genetic differences by requiring homogeneity or heterogeneity in individuals regardless of personal (or biological) preference. Unlike smoking (a behavior), there is little to suggest a social control phenomenon, because subjective well-being is a reported feeling. To the extent social control eliminates genetic correlates, it is likely to be related to gendered reporting or recognition of symptoms (something that would be difficult to measure).

Finally is the concept of *Social Push.* This theory is non-causal, and suggests that social norms either minimize or generate noise of the presence of genetic effects. For example, if there is a genetic cause of smoking, but everyone smokes, then there is no statistical leverage to find the genetic effect (because there is no variance in the behavior). This difference between this and social control is the concept of cause. Accordingly, similar evidence and analysis are potentially applicable for social push versus social trigger.

Boardman, et al. use a twin study in the MIDUS data to calculate heritability of smoking, so their study design does not fit well with the HRS. However, there are some simple designs that might work. The first design would analyze the effect (if any) of the candidate genes listed above across birth cohorts. Similarly, we could calculate polygenic scores and identify whether they are stronger or weaker over various birth cohorts. The problem with the PGS approach, however, is how to account for heterogeneity across cohorts in the PGS score (which by design includes biases related to the gene-by-environment interaction). To pursue this study, I would suggest a thorough review of the manner polygenic scores are (and could be) calculated. This may represent an opportunity for a methodological advancement, depending on the state of the literature regarding the development of polygenic risk scores.

##Proposal 3: Cross-Association of Cognition and Subjective Well-Being

The final proposal is more exploratory, and relates to fundamental assumptions on the relationship between subjective well-being and cognition in social psychology. Simply put, models of subjective well-being presuppose that it rests on a cognitive framework. In other words, people feel good or poor *after* evaluating their situation, and the meaning of their situation ```[@]```.

3. Cross association with cognition (some GWAS for cognition at SSGA). GxG interaction and/or Something demographic, like "happy" LE. Associations between happiness and ADL/IADL. Effect of PGS on buffering this sort of stuff. Multistate LE: happiness, cognition (low SWB is a cognitive process...).

SWB is bound up with cognition. 

#Reference

