--
author: Bryce Bartlett
date: July 12, 2017
title: "Summary of my discussion with Fang Fang re pipeline"
---

#General

1. Run GWAS
2. Output Candidate SNPS
3. Calculate Polygenic Score
4. Every time you run pipeline, you should delete the "project" folder and the "results" folder.
5. "Annotation" is for genes and will apply regardless of the chosen phenotype. 

#Quality Control (plink can do)

1. This is identified by the command "geno_cut". Identifing an integer between 0 and 1 specifies tolerance to missing snps. A value of 0 means that only SNPs with full data will be used. A value of 0.05 (standard) is that only SNPs with 5% or less missing data will be used.

#Annotation

1. There are two methods of annotation. The best one to use is with the option 'NEW'.
2. The "-20" in SAS is to binarize the data. Must use a positive number if you have a continuous variable.
3. You must keep the "AGE CUTOFF" statement. It will not be used for many analyses, but it is necessary to prevent SAS from breaking.
4. Annotation annotates genes only. Needs run only once --- regardless of phenotypes.

#Genetic Control and data

1. You want GC lambda to equal 1
2. PCA 3 methods: TAG (uses plink), ALL (uses plink), PCA (eigensoft --- use this one)

#"Other Pheno"
This is for cox regression and deals with truncation --- not necessary for linear or logistic, but the variables must be included or pipeline will not work.


#Source Data

1. Use SAS.
2. Start by splitting by gender

#Process

1. First run = annotate + grad total
2. Run Eigensoft PCA
3. Run PCA=0 and include pca variables in covariate list
4. Enjoy your results!!

Misc:
- Run pipeline through BASH
- Type "screen" to save terminal.
- Priority = "nice -10"
- Ref for genes = GWAS DBI

