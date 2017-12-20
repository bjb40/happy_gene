
source('code/config~.R',echo=TRUE)

library(ggplot2); library(dplyr); library(scales)
library(qqman)

#load okbay gwas

gwas = read.table(
  list.files('H:/projects/happy_gene/gwas~/',full.names=TRUE),
  header=TRUE
  )

gwas = gwas[complete.cases(gwas),]

#load plotting theme for b-w presentation style--
source('H:/projects/proposal/r_study/code/themes.R',echo=TRUE)

png(filename=paste0(draftimg,'okbay_gwas1.png'),
    height=5.5,width=12,units='in',res=70)

p = manhattan(gwas,
              bp='position',p='P.value',snp='SNPID',chr='Chr')

dev.off()

#ggsave(paste0(draftimg,'okbay_gwas.png'),
#       bg='transparent',
#       height=5.5,width=12)