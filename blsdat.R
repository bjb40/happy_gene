##looks at previously collect bls data

#load
dats = lapply(list.files(outdir,full.names=TRUE,pattern='blsdat[[:digit:]]*.csv'),read.csv)

#concat, del duplicates, and save
blsdat = unique(bind_rows(dats))

blsdat %>% group_by(period,year,region)

save(blsdat,file=paste0(outdir,'blsdat.RData'))