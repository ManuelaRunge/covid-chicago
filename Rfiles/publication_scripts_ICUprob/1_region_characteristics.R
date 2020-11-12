#### --------------------------------------
#### 1- Region characteristics -> region_characteristics.csv
#### --------------------------------------

dat <- f_region_characteristics()
summary(dat$icubeds_per10th)
print(dat)
if(SAVE)fwrite(dat, file.path(outdir,"tables", "region_characteristics.csv"), quote=FALSE)

