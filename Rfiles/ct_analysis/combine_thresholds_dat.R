
#### Combine critical 
f_combinecapacity <- function(){
  
  thresholdsfiles <- list.files(file.path(ems_dir), pattern = "ICUcapacity.csv", recursive = TRUE, full.names = TRUE)
  #lmthresholdsDat <- sapply(thresholdsfiles, read.csv, simplify = FALSE) %>%
  #  bind_rows(.id = "id")
  datlist <- list()
  for(i in c(1:length(thresholdsfiles))){
    temp <-  read.csv(thresholdsfiles[i])
    if(dim(temp)[1]<1)next
    temp$id <- thresholdsfiles[i]
    temp$region <- as.character(temp$region )
    datlist[[length(datlist)+1]] <- temp 
    
    
  }
  
  lmthresholdsDat <- datlist %>% bind_rows()
  if (dim(lmthresholdsDat)[1] <= 1) next
  colnames(lmthresholdsDat)[colnames(lmthresholdsDat)=="regin"] <- "region"
  lmthresholdsDat$id <- gsub(ems_dir, "", lmthresholdsDat$id)
  lmthresholdsDat$region <- gsub("_loess_ICUcapacity.csv", "", lmthresholdsDat$id)
  lmthresholdsDat$region <- gsub("[/]", "", lmthresholdsDat$region)
  table(  lmthresholdsDat$region )
  
  write.csv(lmthresholdsDat, file.path(exp_dir, "thresholds_loess.csv"), row.names = FALSE)
}




f_combinecapacity()






