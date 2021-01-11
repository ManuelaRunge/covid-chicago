### Rscrpt to zip the main files from multiple experiments into separate experiment subfolders for transferring them to Box.
### Note, the simulations and trajectories folder are not zipped and should be deleted from the _temp folder.

## Common pattern across experiments to transfer
stem = "reopen2"
sim_output_dir = "/projects/p30781/covidproject/covid-chicago/_temp/"
exp_names <- list.files(sim_output_dir)[ grep(stem, list.files(sim_output_dir))]
#exp_names <- exp_names[!grepl(paste0(stem,"$"),exp_names)]

foldertozipI <- file.path(sim_output_dir, stem)
if (!dir.exists(foldertozipI)) dir.create(foldertozipI)

for (exp_name in exp_names) {
  expdir <- file.path(sim_output_dir, exp_name)
  filestozip <- c("trajectoriesDat_region_1.csv","trajectoriesDat_region_4.csv","trajectoriesDat_region_11.csv",
  "trajectories_aggregated_EMS-1.csv","trajectories_aggregated_EMS-4.csv","trajectories_aggregated_EMS-11.csv", "trajectories_aggregated.csv")     
    
  #filestozip <- list.files(expdir, pattern = "hospitaloverflow", full.names = F)
  #filestozip <- c(filestozip,"peak_exceed_df.csv","trajectories_aggregated.csv","Ki_dat_All.csv")
  #filestozip <- c("trajectoriesDat_region_1.csv","trajectoriesDat_region_4.csv","trajectoriesDat_region_11.csv")
  filestozip <- c(filestozip, list.files(expdir, pattern = "sampled_parameters", full.names = F))
  filestozip <- c(filestozip, list.files(expdir, pattern = "yaml", full.names = F))
  filestozip <- c(filestozip, list.files(expdir, pattern = "emodl", full.names = F))
  #filestozip <- c("Ki_dat_All.csv","peak_exceed_df.csv","trigger_peak_exceed_df.csv")          
 
          
  print(exp_name)
  #print(filestozip)

  foldertozipII <- file.path(foldertozipI, exp_name)
  if (!dir.exists(foldertozipII)) dir.create(foldertozipII)
  file.copy(file.path(expdir, filestozip), file.path(foldertozipII, filestozip))
}

setwd(sim_output_dir)
filestozipAll <- list.files(foldertozipI, recursive = T)
zip(zipfile = file.path(paste(stem, ".zip")), files = filestozipAll)

#unlink(foldertozipI, recursive = TRUE, force = TRUE)
