trajectoriesDat <- read_csv(file.path(simulation_output, exp_name, "trajectoriesDat.csv"))

### Note use Ki_EMS-  or Ki_EMS.   not Ki_EMS_  (time varying vs input parameter)
# colnames(trajectoriesDat) <- gsub("[.]", "-", colnames(trajectoriesDat))

region_names =c("All", paste0("EMS-",c(1:11)))
outcomevars <- c(
  paste0("deaths_", region_names),
  paste0("deaths_det_", region_names),
  paste0("hosp_cumul_", region_names),
  paste0("hosp_det_cumul_", region_names),
  paste0("hosp_det_", region_names),
  paste0("hospitalized_", region_names),
  paste0("infected_", region_names),
  paste0("prevalence_", region_names),
  paste0("crit_cumul_", region_names),
  paste0("crit_det_cumul_", region_names),
  paste0("crit_det_", region_names),
  paste0("critical_", region_names)
)


region_names =paste0("EMS-",c(1:11))
outcomevars2 <- c(
  paste0("Ki_t_", region_names)
)



paramvars <- c("reopening_multiplier_4","change_testDelay_Sym_1","change_testDelay_As_1", "d_Sym_ct1", "d_AsP_ct1", "reduced_inf_of_det_cases_ct1")
keepvars <- c("time", "startdate", "scen_num","sample_num",  paramvars, outcomevars,outcomevars2)



timdat <- trajectoriesDat %>%
  select(keepvars) 


write.csv(timdat, file.path(simulation_output, exp_name, "trajectoriesDat_trim.csv"))
