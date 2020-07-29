## --------------------------------------------
## Helper functions
## --------------------------------------------

# emsvars_temp <- c(
#   "susceptible_EMS.", "exposed_EMS.", "asymptomatic_EMS.", "symptomatic_mild_EMS.", "symptomatic_severe_EMS.",
#   "hospitalized_EMS.", "critical_EMS.", "deaths_EMS.", "recovered_EMS.", "asymp_cumul_EMS.",
#   "asymp_det_cumul_EMS.", "symp_mild_cumul_EMS.", "symp_severe_cumul_EMS.", "hosp_cumul_EMS.", "hosp_det_cumul_EMS.",
#   "crit_cumul_EMS.", "crit_det_cumul_EMS.", "crit_det_EMS.", "death_det_cumul_EMS.", "infected_EMS.",
#   "infected_cumul_EMS.", "symp_mild_det_cumul_EMS.", "symp_severe_det_cumul_EMS.", "detected_EMS.", "detected_cumul_EMS.",
#   "presymptomatic_EMS."
# )


# getdata
getdata <- function(dat, selected_ems) {
  #'  getdata is per default using the "trajectoriesDat" dataset to perfom specific data editing
  #'  Outcome variables of interest are selected for one or more EMS regions as specified in selected_ems
  #'  If multiple integers are included in the selected_ems, the data will be aggregated
  #'  The data is filtered by date to include only dates within the defined evaluation window
  #'  Factor variables per contact tracing parameter are generated to
  #'  optionally facilitate plotting or custom tables for data exploration
  #' @param dat  simulation dataset in wide format
  #' @param selected_ems  vector of integers representing one or multiple EMS areas


  # emsvars <- colnames(trajectoriesDat)[grep("[.]",colnames(trajectoriesDat))]
  emsvars_temp <- c("critical_EMS.","N_EMS_","Ki_EMS_" )

  emsvars <- NULL
  for (ems in selected_ems) {
    emsvars <- c(emsvars, paste0(emsvars_temp, ems))
  }

  groupvars <- c("startdate","Date","time", "backtonormal_multiplier", "scen_num", "sample_num", "run_num", "backtonormal_multiplier", "detection_success", "isolation_success", "grpvar"  )
  (keepvars <- c(groupvars, emsvars))

  subdat <- dat %>% dplyr::select(keepvars)

  subdat <- subdat %>%
    pivot_longer(cols = -c(groupvars)) %>%
    dplyr::mutate( name = gsub("All", "EMS.IL", name)
    ) %>%
    dplyr::mutate(name = gsub("[.]", "_", name)) %>%
    separate(name, into = c("outcome", "region"), sep = "_EMS_") %>%
    dplyr::filter(Date >= reopeningdate) %>%
    dplyr::select(-c(time))
  
  
  subdat1 <- subdat %>%
    filter(outcome %in% c("N", "Ki")) %>%
    pivot_wider(names_from = outcome, values_from = value)
  
  
  mergevars <- colnames(subdat)[colnames(subdat) %in% colnames(subdat1)]
  subdat <- subdat %>%
    filter(!(outcome %in% c("N", "Ki"))) %>%
    left_join(subdat1, by = mergevars)

  if (length(selected_ems) > 1) {
    groupvars <- c(groupvars[groupvars != "time"], "outcome", "Date", "scen_num")

    subdat <- subdat %>%
      dplyr::group_by_at(.vars = groupvars) %>%
      dplyr::summarize(
        value = sum(value),
        N = sum(N),
        Ki = mean(Ki)
      )
  }


  return(subdat)
}

# region_to_fct
region_to_fct <- function(dat, geography) {
  #' Helper function to convert region character variable to factor variable
  #' @param dat  dataset
  #' @param geography  level of analysis EMS or Region (aggregated or not)


  if (geography == "EMS") {
    levels <- c(1:11)
    labels <- paste0("EMS ", levels)
  }

  if (geography == "Region") {
    levels <- c(names(regions)[5], names(regions)[-5])
    labels <- stringr::str_to_title(levels)
  }

  dat$region <- factor(dat$region,
    levels = levels,
    labels = labels
  )
  return(dat)
}
