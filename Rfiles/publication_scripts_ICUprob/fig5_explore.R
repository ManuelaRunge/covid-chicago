
### relative plot, how much does delay and strengths matter?
ggplot(data = subset(tab_peak, delay == "3 days")) +
  geom_hline(yintercept = Inf) +
  geom_vline(xintercept = Inf) +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.5) +
  geom_errorbar(aes(x = capacity_multiplier_fct, ymin = q2.5.aboveICU, ymax = q97.5.aboveICU,col = region, 
                    group = interaction(region,delay, capacity_multiplier)), width = 0 ) +
  geom_line(aes(x = capacity_multiplier_fct, y = median.aboveICU, col = region, group = interaction(delay,geography_name))) +
  geom_point(aes(x = capacity_multiplier_fct, y = median.aboveICU, fill = region, group = interaction(delay, capacity_multiplier)),shape=21,size=2) +
  
  labs(
    subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
    # caption="Difference between predicted\npeak ICU cases and ICU availability",
    x = "Trigger threshold"
  ) +
 # facet_wrap(~ rollback + reopen, scales="free_x") +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.5))+
  customTheme





f_scatterplot_errorbars <- function(dflist = list_csvs,
                                        subregions = NULL, 
                                        rollback = "sm4", 
                                        reopen = "50perc", 
                                        exp_name_sub, 
                                        stackLike = FALSE) {
  library(dplyr)
  library(plyr)
  
  if (!exists("customTheme")) customTheme <- f_getCustomTheme()
  # customTheme <- f_getCustomTheme(fontscl = -3)
  
  dflist <- dflist[grep("regreopen", dflist)]
  #dflist <- dflist[grep(rollback, dflist)]
  #dflist <- dflist[grep(reopen, dflist)]
  
  tbl_fread = list()
  for(file in dflist){
    tbl_fread[[length(tbl_fread)+1]] <- fread(file.path(simulation_output, "_overflow_simulations", file)) %>%
      mutate(geography_name=as.character(geography_name)) %>%
      filter(geography_name %in% c(1,4,11))
  }
  tbl_fread <- tbl_fread %>% bind_rows()
  
  
  out <- f_describe_peak_and_cumul(dat = tbl_fread, subfolder = exp_name_sub, SAVE = FALSE)
  tab_peak <- out[[2]]
  
  tab_peak <- tab_peak %>%
    dplyr::mutate(exp_name_split = exp_name) %>%
    separate(exp_name_split, into = c("simdate", "locale", "reopen", "delay", "rollback"), sep = "_") %>%
    dplyr::mutate(
      reopen = gsub("regreopen", "", reopen),
      delay = gsub("daysdelay", " days", delay)
    )
  
  
  if (reopen == "50perc") selectedCols <- c("#c6dbef", "#6baed6", "#2171b5")
  if (reopen == "100perc") selectedCols <- c("#fee0d2", "#fb6a4a", "#cb181d")
  
  
  suppressWarnings(capacity_threshold <- tab_peak %>%
                     dplyr::filter(median.aboveICU >= 0) %>%
                     dplyr::filter(capacity_multiplier == min(capacity_multiplier)) %>%
                     dplyr::select(capacity_multiplier) %>%
                     unique() %>%
                     as.numeric())
  
  if (is.na(capacity_threshold)) capacity_threshold <- 1
  
  tab_peak$capacity_multiplier_fct <- factor(tab_peak$capacity_multiplier, levels= rev(unique(sort(tab_peak$capacity_multiplier))),
                                             labels = round(rev(unique(sort(tab_peak$capacity_multiplier)))* 100, 0))
  tab_peak$region <- factor(  tab_peak$geography_name , levels=c(1:11), labels=c(1:11))
    
  pplot_peak <- ggplot(data = subset(tab_peak, delay == "3 days")) +
    geom_hline(yintercept = Inf) +
    geom_vline(xintercept = Inf) +
    geom_hline(yintercept = 0, linetype = "solid", size = 0.5) +
    geom_errorbar(aes(x = capacity_multiplier_fct, ymin = q2.5.aboveICU, ymax = q97.5.aboveICU,col = region, 
                      group = interaction(region,delay, capacity_multiplier)), width = 0 ) +
    geom_line(aes(x = capacity_multiplier_fct, y = median.aboveICU, col = region, group = interaction(delay,geography_name))) +
    geom_point(aes(x = capacity_multiplier_fct, y = median.aboveICU, fill = region, group = interaction(delay, capacity_multiplier)),shape=21,size=2) +

    labs(
      subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
      # caption="Difference between predicted\npeak ICU cases and ICU availability",
      x = "Trigger threshold"
    ) +
    facet_wrap(~ rollback + reopen, scales="free_x") +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    theme(panel.grid.major.y = element_line(colour = "grey", size = 0.5))+
    customTheme
  
  
  
  pplot_peak <- ggplot(data = subset(tab_peak, delay == "3 days")) +
    geom_hline(yintercept = Inf) +
    geom_vline(xintercept = Inf) +
    geom_hline(yintercept = 0, linetype = "solid", size = 0.5) +
    geom_errorbar(aes(x = capacity_multiplier_fct, ymin = q2.5.aboveICU, ymax = q97.5.aboveICU,
                      col = reopen, 
                      group = interaction(region,delay, rollback, reopen, capacity_multiplier)), width = 0 ) +
    geom_line(aes(x = capacity_multiplier_fct, y = median.aboveICU, 
                  col = reopen, group = interaction(delay, rollback, reopen, geography_name))) +
    geom_point(aes(x = capacity_multiplier_fct, y = median.aboveICU, 
                   fill = reopen, group = interaction(delay, rollback, reopen, capacity_multiplier)),shape=21,size=2) +
    
    labs(
      subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
      # caption="Difference between predicted\npeak ICU cases and ICU availability",
      x = "Trigger threshold"
    ) +
    facet_wrap(~ region, scales="free") + 
    scale_fill_manual(values=TwoCols_con) +
    scale_color_manual(values=TwoCols_con) +
    #scale_fill_brewer(palette = "Dark2") +
   # scale_color_brewer(palette = "Dark2") +
    theme(panel.grid.major.y = element_line(colour = "grey", size = 0.5))+
    customTheme
  
  
  
  
  pplot_peak <- ggplot(data = subset(tab_peak, delay == "3 days" & rollback=="sm4")) +
    geom_hline(yintercept = Inf) +
    geom_vline(xintercept = Inf) +
    geom_errorbar(data = subset(tab_peak, delay == "3 days" & rollback=="sm4"),aes(x = capacity_multiplier_fct, ymin = q2.5.aboveICU, ymax = q97.5.aboveICU,
                      col = reopen, 
                      group = interaction(region,delay, rollback, reopen, capacity_multiplier)), 
                  width = 0 ,position = position_dodge(width=1)) +
    geom_bar(data = subset(tab_peak, delay == "3 days" & rollback=="sm4"),aes(x = capacity_multiplier_fct, y = median.aboveICU, 
                   fill = reopen, group = interaction(delay, rollback, reopen, capacity_multiplier)),
             stat="identity",position = position_dodge(width=1)) + 
    geom_bar(data = subset(tab_peak, delay == "3 days" & rollback=="sm7"),aes(x = capacity_multiplier_fct, y = median.aboveICU, 
                                                                              col = reopen, group = interaction(delay, rollback, reopen, capacity_multiplier)),
             stat="identity",position = position_dodge(width=1),fill=NA) + 
    geom_hline(yintercept = 0, linetype = "solid", size = 0.5) +
    labs(
      subtitle = "", y = expression(italic(ICU[predicted_peak] - ICU[capacity])),
      # caption="Difference between predicted\npeak ICU cases and ICU availability",
      x = "Trigger threshold"
    ) +
    facet_wrap( region ~reopen, scales="free", ncol=2) + 
    scale_fill_manual(values=rev(TwoCols_con)) +
    scale_color_manual(values=rev(TwoCols_con)) +
    #scale_fill_brewer(palette = "Dark2") +
    # scale_color_brewer(palette = "Dark2") +
    theme(panel.grid.major.y = element_line(colour = "grey", size = 0.5))+
    customTheme
  
  
  

  out <- list(tab_peak, pplot_peak)
  return(out)
}


ggsave(paste0("scatter_plot_region-1-4-11.png"),
       plot = pplot_peak, path = file.path(exp_dir), width = 10, height = 8, device = "png"
)
ggsave(paste0("scatter_plot_region-1-4-11.pdf"),
       plot = pplot_peak, path = file.path(exp_dir), width = 10, height = 8, device = "pdf"
)


ggsave(paste0("bar_plot_region-1-4-11.png"),
       plot = pplot_peak, path = file.path(exp_dir), width = 10, height = 12, device = "png"
)
ggsave(paste0("bar_plot_region-1-4-11.pdf"),
       plot = pplot_peak, path = file.path(exp_dir), width = 10, height = 12, device = "pdf"
)



