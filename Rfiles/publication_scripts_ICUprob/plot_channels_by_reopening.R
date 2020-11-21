
library(tidyverse)
library(cowplot)
library(scales)
library(viridis)
library(data.table)

source("load_paths.R")
source("processing_helpers.R")

theme_set(theme_cowplot())

exp_name <- "20201120_IL_mr_gradual_reopening2_Sep"  
simdate <- strsplit(exp_name, "_")[1]
simtoday <- as.Date("2020-10-01") 
stopdate <- as.Date("2021-12-31") 

exp_dir <-  file.path(simulation_output,'_overflow_simulations', exp_name)
plot_dir <- file.path(exp_dir, "_plots")
if(!dir.exists(plot_dir))dir.create(plot_dir)

#region_names <- c("All", paste0("EMS-", c(1:11)))
region_names <- c( paste0("EMS-", c(1:11)))
outcomevars <- c(
  paste0("hosp_det_", region_names),
  paste0("crit_det_", region_names),
  paste0("hosp_det_cumul_", region_names),
  paste0("crit_det_cumul_", region_names),
  paste0("deaths_", region_names)
)

region_names <- paste0("EMS-", c(1:11))
outcomevars2 <- c(
  paste0("Ki_t_", region_names)
)

paramvars <- c("reopening_multiplier_4")
keepvars <- c("time", "startdate", "scen_num", paramvars, outcomevars)

if(!file.exists(file.path(exp_dir, "trajectoriesDat.csv"))){
  count=0
  for(i in c(1:11)){
    count=count+1
    load(file.path(exp_dir,paste0("trajectoriesDat_region_",i,".RData")))
    if(count ==1)trajectoriesDat <- subdat
    if(count >1)trajectoriesDat <- left_join(trajectoriesDat, subdat, by=c('time',  'startdate' ,'scen_num','sample_num', 'reopening_multiplier_4'))
    rm(subdat)
    fwrite(trajectoriesDat, file=file.path(exp_dir, "trajectoriesDat.csv"), quote = FALSE)
  }
}

trajectoriesDat <- fread(file.path(exp_dir, "trajectoriesDat.csv"), select = c(keepvars))


if (!(paramvars %in% colnames(trajectoriesDat))) {
  samplesDat <- read_csv(file.path(exp_dir, "sampled_parameters.csv"))
  mergevars <- colnames(samplesDat)[colnames(samplesDat) %in% colnames(trajectoriesDat)]
  trajectoriesDat <- left_join(trajectoriesDat, samplesDat, by = mergevars)
}

#### Summarize only crit_det and hosp_det, and after that pivot longer - too slow otherwise
keepvars <- c("time", "startdate",  paramvars, outcomevars)
paramvalues <- trajectoriesDat %>%
  dplyr::select(keepvars) %>%
  filter(time > 120) %>%
  dplyr::mutate(date = as.Date(startdate) + time) %>%
  pivot_longer(cols = -c("time", "date", "startdate", "reopening_multiplier_4"), names_to = "region") %>%
  separate(region, into = c("outcome", "region"), sep = "_EMS-") %>%
  mutate(
    exp_name = exp_name,
  ) %>%
  dplyr::group_by(date, region, exp_name, reopening_multiplier_4, outcome) %>%
  dplyr::summarize(
    median.value = median(value, na.rm = TRUE),
    min.value = min(value, na.rm = TRUE),
    max.value = max(value, na.rm = TRUE),
    q25.value = quantile(value, probs = 0.25, na.rm = TRUE),
    q75.value = quantile(value, probs = 0.75, na.rm = TRUE),
    q2.5.value = quantile(value, probs = 0.025, na.rm = TRUE),
    q97.5.value = quantile(value, probs = 0.975, na.rm = TRUE)
  )

table(paramvalues$outcome)

capacityDat <- load_new_capacity(filedate = "20200901") %>% mutate(region = as.character(geography_name))

table(paramvalues$region, exclude = NULL)
paramvalues$region[is.na(paramvalues$region)] <- "illinois"
paramvalues$region_label <- factor(paramvalues$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
capacityDat$region_label <- factor(capacityDat$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
paramvalues$reopening_multiplier_4 <- round(paramvalues$reopening_multiplier_4, 2) * 100

paramvalues <- paramvalues[!is.na(paramvalues$region_label), ]

paramvalues$outcome <- gsub("_All","",paramvalues$outcome )
dat <- paramvalues
save(dat, file = file.path(exp_dir, "aggregatedDat_forR.Rdata"))

# library(RColorBrewer)
# getPalette <- colorRampPalette(brewer.pal(9, "PuBuGn"))(12)

### ---------------------------------------
## Timeline plot
### ---------------------------------------

pplot <- paramvalues %>%
  filter(outcome == "crit_det" & date >= as.Date("2020-08-01") & date <= as.Date(stopdate)) %>%
  filter(region != "illinois") %>%
  # filter(reopening_multiplier_4  %in% c(0,22,44, 56, 78, 100)) %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  left_join(capacityDat) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4), 
                  group = reopening_multiplier_4), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4), 
                  group = reopening_multiplier_4), alpha = 0.3) +
  geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4), 
                group = reopening_multiplier_4), show.legend = F, size = 1) +
  facet_wrap(~region_label, scales = "free") +
  labs(color = "Reopening", fill = "Reopening") +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
  labs(x = "", y = "predicted ICU census") +
  customThemeNoFacet

ggsave(paste0("timeline_ICU_perCovidRegion.png"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("timeline_ICU_perCovidRegion.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot)



### Zoomed

pplot <- paramvalues %>%
  filter(outcome == "crit_det" & date >= as.Date(simtoday ) & date <= as.Date(stopdate)) %>%
  filter(region != "illinois") %>%
  filter(reopening_multiplier_4 >= 9) %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  left_join(capacityDat) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4), 
                  group = reopening_multiplier_4), alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4), 
                  group = reopening_multiplier_4), alpha = 0.3) +
  geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4), 
                group = reopening_multiplier_4), show.legend = F, size = 1) +
  facet_wrap(~region_label, scales = "free") +
  labs(color = "Reopening", fill = "Reopening") +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  geom_hline(aes(yintercept = icu_available), linetype = "dashed") +
  labs(x = "", y = "predicted ICU census") +
  customThemeNoFacet

ggsave(paste0("timeline_ICU_perCovidRegion_zoom.png"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("timeline_ICU_perCovidRegion_zoom.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot)


plotIL=FALSE
if(plotIL){
  pplot <- paramvalues %>%
    filter((outcome == "crit_det" | outcome == "crit_det_All") & date >= as.Date("2020-09-01") & date <= as.Date(stopdate)) %>%
    filter(region == "illinois") %>%
    ggplot() +
    geom_ribbon(aes(x = date, ymin = q2.5.value, ymax = q97.5.value, fill = as.factor(reopening_multiplier_4), 
                    group = reopening_multiplier_4), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = q25.value, ymax = q75.value, fill = as.factor(reopening_multiplier_4), 
                    group = reopening_multiplier_4), alpha = 0.3) +
    geom_line(aes(x = date, y = median.value, col = as.factor(reopening_multiplier_4), 
                  group = reopening_multiplier_4), show.legend = F, size = 1) +
    labs(color = "Coverage", fill = "Coverage") +
    scale_color_viridis_d(option = "C") +
    scale_fill_viridis_d(option = "C") +
    # geom_hline(aes(yintercept = icu_available), linetype="dashed") +
    labs(x = "Reopening multiplier (%)", y = "Peak in ICU census until Jan 2020") +
    customThemeNoFacet
  
  ggsave(paste0("timeline_ICU_IL.png"),
         plot = pplot, path = file.path(plot_dir), width = 10, height = 5, device = "png"
  )
  rm(pplot)
}

###### REOPENING PLOT

paramvalues$region_label <- factor(paramvalues$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))
capacityDat$region_label <- factor(capacityDat$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))


pplot <- paramvalues %>%
  filter(date >= as.Date(simtoday) &  date <= as.Date("2020-12-31")&  outcome == "hosp_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat, by = c("region")) %>%
  mutate(belowCapacity = ifelse(median.value <= medsurg_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening_multiplier_4), ymin = q2.5.value, ymax = q97.5.value, col = as.factor(belowCapacity)), 
                width = 0.3) +
  geom_point(aes(x = as.factor(reopening_multiplier_4), y = median.value, fill = as.factor(belowCapacity)),
             shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = medsurg_available)) +
  labs(x = "Reopening multiplier (%)", 
       y = "Peak in ICU census until Dec 31st 2020",
       color="Below capacity") +
  customThemeNoFacet

ggsave(paste0("nonICU_reopening_perCovidRegion.png"),
  plot = pplot, path = file.path(plot_dir), width = 18, height = 8, device = "png"
)
ggsave(paste0("nonICU_reopening_perCovidRegion.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 18, height = 8, device = "pdf"
)
rm(pplot)



pplot <- paramvalues %>%
  filter(date >= as.Date(simtoday) & date <= as.Date(stopdate) &  outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0)) %>%
  ggplot() +
  geom_errorbar(aes(x = as.factor(reopening_multiplier_4), ymin = q2.5.value, ymax = q97.5.value, col = as.factor(belowCapacity)), 
                width = 0.3) +
  geom_point(aes(x = as.factor(reopening_multiplier_4), y = median.value, fill = as.factor(belowCapacity)), 
             shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x = "Reopening multiplier (%)", 
       y = "Peak in ICU census until Dec 31st 2020",
       color="Below capacity") +
  customThemeNoFacet

ggsave(paste0("ICU_reopening_perCovidRegion.png"),
  plot = pplot, path = file.path(plot_dir), width = 18, height = 8, device = "png"
)
ggsave(paste0("ICU_reopening_perCovidRegion.pdf"),
  plot = pplot, path = file.path(plot_dir), width = 18, height = 8, device = "pdf"
)
rm(pplot)


### Identify soft and hard reopening


### Generate prediction dataset
get_fit <- function(df=paramvalues, outputVar ="median.value"){
  

  df <- as.data.frame(df)
  df$outputVar <- df[, colnames(df)==outputVar]
  xnew <- seq(0, max(df$reopening_multiplier_4), 0.1)
  
  matdat_list <- list()
  for (region_i in unique(df$region)) {
    print(region_i)
    
    tempdat <- df %>% 
      filter(region == region_i) %>%
      filter(date >= as.Date(simtoday) & date <= as.Date(stopdate) & outcome == "crit_det") %>%
      group_by(region,reopening_multiplier_4) %>%
      filter(outputVar == max(outputVar)) %>%
      filter(outputVar >0) %>%
      mutate(x =reopening_multiplier_4 ,
             y =log(outputVar) )  
    
    # plot(tempdat$y, tempdat$x)
    dfModel <- lm(y~ poly(x,3), data=tempdat )
    
    t_matdat <- expand.grid(x = xnew)
    
    t_matdat <- as.data.frame(cbind(t_matdat, predict(dfModel, newdata = t_matdat, interval = "confidence")))
    t_matdat$region <- region_i
    t_matdat$icu_available <- capacityDat[capacityDat$region==region_i,"icu_available"]
    t_matdat[,outputVar] = exp(t_matdat$fit)
    
    matdat_list[[length(matdat_list) + 1]] <- t_matdat
    
    rm(tempdat, t_matdat)
  }
  
  fitDat <- matdat_list %>% bind_rows()
  return(fitDat)
  
}


fitDat_median <- get_fit(df=paramvalues, outputVar ="median.value") %>% dplyr::rename(reopening_multiplier_4=x) %>% dplyr::select(-c(fit ,lwr ,upr))
fitDat_lwr <- get_fit(df=paramvalues, outputVar ="q2.5.value") %>% dplyr::rename(reopening_multiplier_4=x) %>% dplyr::select(-c(fit ,lwr ,upr))
fitDat_upr <- get_fit(df=paramvalues, outputVar ="q97.5.value") %>% dplyr::rename(reopening_multiplier_4=x) %>% dplyr::select(-c(fit ,lwr ,upr))

mergevars <- colnames(fitDat_median)[colnames(fitDat_median) %in% colnames(fitDat_lwr)]
fitDat <- fitDat_median %>% left_join( fitDat_lwr,  by=mergevars) %>%
          left_join(fitDat_upr, by=mergevars)

pplot  <- ggplot(data=fitDat) + 
  geom_ribbon(aes(x =reopening_multiplier_4,ymin=q2.5.value, ymax=q97.5.value, fill=region), alpha=0.3) + 
  geom_line(aes(x =reopening_multiplier_4, y = median.value, col=region))+
  facet_wrap(~region, scales="free")+
  theme(legend.position = "none")
 
ggsave(paste0("ICU_reopening_regressionPredict.png"),
       plot = pplot, path = file.path(plot_dir), width = 10, height = 8, device = "png"
)
ggsave(paste0("ICU_reopening_regressionPredict.pdf"),
       plot = pplot, path = file.path(plot_dir), width = 10, height = 8, device = "pdf"
) 
rm(pplot)

tdat <-  paramvalues %>%
  filter(date >= as.Date(simtoday) & date <= as.Date(stopdate) &  outcome == "crit_det") %>%
  group_by(region, reopening_multiplier_4, outcome) %>%
  filter(median.value == max(median.value)) %>%
  left_join(capacityDat) %>%
  mutate(belowCapacity = ifelse(median.value <= icu_available, 1, 0))

fitDat$region  <- as.numeric(fitDat$region )
fitDat$region <- factor(fitDat$region , levels=c(1:11), labels=c(1:11))

tdat$region  <- as.numeric(tdat$region )
tdat$region <- factor(tdat$region , levels=c(1:11), labels=c(1:11))


pplot <- ggplot(data=subset(fitDat,!is.na(region))) +
  geom_ribbon(aes(x =reopening_multiplier_4,ymin=q2.5.value, ymax=q97.5.value), alpha=0.3) + 
   geom_line(aes(x =reopening_multiplier_4, y = median.value), size=1) +
  geom_point(data=subset(tdat,!is.na(region)),aes(x =reopening_multiplier_4, y = median.value, fill = as.factor(belowCapacity)), 
             shape = 21, show.legend = F, size = 2) +
  facet_wrap(~region, scales = "free") +
  scale_fill_manual(values = c("indianred", "deepskyblue3")) +
  geom_hline(aes(yintercept = icu_available)) +
  labs(x = "Reopening multiplier (%)", 
       y = "Peak in ICU census until Dec 31st 2020",
       color="Below capacity") +
  customThemeNoFacet

ggsave(paste0("ICU_reopening_fit_perCovidRegion.png"),
       plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "png"
)
ggsave(paste0("ICU_reopening_fit_perCovidRegion.pdf"),
       plot = pplot, path = file.path(plot_dir), width = 14, height = 8, device = "pdf"
)
rm(pplot)


soft_reopen <- fitDat %>%
  dplyr::group_by(region) %>%
  dplyr::filter(median.value >= icu_available)  %>%
  dplyr::filter(reopening_multiplier_4 == min(reopening_multiplier_4, na.rm = TRUE))   %>%
  dplyr::mutate(reopen_soft = reopening_multiplier_4/100) %>%
  dplyr::select(region, reopen_soft)

hard_reopen <- fitDat %>% 
  dplyr::group_by(region) %>%
  dplyr::filter(q2.5.value >= icu_available)  %>%
  dplyr::filter(reopening_multiplier_4 == min(reopening_multiplier_4, na.rm = TRUE))  %>%
  dplyr::mutate(reopen_hard = reopening_multiplier_4/100) %>%
  dplyr::select(region, reopen_hard)


custom_reopen <- left_join(soft_reopen,hard_reopen, by="region" ) %>% arrange(region)

sink(file=file.path(exp_dir, "custom_reopen.txt"))
print(custom_reopen)
sink()

combinedPlot <- T
if (combinedPlot) {
  
  missingDat <- paramvalues %>%
    ungroup() %>%
    dplyr::select(region) %>%
    unique() %>%
    mutate(reopenNA = NA)


  pdat1 <- paramvalues %>%
    filter(date >= as.Date(simtoday) & outcome == "hosp_det") %>%
    dplyr::group_by(region, reopening_multiplier_4, outcome) %>%
    filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    filter(median.value <= medsurg_available) %>%
    filter(reopening_multiplier_4 == max(reopening_multiplier_4)) %>%
    filter(date == min(date))

  pdat2 <- paramvalues %>%
    filter(date >= as.Date(simtoday) & outcome == "crit_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    filter(median.value <= icu_available) %>%
    filter(reopening_multiplier_4 == max(reopening_multiplier_4)) %>%
    filter(date == min(date)) %>%
    dplyr::group_by(region)


  pdat1 <- missingDat %>%
    left_join(pdat1, by = c("region")) %>%
    mutate(
      reopenNA = reopening_multiplier_4,
      reopening_multiplier_4 = ifelse(is.na(reopening_multiplier_4), 0, reopening_multiplier_4)
    )

  pdat2 <- missingDat %>%
    left_join(pdat2, by = c("region")) %>%
    mutate(
      reopenNA = reopening_multiplier_4,
      reopening_multiplier_4 = ifelse(is.na(reopening_multiplier_4), 0, reopening_multiplier_4)
    )

  table(pdat2$region, pdat2$reopening_multiplier_4, exclude = NULL)


  datRib <- rbind(pdat1, pdat2) %>%
    dplyr::group_by(region, outcome) %>%
    dplyr::summarize(maxReopen = max(reopening_multiplier_4))

  datRib$region_label <- factor(datRib$region, levels = c("illinois", c(1:11)), labels = c("illinois", c(1:11)))

  pplot <- ggplot(data = subset(datRib, !is.na(outcome) & region_label != "illinois")) +
    # geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, group = outcome), width = 0.3) +
    geom_bar(aes(x = as.factor(region_label), y = maxReopen, fill = as.factor(outcome), group = as.factor(outcome)),
      stat = "identity", position = "dodge", width = 0.8, show.legend = T, size = 2
    ) +
    labs(
      x = "Region",
      y = "Reopening multiplier",
      title = "Reopening tolerance for ICU per covid region\n",
      fill = "Resource type"
    ) +
    scale_color_manual(values = rev(c("orange", "deepskyblue3"))) +
    scale_fill_manual(values = rev(c("orange", "deepskyblue3"))) +
    background_grid() +
    customThemeNoFacet +
    theme(panel.grid.major.y = element_blank()) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf)) +
    scale_y_continuous(expand = c(0, 0))


  ggsave(paste0("reopening_perCovidRegion.png"),
    plot = pplot, path = file.path(plot_dir), width = 12, height = 8, device = "png"
  )
  ggsave(paste0("reopening_perCovidRegion.pdf"),
    plot = pplot, path = file.path(plot_dir), width = 12, height = 8, device = "pdf"
  )
  ########################################
}

getDatePlot <-T
if (getDatePlot) {
  ### GEt minimum date
  pdat1 <- paramvalues %>%
    # filter(date <= as.Date("2020-12-30") ) %>%
    filter(date >= as.Date(simtoday) & outcome == "hosp_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    # filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    filter(median.value >= medsurg_available) %>%
    filter(reopening_multiplier_4 == min(reopening_multiplier_4)) %>%
    filter(date == min(date))


  pdat2 <- paramvalues %>%
    # filter(date <= as.Date("2020-12-30") ) %>%
    filter(date >= as.Date(simtoday) & outcome == "crit_det") %>%
    group_by(region, reopening_multiplier_4, outcome) %>%
    # filter(median.value == max(median.value)) %>%
    left_join(capacityDat) %>%
    filter(median.value >= icu_available) %>%
    filter(reopening_multiplier_4 == min(reopening_multiplier_4)) %>%
    filter(date == min(date))


  datRib <- rbind(pdat1, pdat2)

  summary(paramvalues$date)
  summary(datRib$date)
  summary(pdat1$date)
  summary(pdat2$date)


  pplot <- ggplot(data = datRib) +
    geom_rect(xmin = as.Date(stopdate), xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "grey") +
    # geom_errorbar(aes(x = as.factor(reopening), ymin = q2.5.value, ymax = q97.5.value, group = outcome), width = 0.3) +
    geom_point(aes(x = date, y = reopening_multiplier_4, fill = as.factor(outcome), group = as.factor(outcome)),
      stat = "identity", position = position_dodge(width = 0.7), shape = 21, show.legend = T, size = 3
    ) +
    labs(
      x = "",
      y = "Reopening multiplier (%)",
      title = "Median date of capacity exceedance for ICU per covid region\n",
      fill = "Resource type"
    ) +
    facet_wrap(~region) +
    scale_color_manual(values = rev(c("orange", "deepskyblue3"))) +
    scale_fill_manual(values = rev(c("orange", "deepskyblue3"))) +
    background_grid() +
    customThemeNoFacet +
    # theme(panel.grid.major.y = element_blank()) +
    geom_hline(yintercept = c(-Inf, Inf)) +
    geom_vline(xintercept = c(-Inf, Inf))

  ggsave(paste0("date_of_overflow_perCovidRegion.png"),
    plot = pplot, path = file.path(plot_dir), width = 12, height = 8, device = "png"
  )
  ggsave(paste0("date_of_overflow_perCovidRegion.pdf"),
    plot = pplot, path = file.path(plot_dir), width = 12, height = 8, device = "pdf"
  )
}
