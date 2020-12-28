library(tidyverse)
library(cowplot)
library(data.table)

source("load_paths.R")
source("setup.R")
source("processing_helpers.R")
source("publication_scripts_ICUprob/functions.R")

TwoCols_seq <- c("#00a79d", "#f7941d")
capacity_col <- "#2a3b90"
customTheme <- f_getCustomTheme()
theme_set(theme_minimal())
## -------------------------------
## Run script
## -------------------------------
capacityDat <- load_new_capacity(filedate = "20200915")
capacityDat <- capacityDat %>%
  filter(geography_name %in% c(1,4,11)) %>%
  rename(avg_resource_available=icu_available )
capacityDat$region <- factor(capacityDat$geography_name, levels = c(1,4,11), labels = paste0("Region ", c(1,4,11)))


simdate <-'20201212'
sim_dir <- file.path(simulation_output,'_overflow_simulations', simdate)
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots"))
if (!dir.exists(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))) dir.create(file.path(sim_dir, "ICU_trajectories_plots", "pdf"))

exp_names <- list.dirs(sim_dir, recursive = FALSE, full.names = FALSE)
exp_names <- exp_names[grep("IL_regreopen",exp_names)]
exp_names <- exp_names[c(grep("daysdelay",exp_names),grep("counterfactual",exp_names))]
exp_names <- exp_names[!(grepl("_reopen",exp_names))]
#exp_names <- exp_names[!(grepl("counterfactual",exp_names))]

hosp_files <- list.files(file.path(sim_dir), pattern="hospitaloverflow_randsub_100", recursive = TRUE)

datList <- list()
for( hfile in hosp_files){
  exp_name <- strsplit(hfile, "/" )[[1]][1]
  if(sum(grep("_reopen", exp_name))>0)next
  randsub <-gsub("[]]","", gsub("[[]","_", gsub(".csv","", gsub("hospitaloverflow_randsub_100_","",strsplit(hfile, "/" )[[1]][2]))))
  datList[[length(datList)+1]] <- fread(file.path(sim_dir,hfile))  %>% mutate(exp_name=exp_name,randsub=randsub )
}

dat <- datList %>% bind_rows()
rm(datList)

table(dat$exp_name)
dat <- dat %>% f_get_scenVars()
dat <- subset(dat, geography_modeled %in% c(1,4,11))
dat$region <- factor(dat$geography_modeled, levels = c(1,4,11), labels = paste0("Region ", c(1,4,11)))

datAggr <- dat %>% 
            group_by(region, exp_name,capacity_multiplier ) %>% 
            summarize(            min.value = min(prob, na.rm = TRUE),
                                  max.value = max(prob, na.rm = TRUE),
                                  median.value = median(prob, na.rm = TRUE),
                                  q25.value = quantile(prob, probs = 0.25, na.rm = TRUE),
                                  q75.value = quantile(prob, probs = 0.75, na.rm = TRUE),
                                  q2.5.value = quantile(prob, probs = 0.025, na.rm = TRUE),
                                  q97.5.value = quantile(prob, probs = 0.975, na.rm = TRUE)
                      ) %>% 
            f_get_scenVars()

tapply(datAggr$capacity_multiplier, datAggr$delay, summary)

p_rollback <- ggplot(data = subset(datAggr, delay == "7daysdelay" )) +
  theme_minimal() +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_ribbon(aes(x=capacity_multiplier * 100, 
                  ymin=min.value * 100 ,
                  ymax= max.value * 100, 
                  group=interaction(rollback,reopen_fct2),
                  fill=reopen_fct2 ),alpha=0.2) +
  geom_line(aes(x = capacity_multiplier * 100, 
                y = median.value * 100,
                group=interaction(rollback,reopen_fct2), 
                col=reopen_fct2, 
                alpha=rollback_fct), size = 1.1) +
  scale_y_continuous(lim = c(0, 101), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_x_continuous(lim = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values=c(1,0.8,0.6, 0.4))+
  facet_wrap( ~region, scales = "free") +
  customTheme +
  theme(
    panel.spacing = unit(2, "lines"),
    #legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)"
  )



p_delay <- ggplot(data = subset(datAggr, rollback == "pr6" )) +
  theme_minimal() +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_ribbon(aes(x=capacity_multiplier * 100, 
                  ymin=min.value * 100 ,
                  ymax= max.value * 100, 
                  group=interaction(delay,reopen_fct2),
                  fill=reopen_fct2 ),alpha=0.2) +
  geom_line(aes(x = capacity_multiplier * 100, 
                y = median.value * 100,
                group=interaction(delay,reopen_fct2), 
                col=reopen_fct2, 
                alpha=delay), size = 1.1) +
  scale_y_continuous(lim = c(0, 101), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_x_continuous(lim = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values=c(1, 0.5))+
  facet_wrap( ~region, scales = "free") +
  customTheme +
  theme(
    panel.spacing = unit(2, "lines"),
    #legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)"
  )


pplotall <- plot_grid(p_rollback, p_delay, ncol = 1, labels = c("A", "B"))


f_save_plot(
  plot_name = paste0("ICU_prob_withUncertainty"), pplot = pplotall,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 8
)




###------------------------
### Loess regression
###------------------------
library(purrr)
#https://stackoverflow.com/questions/50163106/loess-regression-on-each-group-with-dplyrgroup-by
models <- dat %>% 
  dplyr::select(region, exp_name, randsub,capacity_multiplier, prob) %>%
  tidyr::nest(-c(region, exp_name, randsub )) %>%
  dplyr::mutate(
    # Perform loess calculation on each CpG group
    m = purrr::map(data, loess,
                   formula = prob~capacity_multiplier , span = .55),
    # Retrieve the fitted values from each model
    fitted = purrr::map(m, `[[`, "fitted")
  )

# Apply fitted y's as a new column
results <- models %>%
  dplyr::select(-m) %>%
  tidyr::unnest(cols = c(data, fitted)) %>%
  f_get_scenVars() %>%
  mutate(fitted=ifelse(fitted<0, 0,fitted)) %>%
  mutate(fitted=ifelse(fitted>1,1,fitted)) 

ggplot(data=results)+
  geom_point(aes(x=prob, y=fitted))

summary(results$fitted)

resultsAggr <- results %>% 
  group_by(region, exp_name,capacity_multiplier ) %>% 
  summarize(            min.value = min(fitted, na.rm = TRUE),
                        max.value = max(fitted, na.rm = TRUE),
                        median.value = median(fitted, na.rm = TRUE),
                        q25.value = quantile(fitted, probs = 0.25, na.rm = TRUE),
                        q75.value = quantile(fitted, probs = 0.75, na.rm = TRUE),
                        q2.5.value = quantile(fitted, probs = 0.025, na.rm = TRUE),
                        q97.5.value = quantile(fitted, probs = 0.975, na.rm = TRUE)
  ) %>% 
  f_get_scenVars()


p_rollback <- ggplot(data = subset(resultsAggr, delay == "7daysdelay" )) +
  theme_minimal() +  
  geom_vline(xintercept = c(50),size=0.5, alpha=0.5, col="#b2b2b2") +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=50, ymax=Inf, fill="#b2b2b2", alpha=0.008)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=80, ymax=Inf, fill="#4c4c4c", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=60, ymax=80, fill="#595959", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=40, ymax=60, fill="#666666", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=20, ymax=40, fill="#737373", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=20, fill="#808080", alpha=0.05)+
  #background_grid()+
  # geom_vline(xintercept = seq(0,100,20),col="#b2b2b2", size=0.3, alpha=0.3) +
  #geom_hline(yintercept = seq(0,100,20),col="#b2b2b2", size=0.3, alpha=0.3) +
  geom_hline(yintercept = c(50),size=0.5, alpha=0.5, col="#b2b2b2") +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_ribbon(aes(x=capacity_multiplier * 100, 
                  ymin=min.value * 100 ,
                  ymax= max.value * 100, 
                  group=interaction(rollback,reopen_fct2),
                  fill=reopen_fct2 ),alpha=0.2) +
  geom_line(aes(x = capacity_multiplier * 100, 
                y = median.value * 100,
                group=interaction(rollback,reopen_fct2), 
                col=reopen_fct2, 
                alpha=rollback_fct), size = 1.1) +
  scale_y_continuous(lim = c(0, 102), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_x_continuous(lim = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values=c(1,0.8,0.6, 0.4))+
  facet_wrap( ~region, scales = "free") +
  customTheme +
  theme(
    panel.spacing = unit(2, "lines"),
    #legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)"
  )

p_rollback

p_delay <- ggplot(data = subset(resultsAggr, rollback == "pr6" )) +
  theme_minimal() +
  geom_vline(xintercept = c(50),size=0.5, alpha=0.5, col="#b2b2b2") +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=50, ymax=Inf, fill="#b2b2b2", alpha=0.008)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=80, ymax=Inf, fill="#4c4c4c", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=60, ymax=80, fill="#595959", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=40, ymax=60, fill="#666666", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=20, ymax=40, fill="#737373", alpha=0.05)+
  #geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=20, fill="#808080", alpha=0.05)+
  #background_grid()+
 # geom_vline(xintercept = seq(0,100,20),col="#b2b2b2", size=0.3, alpha=0.3) +
  #geom_hline(yintercept = seq(0,100,20),col="#b2b2b2", size=0.3, alpha=0.3) +
  geom_hline(yintercept = c(50),size=0.5, alpha=0.5, col="#b2b2b2") +
  geom_vline(xintercept = c(-Inf, Inf)) +
  geom_hline(yintercept = c(-Inf, Inf)) +
  geom_ribbon(aes(x=capacity_multiplier * 100, 
                  ymin=min.value * 100 ,
                  ymax= max.value * 100, 
                  group=interaction(delay,reopen_fct2),
                  fill=reopen_fct2 ),alpha=0.2) +
  geom_line(aes(x = capacity_multiplier * 100, 
                y = median.value * 100,
                group=interaction(delay,reopen_fct2), 
                col=reopen_fct2, 
                alpha=delay), size = 1.1) +
  scale_y_continuous(lim = c(0, 102), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_x_continuous(lim = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 20), minor_breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(TwoCols_seq)) +
  scale_fill_manual(values = c(TwoCols_seq)) +
  scale_alpha_manual(values=c(1, 0.5))+
  facet_wrap( ~region, scales = "free") +
  customTheme +
  theme(
    panel.spacing = unit(2, "lines"),
    #legend.position = "None",
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = 0.75)
  ) +
  labs(
    y = "Probability of ICU overflow (%)",
    x = "Trigger threshold (% of available ICU beds)"
  )
p_delay

pplotall <- plot_grid(p_rollback, p_delay, ncol = 1, labels = c("A", "B"))


f_save_plot(
  plot_name = paste0("ICU_prob_withUncertainty_loess_v2"), pplot = pplotall,
  plot_dir = file.path(sim_dir, "ICU_prob_plots"), width = 12, height = 8
)


