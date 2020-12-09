
generatePlot=FALSE
if(generatePlot){
  
  
  ### save combined dat in simulation folder
  RtdatCOmbined$covidregion <- as.numeric(gsub("covidregion_","", RtdatCOmbined$geography_modeled))
  #RtdatCOmbined %>% write.csv(file.path(simulation_output, "NU_civis_outputs" ,simdate, 'csv',fname), row.names = FALSE)
  
  
  if(Rtplot){
    
    
    regions <- list(
      "Northcentral" = c(1, 2),
      "Northeast" = c(7, 8, 9, 10, 11),
      "Central" = c(3, 6),
      "Southern" = c(4, 5)
    )
    
    
    for (i in c(1:length(regions))) {
      reg <- names(regions)[[i]]
      covidregs <- regions[[i]]
      
      
      ptotal <- RtdatCOmbined %>% 
        filter(Date >= as.Date("2020-06-01") & Date <= as.Date("2020-11-01"))  %>%
        rename(region=covidregion) %>%
        f_addRestoreRegion() %>%
        filter(restore_region == reg) %>%
        mutate(restore_region = "total") %>%
        dplyr::group_by(restore_region, Date, scenario_name) %>%
        dplyr::summarize(
          Lower.error.bound.of.covid.19.Rt = mean(Lower.error.bound.of.covid.19.Rt),
          Upper.error.bound.of.covid.19.Rt = mean(Upper.error.bound.of.covid.19.Rt),
          Median.of.covid.19.Rt = mean(Median.of.covid.19.Rt)) %>%
        ggplot() +
        theme_cowplot()+
        geom_ribbon(aes(x=as.Date(Date), ymin=Lower.error.bound.of.covid.19.Rt, ymax =Upper.error.bound.of.covid.19.Rt, group=1 ),  fill="red2", size=1.3, alpha=0.5) +
        geom_line(aes(x=as.Date(Date), y =Median.of.covid.19.Rt, group=1 ), col="red2", size=1.3) +
        facet_wrap(~restore_region, nrow=1)+
        geom_hline(yintercept = 1, linetype="dashed") +
        geom_vline(xintercept = as.Date("2020-06-21"))+
        scale_y_continuous(limits=c(0.6, 1.5))+ 
        scale_x_date(date_breaks = "4 week", date_labels = "%d\n%b")  +
        labs(x="", y="", title=reg,subtitle="")   +
        geom_hline(yintercept = c(-Inf, Inf)) +
        geom_vline(xintercept = c(-Inf, Inf)) +
        customThemeNoFacet +
        theme(panel.spacing = unit(2, "lines"),
              plot.margin = unit(c(0,0,0,0), "cm"))
      
      pcovidreg <- RtdatCOmbined %>% 
        filter(Date >= as.Date("2020-06-01") & Date <= as.Date("2020-11-01"))  %>%
        f_addRestoreRegion() %>%
        filter(covidregion %in% covidregs) %>%
        ggplot() +
        theme_cowplot()+
        geom_ribbon(aes(x=as.Date(Date), ymin=Lower.error.bound.of.covid.19.Rt, ymax =Upper.error.bound.of.covid.19.Rt, group=1 ),  fill="red2", size=1.3, alpha=0.5) +
        geom_line(aes(x=as.Date(Date), y =Median.of.covid.19.Rt, group=1 ), col="red2", size=1.3) +
        facet_wrap(~covidregion, nrow=1)+
        geom_hline(yintercept = 1, linetype="dashed") +
        geom_vline(xintercept = as.Date("2020-06-21"))+
        scale_y_continuous(limits=c(0.6, 1.5))+ 
        scale_x_date(date_breaks = "4 week", date_labels = "%d\n%b")  +
        labs(x="", y="", title="",subtitle="")   +
        geom_hline(yintercept = c(-Inf, Inf)) +
        geom_vline(xintercept = c(-Inf, Inf)) +
        customThemeNoFacet +
        theme(panel.spacing = unit(2, "lines"),
              plot.margin = unit(c(0,0,0,0), "cm"))
      
      
      
      if (i == 1) r1 <- ptotal
      if (i == 2) r2 <- ptotal
      if (i == 3) r3 <- ptotal
      if (i == 4) r4 <- ptotal
      
      
      if (i == 1) p1 <- pcovidreg
      if (i == 2) p2 <- pcovidreg
      if (i == 3) p3 <- pcovidreg
      if (i == 4) p4 <- pcovidreg
    }
    
    
    # Tests per 1000 population\n(7 day average)
    c1 <- plot_grid(r1, p1, NULL, NULL, NULL, nrow = 1, rel_widths = c(1, 2, 1, 1, 1))
    c2 <- plot_grid(r2, p2, nrow = 1, rel_widths = c(1, 5))
    c3 <- plot_grid(r3, p3, NULL, NULL, NULL, nrow = 1, rel_widths = c(1, 2, 1, 1, 1))
    c4 <- plot_grid(r4, p4, NULL, NULL, NULL, nrow = 1, rel_widths = c(1, 2, 1, 1, 1))
    
    
    title <- ggdraw() + draw_label("Estimated Rt", fontface='bold')
    
    pplot <- plot_grid(c1, c2, c3, c4, ncol = 1)
    pplot <-  plot_grid(title, pplot, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
    
    
    ggsave(paste0("Regions_7dayAvr_testingRates_", LL_date, "LLpublic.png"),
           plot = pplot, path = plot_dir, width = 20, height = 14, device = "png"
    )
    
    # ggsave(paste0("Regions_7dayAvr_testingRates_", LL_date, "LLpublic.pdf"),
    #       plot = pplot, path = plot_dir, width = 20, height = 14, device = "pdf"
    #)
    
  }  
  
  
  
  
  
  f_overviewPlot <- function(restore_region){
    
    if(restore_region=="Central") ems =c(3,6)
    if(restore_region=="Northeast") ems =c(7,8,9,10,11)
    if(restore_region=="Northcentral") ems =c(1,2)
    if(restore_region=="Southern") ems =c(4,5)
    
    
    
    plotdat <- RtdatCOmbined %>% 
      filter(covidregion %in% ems, Date >= as.Date("2020-06-01") & Date <= as.Date("2020-12-01")) 
    
    
    p1 <- ggplot(data=plotdat) +
      geom_ribbon(aes(x=as.Date(Date), ymin=Lower.error.bound.of.covid.19.Rt, ymax =Upper.error.bound.of.covid.19.Rt, group=1 ),  fill="red2", size=1.3, alpha=0.5) +
      geom_line(aes(x=as.Date(Date), y =Median.of.covid.19.Rt, group=1 ), col="red2", size=1.3) +
      facet_wrap(~covidregion, nrow=1)+
      geom_hline(yintercept = 1, linetype="dashed") +
      geom_vline(xintercept = as.Date("2020-06-21"))+
      scale_y_continuous(limits=c(0.6, 1.3))+ 
      scale_x_date(date_breaks = "2 week", date_labels = "%d-%b")  +
      labs(x="", y="", title=restore_region,subtitle="Estimated Rt") 
    
    p2 <- ggplot(data=plotdat) +
      geom_ribbon(aes(x=as.Date(Date), ymin=Lower.error.bound.of.covid.19.new.infections, ymax =Upper.error.bound.of.covid.19.new.infections, group=1 ),  fill="deepskyblue2", size=1.3, alpha=0.5) +
      geom_line(aes(x=as.Date(Date), y =Number.of.Covid.19.new.infections, group=1 ), col="deepskyblue2", size=1.3) +
      facet_wrap(~covidregion, nrow=1)+
      geom_vline(xintercept = as.Date("2020-06-21"))+
      scale_x_date(date_breaks = "2 week", date_labels = "%d-%b")  +
      labs(x="", y="", subtitle="New infections") 
    
    
    p3 <- ggplot(data=plotdat) +
      geom_ribbon(aes(x=as.Date(Date), ymin=Lower.error.bound.of.number.of.hospital.beds.occupied, ymax =Upper.error.bound.of.number.of.hospital.beds.occupied, group=1 ),  fill="orchid4", size=1.3, alpha=0.5) +
      geom_line(aes(x=as.Date(Date), y =Number.of.hospital.beds.occupied, group=1 ), col="orchid4", size=1.3) +
      facet_wrap(~covidregion, nrow=1)+
      geom_vline(xintercept = as.Date("2020-06-21"))+
      scale_x_date(date_breaks = "2 week", date_labels = "%d-%b")  +
      labs(x="", y="", subtitle="Hospital beds occupied") 
    
    p4 <- ggplot(data=plotdat) +
      geom_ribbon(aes(x=as.Date(Date), ymin=Lower.error.bound.of.number.of.ICU.beds.occupied, ymax =Upper.error.bound.of.number.of.ICU.beds.occupied, group=1 ),  fill="chartreuse4", size=1.3, alpha=0.5) +
      geom_line(aes(x=as.Date(Date), y =Number.of.ICU.beds.occupied, group=1 ), col="chartreuse4", size=1.3) +
      facet_wrap(~covidregion, nrow=1)+
      geom_vline(xintercept = as.Date("2020-06-21"))+
      scale_x_date(date_breaks = "2 week", date_labels = "%d-%b") +
      labs(x="", y="", subtitle="ICU beds occupied") 
    
    
    p1 <- p1 +theme_minimal() +  geom_hline(yintercept = -Inf) + geom_vline(xintercept =- Inf)
    p2 <- p2 +theme_minimal() +  geom_hline(yintercept = -Inf) + geom_vline(xintercept = -Inf)
    p3 <- p3 +theme_minimal() +  geom_hline(yintercept = -Inf) + geom_vline(xintercept = -Inf)
    p4 <- p4 +theme_minimal() +  geom_hline(yintercept = -Inf) + geom_vline(xintercept = -Inf)
    
    
    pall <-  plot_grid(p1, p2,p3, p4, ncol=1, rel_heights = c(1.2,1,1,1))
    
    #ggsave(paste0(restore_region, "_latest_trend_predictions", ".png"),
    #       plot = pall, path = file.path(simulation_output, exp_name), width = 8, height = 5, device = "png"
    #)
    
    return(pall)
  }
  
  
  f_overviewPlot(restore_region ="Central")
  f_overviewPlot(restore_region ="Northeast")
  f_overviewPlot(restore_region ="Northcentral")
  f_overviewPlot(restore_region ="Southern")
  
  
  
  ### Calculate doubling time and weekly increase
  
  
}

