

nu_20200929 <- read_csv("C:/Users/mrm9534/Box/NU-malaria-team/projects/covid_chicago/NU_civis_outputs/20200929/csv/nu_20200929.csv")
nu_20200929 <- subset(nu_20200929, date <= as.Date("2020-10-01"))

nu_20200929 <-nu_20200929%>%
  mutate(new_hosp_bed_median = hosp_bed_median - lag(hosp_bed_median))

scl <- mean(dat$cases) / mean(Rt_dat$rt_median)
dat$date <- as.Date(dat$date)
Rt_dat$date <- as.Date(Rt_dat$date)


scl2 <- mean(nu_20200929$hosp_bed_median) / mean(Rt_dat$rt_median)


nu_20200929$hosp_bed_median_scl <- rescale(nu_20200929$hosp_bed_median, c(0,max(dat$cases)), from=range(nu_20200929$hosp_bed_median))
dat$cases_scl <- rescale(dat$cases, c(0,max(dat$cases)), from=range(dat$cases))

summary(nu_20200929$hosp_bed_median)
summary(dat$cases)
#dat$cases_scl = scale(dat$cases)
#nu_20200929$cases_new_median_scl = scale(nu_20200929$cases_new_median)
#scl <- mean(dat$cases_scl) / mean(Rt_dat$rt_median)

pplot <- ggplot(data = subset(Rt_dat, date >= "2020-02-01")) +
  theme_bw() +
  geom_bar(data=nu_20200929, aes(x = date, y = hosp_bed_median_scl / scl), fill = "orange", stat = "identity", alpha = 0.5) +
  geom_bar(data = subset(dat, date >= "2020-02-01"), aes(x = date, y = cases / scl), fill = "deepskyblue3", stat = "identity", alpha = 0.5) +
  geom_line(aes(x = date, y = rt_median), col = "deepskyblue4", size = 1) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper), fill = "deepskyblue4", alpha = 0.5) +
  geom_line(data=nu_20200929,aes(x = date, y = rt_median), col = "darkorange", size = 1) +
  geom_ribbon(data=nu_20200929,aes(x = date, ymin = rt_lower, ymax = rt_upper), fill = "orange", alpha = 0.5) +
  facet_wrap(~covid_region, scales = "free") +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
  scale_y_continuous(expression(italic(R[t])), sec.axis = sec_axis(~ . * scl, name = "Cases")) +
  labs(fill = "", color = "", x = "", y = expression(italic(R[t])), caption = "method = uncertain_si") +
  scale_x_date(breaks = "30 days", date_labels = "%d\n%b") +
  customThemeNoFacet


pplot <- ggplot(data = subset(Rt_dat, date >= "2020-02-01")) +
  theme_bw() +
  geom_line(aes(x = date, y = rt_median), col = "deepskyblue4", size = 1) +
  geom_ribbon(aes(x = date, ymin = rt_lower, ymax = rt_upper), fill = "deepskyblue4", alpha = 0.5) +
  geom_line(data=nu_20200929,aes(x = date, y = rt_median), col = "orange", size = 1) +
  geom_ribbon(data=nu_20200929,aes(x = date, ymin = rt_lower, ymax = rt_upper), fill = "orange", alpha = 0.5) +
  facet_wrap(~covid_region, scales = "free") +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
  scale_y_continuous(expression(italic(R[t]))) +
  labs(fill = "", color = "", x = "", y = expression(italic(R[t])), caption = "method = uncertain_si") +
  scale_x_date(breaks = "30 days", date_labels = "%d\n%b") +
  customThemeNoFacet


nu_20200929$covid_region <- gsub("covidregion_","",nu_20200929$geography_modeled)
nu_20200929$covid_region<- as.numeric(nu_20200929$covid_region)
datRt2 <- Rt_dat %>% left_join(nu_20200929, by=c("date","covid_region"))


ggplot(data=subset(datRt2,rt_median.x<=3 & rt_median.y<=3 & date>=as.Date("2020-09-01") & date<=as.Date("2020-10-01")))+
  geom_point(aes(x=rt_median.x, y=rt_median.y)) +
  geom_smooth(aes(x=rt_median.x, y=rt_median.y), method="lm") +
  facet_wrap(~ covid_region, scales="free")


scl <- mean(nu_20200929$hosp_bed_median) / mean(dat$cases)

pplot <- ggplot(data = subset(Rt_dat, date >= "2020-02-01")) +
  theme_bw() +
  geom_bar(data=nu_20200929, aes(x = date, y = new_hosp_bed_median ), fill = "orange", stat = "identity", alpha = 0.5) +
  geom_bar(data = subset(dat, date >= "2020-02-01"), aes(x = date, y = admissions ), fill = "deepskyblue3", stat = "identity", alpha = 0.5) +
   facet_wrap(~covid_region, scales = "free") +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
scale_y_log10()+
  scale_x_date(breaks = "30 days", date_labels = "%d\n%b") +
  customThemeNoFacet




ggsave(paste0(today, "_Rt_and_cases_from_data.pdf"),
       plot = pplot, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "pdf"
)
ggsave(paste0(today, "_Rt_and_cases_from_data.png"),
       plot = pplot, path = file.path(outdir), width = 14, height = 8, dpi = 300, device = "png"
)


