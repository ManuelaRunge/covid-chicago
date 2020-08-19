#source("load_paths.R")


f_valuefct = function(df){
  
  df$value_fct <- NA
  df$value_fct[df$value >= 400] <- ">400"
  df$value_fct[df$value < 400] <- "<400"
  df$value_fct[df$value < 300] <- "<300"
  df$value_fct[df$value < 200] <- "<200"
  df$value_fct[df$value < 100] <- "<100"
  df$value_fct[df$value < 50] <- "<50"
  
  df$value_fct <- factor(df$value_fct,
                         levels = c(">400", "<400", "<300", "<200", "<100", "<50"),
                         labels = c(">400", "<400", "<300", "<200", "<100", "<50")
  )
  
  return(df)
  
}


f_valuefct2 = function(df){
  
  q75 = quantile(df$value, probs=0.75, na.rm = TRUE)
  q50 =  median(df$value, na.rm=TRUE)
  q25 = quantile(df$value, probs=0.25, na.rm = TRUE)
  
  df$value_fct <- NA
  df$value_fct[df$value >= q75] <- "> quant75"
  df$value_fct[df$value < q75] <- "< quant75"
  df$value_fct[df$value < q50] <- "< quant50"
  df$value_fct[df$value < q25] <- "< quant25"
  
  df$value_fct <- factor(df$value_fct,
                         levels = c("< quant25", "< quant50", "< quant75", "> quant75"),
                         labels = c(paste0("> ", q25), 
                                    paste0("< ", q50), 
                                    paste0("< ", q75), 
                                    paste0(">= ", q75))
  )
  
  return(df)
  
}


f_valuefct_cap = function(df, valueVar="value", fewerClasses=FALSE){
  
  df= as.data.frame(df)
  colnames(df)[colnames(df)==paste0(valueVar, "_fct")] <- paste0(valueVar, "_fct_old")
  capacity = unique(df$capacity)
  
  qLT2.5 <- capacity - (capacity * 0.025)
  qLT5 <- capacity- (capacity* 0.05)
  qLT10 <-capacity - (capacity* 0.10)
  qLT15 <-capacity - (capacity* 0.15)
  qLT20 <-capacity - (capacity* 0.20)
  qLT30 <-capacity - (capacity* 0.30)
  qLT40 <-capacity - (capacity* 0.40)
  qLT50 <-capacity - (capacity* 0.50)
  qLT60 <-capacity - (capacity* 0.60)
  
  qGT2.5 <- capacity + (capacity * 0.025)
  qGT5 <- capacity+ (capacity* 0.05)
  qGT10 <-capacity + (capacity* 0.10)
  qGT15 <-capacity + (capacity* 0.15)
  qGT20 <-capacity + (capacity* 0.20)
  qGT30 <-capacity + (capacity* 0.30)
  qGT40 <-capacity + (capacity* 0.40)
  qGT50 <-capacity + (capacity* 0.50)
  qGT60 <-capacity + (capacity* 0.60)
  
  if(fewerClasses){
    
    cuts <- c(-Inf,qLT15,qLT5 ,qLT2.5,  capacity, qGT2.5, qGT5, qGT15, Inf)
    df$tempVar <- cut(df[,colnames(df)==valueVar] , breaks = cuts,   labels = c("- >15%","-15%","-5%","-2.5%","+2.5%","+5%", "+15%", "+ >15%"))

    }
  
if(fewerClasses==FALSE){
  cuts <- c(-Inf,
            qLT50,
            qLT40,
            qLT30,
            qLT20,
            qLT10,
            qLT5, 
            capacity,
            qGT5,
            qGT10,
            qGT20,
            qGT30,
            qGT40,
            qGT50, 
            Inf)

  df$tempVar <- cut(df[,colnames(df)==valueVar] , breaks = cuts, 
                    labels = c("- >50%",
                               "-50%",
                               "-40%",
                               "-30%",
                               "-20%",
                               "-10%",
                               "-5%",
                               "+5%",
                               "+10%", 
                               "+20%", 
                               "+30%", 
                               "+40%",
                               "+50%", 
                               "+ >50%"
                               ))
  
}
  
  colnames(df)[colnames(df)=="tempVar"] <- paste0(valueVar, "_fct")
  #tapply(df$value, df$value_fct, summary)

  return(df)
  
}



regions <- list(
  "Northcentral" = c(1, 2),
  "Northeast" = c(7, 8, 9, 10, 11),
  "Central" = c(3, 6),
  "Southern" = c(4, 5),
  "Illinois" = c(1:11)
)


f_addRestoreRegion <- function(dat){
  
  dat$restore_region <- NA
  dat$restore_region[dat$region %in%  regions$Northcentral ] <- "Northcentral"
  dat$restore_region[dat$region %in%  regions$Northeast ] <- "Northeast"
  dat$restore_region[dat$region %in%  regions$Central ] <- "Central"
  dat$restore_region[dat$region %in%  regions$Southern ] <- "Southern"
  
  
  return(dat)
  
}



combineDat <- function(filelist, namelist){
  
  count=0
  for (i in c(1:length(filelist))) {
    count=count+1
    dat <- read_csv(file.path(filelist[i]))
    dat$exp_name = namelist[i]
    if(count==1)combinedDat <- dat
    if(count>=1)combinedDat <- rbind(combinedDat, dat)
  }
  return(combinedDat)
}


load_population <- function(){
  geography_name <- c('illinois',c(1:11))
  pop <- c("12830632", "688393","1269519","581432",'676017','424810','760362','791009','1432193','1012222','2477754','2716921')
  df <-as.data.frame( cbind(geography_name, pop))
  return(df)
}


load_capacity <- function(selected_ems=NULL) {
  df <- read.csv(file.path(data_path, "covid_IDPH/Corona virus reports/capacity_by_covid_region.csv"))  %>%
    filter(date == max(date) & geography_name !="chicago") %>%
    mutate(
      geography_name = gsub("restore_","",geography_name),
      hospitalized = medsurg_total,
      critical = icu_total,
      ventilators = vent_total
    ) %>%
    dplyr::select(geography_name,hospitalized, critical, ventilators)
  
  if(!(is.null(selected_ems))) df <-  df %>% filter(geography_name %in% selected_ems)
  
  return(df)
}


customThemeNoFacet <- theme(
  strip.text.x = element_text(size = 12, face = "bold"),
  strip.text.y = element_text(size = 12, face = "bold"),
  strip.background = element_blank(),
  plot.title = element_text(size = 16, vjust = -1, hjust = 0),
  plot.subtitle = element_text(size = 12),
  plot.caption = element_text(size = 8),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text.y = element_text(size = 12)
)

## Extended and edited from: 
#https://stackoverflow.com/questions/35953394/calculating-length-of-95-ci-using-dplyr

# library(dplyr)

## Custom functions
## To Do: add multiple variables at once ? 

f_aggrDat <- function(dataframe, groupVars, valueVar, WideToLong=FALSE){
  # dataframe = dataframe to aggregate (new datafram will be created)
  # groupVars = variables to aggregate at 
  # valueVar = variable to aggregate 
  # WideToLong = transfrom data to long format, 
  #              so that statistics are in one column instead of spread over rows
  dataframe <- as.data.frame(dataframe)
  dataframe$tempvar <- dataframe[,colnames(dataframe)==valueVar]
  datAggr <- dataframe %>% 
    dplyr::group_by_(.dots=groupVars) %>%
    dplyr::summarise(
      min.val 	= min(tempvar, na.rm = TRUE),
      max.val	= max(tempvar, na.rm = TRUE),
      mean.val 	= mean(tempvar, na.rm = TRUE),
      median.val	= median(tempvar, na.rm = TRUE),
      sd.val		= sd(tempvar, na.rm = TRUE),
      n.val 		= n(),										 
      q25		= quantile(tempvar, probs=0.25, na.rm = TRUE),
      q75		= quantile(tempvar, probs=0.75, na.rm = TRUE),
      q2.5		= quantile(tempvar, probs=0.025, na.rm = TRUE),
      q97.5  	= quantile(tempvar, probs=0.975, na.rm = TRUE)) %>%
    dplyr::mutate(	
      se.val 			= sd.val / sqrt(n.val),
      lower.ci.val 	= mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
      upper.ci.val 	= mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val,
      weighted=0) %>%
    #dplyr::select(-sd.val, -n.val,-se.val) %>%
    as.data.frame()
  
  if(WideToLong){
    datAggr <- gather(datAggr, -groupVars)
    colnames(datAggr)[colnames(datAggr)=="variable"] <- "statistic" 
    colnames(datAggr)[colnames(datAggr)=="value"] <- valueVar
    datAggr$statistic <- gsub(".val","",datAggr$statistic)
  }
  
  return(datAggr)
}

## keep both names as long as not all scripts are updated
aggrDat <- f_aggrDat

f_weighted.aggrDat <- function(dataframe, groupVars, valueVar, weightVar, WideToLong=FALSE){
  # dataframe = dataframe to aggregate (new datafram will be created)
  # groupVars = variables to aggregate at 
  # valueVar = variable to aggregate 
  # WideToLong = transfrom data to long format, 
  #              so that statistics are in one column instead of spread over rows
  
  dataframe <- as.data.frame(dataframe)
  dataframe$tempvar <- dataframe[,colnames(dataframe)==valueVar]
  dataframe$w <- dataframe[,colnames(dataframe)==weightVar]
  
  datAggr <- dataframe %>% 
    dplyr::group_by_(.dots=groupVars) %>%
    dplyr::summarise(
      min.val 	= min(tempvar, na.rm = TRUE),
      max.val	= max(tempvar, na.rm = TRUE),
      mean.val 	= weighted.mean(tempvar, w),
      median.val	= weighted.median(tempvar, w),
      sd.val		= sqrt(sum(w * (tempvar - mean.val)^2)),
      n.val 		= n(),										 
      q25		= weighted.quantile(tempvar,w, probs=0.25),
      q75		= weighted.quantile(tempvar,w, probs=0.75),
      q2.5		= weighted.quantile(tempvar,w, probs=0.025),
      q97.5  	= weighted.quantile(tempvar,w, probs=0.975)) %>%
    dplyr::mutate(	
      se.val 			= sd.val / sqrt(n.val),
      lower.ci.val 	= mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
      upper.ci.val 	= mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val,
      weighted=1) %>%
    #dplyr::select(-sd.val, -n.val,-se.val) %>%
    as.data.frame()
  
  if(WideToLong){
    datAggr <- gather(datAggr, -groupVars)
    colnames(datAggr)[colnames(datAggr)=="variable"] <- "statistic" 
    colnames(datAggr)[colnames(datAggr)=="value"] <- valueVar
    datAggr$statistic <- gsub(".val","",datAggr$statistic)
  }
  
  return(datAggr)
}