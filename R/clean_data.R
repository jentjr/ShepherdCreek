library(XLConnect)
library(reshape2)
library(dplyr)
library(lubridate)
library(weatherData)
library(data.table)

fix_datetime <- function(df, date, time) {
  lubridate::hour(df[,paste(date)]) <- lubridate::hour(df[,paste(time)])
  lubridate::minute(df[,paste(date)]) <- lubridate::minute(df[,paste(time)])
  lubridate::second(df[,paste(date)]) <- lubridate::second(df[,paste(time)])
  df[,paste(date)] <- lubridate::force_tz(df[,paste(date)], tzone = "America/New_York")
  df <- df[-which(names(df) %in% paste(time))]
  return(df)
}


wb <- loadWorkbook("data/ShepCrk Water Quality Data - Main.xlsx")

data <- readWorksheet(wb, sheet = "sheet1", startRow = 4, endRow = 314,
                      startCol = 2, endCol = 36)

data <- fix_datetime(data, "Date", "Time")

colnames(data) <- c("Site", 
                    "Date", 
                    "Flow_Condition",
                    "E_coli",
                    "Temperature_C", 
                    "Turbidity",
                    "SSC",
                    "Alkalinity",
                    "DOC",
                    "TOC",
                    "Chloride",
                    "Bromide",
                    "Nitrate",
                    "Orthophosphate",
                    "Sulfate",
                    "Ammoniacal_nitrogen",
                    "DIN",
                    "TKN",
                    "TDP",
                    "TP",
                    "Aluminum_dissolved",
                    "Calcium",
                    "Copper_dissolved",
                    "Iron_dissolved",
                    "Magnesium",
                    "Manganese_dissolved",
                    "Potassium",
                    "Sodium",
                    "Zinc_dissolved",
                    "Aluminum_total",
                    "Copper_total",
                    "Iron_total",
                    "Manganese_total",
                    "Zinc_total")

data <- data[-which(is.na(data$Date)), ]

data[, c(4:34)] <- sapply(data[, c(4:34)], as.numeric)

subbasins <- read.csv("data/subbasins.csv", stringsAsFactors = FALSE)
subbasins[, c(2:20)] <- sapply(subbasins[, c(2:20)], as.numeric)

data <- inner_join(data, subbasins, by = c("Site" = "site_id"))

data <- arrange(data, Date)

# # KLUK <- getWeatherForDate("KLUK", "2005-04-19", "2007-01-11", 
# #                           opt_detailed = TRUE, opt_all_columns = TRUE)
# save(KLUK, file = "data/KLUK.RData")

data(KLUK)

# Use foverlap joins from data.table to left join wq data to weather data 
# between the hourly date ranges
# need to create start and end columns 
KLUK <- as.data.table(KLUK)
KLUK$start <- KLUK$Time
KLUK$end <- KLUK$Time + 3600

data <- as.data.table(data)
data$start <- data$Date 
data$end <- data[2:nrow(data), start]  
data[nrow(data), end:=start + 3600*24] 

setkey(KLUK, start, end)

data <- foverlaps(data, KLUK, type = "any", mult = "first", nomatch = 0L)

data <- data[,-c(1:2, 12, 15:18,72:73), with=FALSE]

data[, ][PrecipitationIn == "N/A", PrecipitationIn := NA]
data[, Wind_Direction := as.factor(Wind_Direction)]
data[, Wind_SpeedMPH := as.numeric(Wind_SpeedMPH)]
data[, Gust_SpeedMPH := as.numeric(Gust_SpeedMPH)]
data[, PrecipitationIn := as.numeric(PrecipitationIn)]
data[, Humidity := as.numeric(Humidity)]
data[, Conditions := as.factor(Conditions)]
data[, Site := as.factor(Site)]
data[, Flow_Condition :=as.factor(Flow_Condition)]
# The STV approximates the 90th percentile of the water quality distribution 
# and is intended to be a value that should not be exceeded by more than 10 
# percent of the samples taken. 
# http://water.epa.gov/scitech/swguidance/standards/criteria/health/recreation/upload/factsheet2012.pdf
data$limit <- ifelse(data$E_coli < 410, 0, 1)
data$limit <- factor(data$limit, levels=c("No"=0, "Yes"=1))

data <- droplevels(data[!is.na(data$E_coli),])

# con_base <- readWorksheet(wb1, sheet = "CON", startRow = 3, endRow = 54, 
#                           startCol = 2, endCol = 43)
# con_base$Flow.condition <- "Base"
# con_storm <- readWorksheet(wb1, sheet = "CON", startRow = 63, endRow = 185, 
#                            startCol = 2, endCol = 43)
# colnames(con_storm) <- colnames(con_base)
# con_storm$Flow.condition = "Storm"
# 
# con <- rbind(con_base, con_storm)
# con <- fix_datetime(con, "Sampling.Date", "Time")
# rm("con_base", "con_storm")
# 
# variables <- c("site", "date", "temperature (degrees C)", 
#                    "Spec_Cond (mS)",
#                    "DO_sat (% sat)",
#                    "DO (mg/L)",
#                    "pH (pH)",
#                    "ORP (mV)",
#                    "Turbidity (NTU)",
#                    "Chlor (ug/L)",
#                    "SSC (mg/L)",
#                    "Alkalinity (mg CaCO3/L)",
#                    "DOC_R5 (mg/L)",
#                    "DOC_Awberc (mg/L)",
#                    "TOC_R5 (mg/L)",
#                    "TOC_Awberc (mg/L)",
#                    "Fluoride (mg/L)",
#                    "Chloride (mg/L)",
#                    "Bromide (mg/L)",
#                    "Nitrate (mg/L)",
#                    "Orthophosphate (mg/L)",
#                    "Sulfate (mg/L)",
#                    "Ammoniacal_nitrogen (mg/L)",
#                    "DIN (mg/L)",
#                    "TKN (mg/L)",
#                    "TDP (mg/L)",
#                    "TP (mg/L)",
#                    "Aluminum_dissolved",
#                    "Calcium (mg/L)",
#                    "Copper_dissolved (ug/L)",
#                    "Iron_dissolved (mg/L)",
#                    "Magnesium (mg/L)",
#                    "Manganese_dissolved (mg/L)",
#                    "Potassium (mg/L)",
#                    "Sodium (mg/L)",
#                    "Zinc_dissolved (ug/L)",
#                    "Aluminum_total (mg/L)",
#                    "Copper_total (mg/L)",
#                    "Iron_total (mg/L)",
#                    "Manganese_total (mg/L)",
#                    "Zinc_total (ug/L)",
#                    "Flow_condition")
# 
# colnames(con) <- variables
# 
# dri_base <- readWorksheet(wb1, sheet = "DRI", startRow = 6, endRow = 57, 
#                           startCol = 2, endCol = 43)
# dri_base$Flow.Condition <- "Base"
# dri_storm <- readWorksheet(wb1, sheet = "DRI", startRow = 65, endRow = 186, 
#                           startCol = 2, endCol = 43)
# dri_storm$Flow.Condition = "Storm"
# 
# colnames(dri_storm) <- colnames(dri_base)
# dri <- rbind(dri_base, dri_storm)
# dri <- fix_datetime(dri, "Sampling.Date", "Time")
# rm("dri_base", "dri_storm")
# 
# colnames(dri) <- variables
# 
# ref7_base <- readWorksheet(wb1, sheet = "REF7", startRow = 6, endRow = 55, 
#                           startCol = 2, endCol = 43)
# ref7_base$Flow.Condition <- "Base"
# ref7_base$Time <- ymd_hms(ref7_base$Time)
# ref7_storm <- readWorksheet(wb1, sheet = "REF7", startRow = 63, endRow = 179, 
#                            startCol = 2, endCol = 43)
# ref7_storm$Flow.Condition = "Storm"
# 
# colnames(ref7_storm) <- colnames(ref7_base)
# ref7 <- rbind(ref7_base, ref7_storm)
# ref7 <- fix_datetime(ref7, "Sampling.Date", "Time")
# rm("ref7_base", "ref7_storm")
# 
# colnames(ref7) <- variables
# 
# urb_base <- readWorksheet(wb1, sheet = "URB", startRow = 6, endRow = 57, 
#                            startCol = 2, endCol = 43)
# urb_base$Flow.Condition <- "Base"
# urb_storm <- readWorksheet(wb1, sheet = "URB", startRow = 66, endRow = 192, 
#                             startCol = 2, endCol = 43)
# urb_storm$Flow.Condition = "Storm"
# 
# colnames(urb_storm) <- colnames(urb_base)
# urb <- rbind(urb_base, urb_storm)
# urb$Time <- ymd_hms(urb$Time)
# urb <- fix_datetime(urb, "Sampling.Date", "Time")
# rm("urb_base", "urb_storm")
# 
# colnames(urb) <- variables
# 
# roa_base <- readWorksheet(wb1, sheet = "ROA", startRow = 6, endRow = 57, 
#                            startCol = 2, endCol = 43)
# roa_base$Flow.Condition <- "Base"
# roa_storm <- readWorksheet(wb1, sheet = "ROA", startRow = 64, endRow = 181, 
#                             startCol = 2, endCol = 43)
# roa_storm$Flow.Condition = "Storm"
# 
# colnames(roa_storm) <- colnames(roa_base)
# roa_base$Time <- ymd_hms(roa_base$Time)
# roa_storm$Time <- ymd_hms(roa_storm$Time)
# roa <- rbind(roa_base, roa_storm)
# roa <- fix_datetime(roa, "Sampling.Date", "Time")
# rm("roa_base", "roa_storm")
# 
# colnames(roa) <- variables
# 
# pwr_base <- readWorksheet(wb1, sheet = "PWR", startRow = 6, endRow = 50, 
#                           startCol = 2, endCol = 43)
# pwr_base$Flow.Condition <- "Base"
# pwr_storm <- readWorksheet(wb1, sheet = "PWR", startRow = 57, endRow = 166, 
#                            startCol = 2, endCol = 43)
# pwr_storm$Flow.Condition = "Storm"
# 
# colnames(pwr_storm) <- colnames(pwr_base)
# pwr_base$Time <- ymd_hms(pwr_base$Time)
# pwr_storm$Time <- ymd_hms(pwr_storm$Time)
# pwr <- rbind(pwr_base, pwr_storm)
# pwr <- fix_datetime(pwr, "Sampling.Date", "Time")
# rm("pwr_base", "pwr_storm")
# 
# colnames(pwr) <- variables
# 
# shep_creek <- rbind(con, dri, pwr, ref7, roa, urb)
# shep_creek_melt <- melt(shep_creek, id.vars = c("site", "date"))
# 
# shep_creek_cast <- dcast(shep_creek, site + date ~ variable, value.var = "value")