library(XLConnect)
library(reshape2)
library(dplyr)
library(lubridate)

wb1 <- loadWorkbook("ShepCrk Water Quality Data - Main.xlsx")

main <- readWorksheet(wb1, sheet = "sheet1", startRow = 4, endRow = 314,
                      startCol = 2, endCol = 36)

main <- fix_datetime(main, "Date", "Time")

colnames(main) <- c("Site", 
                    "Date", 
                    "Flow_Condition",
                    "E_coli (CFU/100 mL)",
                    "Temperature (degrees C)", 
                    "Turbidity (NTU)",
                    "SSC (mg/L)",
                    "Alkalinity (mg CaCO3/L)",
                    "DOC (mg/L)",
                    "TOC (mg/L)",
                    "Chloride (mg/L)",
                    "Bromide (mg/L)",
                    "Nitrate (mg/L)",
                    "Orthophosphate (mg/L)",
                    "Sulfate (mg/L)",
                    "Ammoniacal_nitrogen (mg/L)",
                    "DIN (mg/L)",
                    "TKN (mg/L)",
                    "TDP (mg/L)",
                    "TP (mg/L)",
                    "Aluminum_dissolved",
                    "Calcium (mg/L)",
                    "Copper_dissolved (ug/L)",
                    "Iron_dissolved (mg/L)",
                    "Magnesium (mg/L)",
                    "Manganese_dissolved (mg/L)",
                    "Potassium (mg/L)",
                    "Sodium (mg/L)",
                    "Zinc_dissolved (ug/L)",
                    "Aluminum_total (mg/L)",
                    "Copper_total (mg/L)",
                    "Iron_total (mg/L)",
                    "Manganese_total (mg/L)",
                    "Zinc_total (ug/L)")

con_base <- readWorksheet(wb1, sheet = "CON", startRow = 3, endRow = 54, 
                          startCol = 2, endCol = 43)
con_base$Flow.condition <- "Base"
con_storm <- readWorksheet(wb1, sheet = "CON", startRow = 63, endRow = 185, 
                           startCol = 2, endCol = 43)
colnames(con_storm) <- colnames(con_base)
con_storm$Flow.condition = "Storm"

con <- rbind(con_base, con_storm)
con <- fix_datetime(con, "Sampling.Date", "Time")
rm("con_base", "con_storm")

variables <- c("site", "date", "temperature (degrees C)", 
                   "Spec_Cond (mS)",
                   "DO_sat (% sat)",
                   "DO (mg/L)",
                   "pH (pH)",
                   "ORP (mV)",
                   "Turbidity (NTU)",
                   "Chlor (ug/L)",
                   "SSC (mg/L)",
                   "Alkalinity (mg CaCO3/L)",
                   "DOC_R5 (mg/L)",
                   "DOC_Awberc (mg/L)",
                   "TOC_R5 (mg/L)",
                   "TOC_Awberc (mg/L)",
                   "Fluoride (mg/L)",
                   "Chloride (mg/L)",
                   "Bromide (mg/L)",
                   "Nitrate (mg/L)",
                   "Orthophosphate (mg/L)",
                   "Sulfate (mg/L)",
                   "Ammoniacal_nitrogen (mg/L)",
                   "DIN (mg/L)",
                   "TKN (mg/L)",
                   "TDP (mg/L)",
                   "TP (mg/L)",
                   "Aluminum_dissolved",
                   "Calcium (mg/L)",
                   "Copper_dissolved (ug/L)",
                   "Iron_dissolved (mg/L)",
                   "Magnesium (mg/L)",
                   "Manganese_dissolved (mg/L)",
                   "Potassium (mg/L)",
                   "Sodium (mg/L)",
                   "Zinc_dissolved (ug/L)",
                   "Aluminum_total (mg/L)",
                   "Copper_total (mg/L)",
                   "Iron_total (mg/L)",
                   "Manganese_total (mg/L)",
                   "Zinc_total (ug/L)",
                   "Flow_condition")

colnames(con) <- variables

dri_base <- readWorksheet(wb1, sheet = "DRI", startRow = 6, endRow = 57, 
                          startCol = 2, endCol = 43)
dri_base$Flow.Condition <- "Base"
dri_storm <- readWorksheet(wb1, sheet = "DRI", startRow = 65, endRow = 186, 
                          startCol = 2, endCol = 43)
dri_storm$Flow.Condition = "Storm"

colnames(dri_storm) <- colnames(dri_base)
dri <- rbind(dri_base, dri_storm)
dri <- fix_datetime(dri, "Sampling.Date", "Time")
rm("dri_base", "dri_storm")

colnames(dri) <- variables

ref7_base <- readWorksheet(wb1, sheet = "REF7", startRow = 6, endRow = 55, 
                          startCol = 2, endCol = 43)
ref7_base$Flow.Condition <- "Base"
ref7_base$Time <- ymd_hms(ref7_base$Time)
ref7_storm <- readWorksheet(wb1, sheet = "REF7", startRow = 63, endRow = 179, 
                           startCol = 2, endCol = 43)
ref7_storm$Flow.Condition = "Storm"

colnames(ref7_storm) <- colnames(ref7_base)
ref7 <- rbind(ref7_base, ref7_storm)
ref7 <- fix_datetime(ref7, "Sampling.Date", "Time")
rm("ref7_base", "ref7_storm")

colnames(ref7) <- variables

urb_base <- readWorksheet(wb1, sheet = "URB", startRow = 6, endRow = 57, 
                           startCol = 2, endCol = 43)
urb_base$Flow.Condition <- "Base"
urb_storm <- readWorksheet(wb1, sheet = "URB", startRow = 66, endRow = 192, 
                            startCol = 2, endCol = 43)
urb_storm$Flow.Condition = "Storm"

colnames(urb_storm) <- colnames(urb_base)
urb <- rbind(urb_base, urb_storm)
urb$Time <- ymd_hms(urb$Time)
urb <- fix_datetime(urb, "Sampling.Date", "Time")
rm("urb_base", "urb_storm")

colnames(urb) <- variables

roa_base <- readWorksheet(wb1, sheet = "ROA", startRow = 6, endRow = 57, 
                           startCol = 2, endCol = 43)
roa_base$Flow.Condition <- "Base"
roa_storm <- readWorksheet(wb1, sheet = "ROA", startRow = 64, endRow = 181, 
                            startCol = 2, endCol = 43)
roa_storm$Flow.Condition = "Storm"

colnames(roa_storm) <- colnames(roa_base)
roa_base$Time <- ymd_hms(roa_base$Time)
roa_storm$Time <- ymd_hms(roa_storm$Time)
roa <- rbind(roa_base, roa_storm)
roa <- fix_datetime(roa, "Sampling.Date", "Time")
rm("roa_base", "roa_storm")

colnames(roa) <- variables

pwr_base <- readWorksheet(wb1, sheet = "PWR", startRow = 6, endRow = 50, 
                          startCol = 2, endCol = 43)
pwr_base$Flow.Condition <- "Base"
pwr_storm <- readWorksheet(wb1, sheet = "PWR", startRow = 57, endRow = 166, 
                           startCol = 2, endCol = 43)
pwr_storm$Flow.Condition = "Storm"

colnames(pwr_storm) <- colnames(pwr_base)
pwr_base$Time <- ymd_hms(pwr_base$Time)
pwr_storm$Time <- ymd_hms(pwr_storm$Time)
pwr <- rbind(pwr_base, pwr_storm)
pwr <- fix_datetime(pwr, "Sampling.Date", "Time")
rm("pwr_base", "pwr_storm")

colnames(pwr) <- variables

shep_creek <- rbind(con, dri, pwr, ref7, roa, urb)
shep_creek_melt <- melt(shep_creek, id.vars = c("site", "date"))
shep_creek_melt$units <- 

shep_creek_cast <- dcast(shep_creek, site + date ~ variable, value.var = "value")

fix_datetime <- function(df, date, time) {
  lubridate::hour(df[,paste(date)]) <- lubridate::hour(df[,paste(time)])
  lubridate::minute(df[,paste(date)]) <- lubridate::minute(df[,paste(time)])
  lubridate::second(df[,paste(date)]) <- lubridate::second(df[,paste(time)])
  df[,paste(date)] <- lubridate::force_tz(df[,paste(date)], tzone = "America/New_York")
  df <- df[-which(names(df) %in% paste(time))]
  return(df)
}



wb2 <- loadWorkbook("Shep Cr Microbiol Data LB.xls")