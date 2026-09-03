#===============================================================================
# This script is to generate a DWLR reporing file for LCR samples
# 
# Tyler Bradley 
# 2022-06-30
#===============================================================================


library(tidyverse)
library(wqr)
library(writexl)


start_date <- "2025-06-01"
end_date <- "2025-07-01"

parameters <- c("Lead", "Copper")

reg_data_raw <- read_LIMS(sample_class = "Lead/Copper", parameter = parameters, 
                          start_date = start_date, end_date = end_date,
                          validated = TRUE, date_type = "analyzed")

reg_data <- reg_data_raw %>% 
  mutate(
    PWSID = "1510001",
    ContamCode = ifelse(parameter == "Lead", "1030", "1022"),
    Method = "170",
    Result = ifelse(result_as_entered == "<0.001", "0", result_as_entered),
    Anal_Date = format(date_time_analyzed, "%m%d%y"),
    Location = str_replace(site, "^LC-", ""),
    Samp_Date = format(date_time_collected, "%m%d%y"),
    Samp_Type = "D",
    Samptime = format(date_time_collected, "%H%M"),
    Lab_ID = "51016",
    SampleID = lims_number,
    Filterid = ""
  ) %>% 
  select(
    PWSID, ContamCode, Method, Result, Anal_Date,
    Location, Samp_Date, Samp_Type, Samptime, 
    Lab_ID, SampleID, Filterid
  ) %>% 
  arrange(ContamCode, SampleID)


# write_csv(reg_data, file = "P:/BLS/SRA/DWREGS/LCR/LCR - 2022/Results/Reporting/LCR22 - June Results - DWELR Submission - SDWA1.csv")
#write_xlsx(reg_data, path = "P:/BLS/SRA/DWREGS/LCR/LCR - 2022/Results/Reporting/LCR22 - September Results - DWELR Submission - SDWA1.xlsx")

write.csv(reg_data, "LCR22 - June Results - DWELR Submission - SDWA1.csv")


lcr_reporting <- function(start_date, end_date, file = NULL, save = TRUE){
  
  parameters <- c("Lead", "Copper")
  
  reg_data_raw <- wqr::read_LIMS(sample_class = "Lead/Copper", parameter = parameters, 
                            start_date = start_date, end_date = end_date,
                            validated = TRUE, date_type = "analyzed")
  
  reg_data <- reg_data_raw %>% 
    dplyr::mutate(
      PWSID = "1510001",
      ContamCode = ifelse(parameter == "Lead", "1030", "1022"),
      Method = "170",
      Result = ifelse(result_as_entered == "<0.001", "0", result_as_entered),
      Anal_Date = format(date_time_analyzed, "%m%d%Y"),
      Location = stringr::str_replace(site, "^LC-", ""),
      Samp_Date = format(date_time_collected, "%m%d%Y"),
      Samp_Type = "D",
      Samptime = format(date_time_collected, "%H%M"),
      Lab_ID = "51016",
      SampleID = lims_number,
      Filterid = ""
    ) %>% 
    dplyr::select(
      PWSID, ContamCode, Method, Result, Anal_Date,
      Location, Samp_Date, Samp_Type, Samptime, 
      Lab_ID, SampleID, Filterid
    ) %>% 
    dplyr::arrange(ContamCode, SampleID)
  
  if (save){
    if (is.null(file)){
      file <- as.character(glue::glue("P:/BLS/SRA/DWREGS/LCR/LCR - 2022/Results/Reporting/LCR22 - {lubridate::month(start_date)} Results - DWELR Submission - SDWA1.csv"))
    }
    
    readr::write_csv(reg_data, file = file)
  } else {
    return(reg_data)
  }
  
  
  
}
