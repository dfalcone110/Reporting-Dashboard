# library(tidyverse)
# library(wqr)
# library(kableExtra)
# library(rmarkdown)
# library(knitr)
# library(lubridate)
# library(stringr)
# library(blastula)


# source("variables.R")
# source("fn1.R")
# source("variable_assignment.R")

table_generator_vcs <- function(x,y,z){
  
  variable_assignment(x,y,z)
  
  #original variable assignment code is still in nit.R and cl2_col.R
  
  #generating Data Frame
  
  
  df2 <- fn1(z, st, en, si, sc)
  
  df2 <- df2 %>%  filter(
    parameter == "E. coli (Colilert)" & lims_number %in% df2$lims_number[which(df2$parameter == "Coliforms Total (Colilert)" & df2$result != 0)] | sample_class == "Violation Check Samples"
  ) %>%
    mutate(
      `Loc/EPID2` = padep_id,
    ) %>% 
    rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
    ) %>%
    select(site,parameter, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2, ANALYZED_BY, VALIDATED_ON) %>%
    mutate(across(everything(), as.character))
  
  return(df2)
  
}

# vcs_check <- table_generator_vcs("August", 2023, "Violation Check Samples")
# 
# aug <- read_LIMS(
#   start_date = "2023-08-01",
#   end_date = "2023-09-01",
#   parameter = "Coliforms Total (Colilert)",
#   site = 2502,
#   select_additional = c("METHOD_USED", "ANALYZED_BY")
# ) %>% mutate(
#   check = ifelse(str_detect(METHOD_USED, "9223"),  T, F)
# )
# 
# aug2 <- fn1("Coliforms Total (Colilert)", "2023-01-01", "2023-09-01", 2502, c("Routine Daily", "Violation Check Samples", "THM/HAAs Monthly"))
