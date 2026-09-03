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


secondaries <- c("Chloride", "Iron", "Sulfate", 
                 "Solids Dissolved Total", "Manganese",
                 "Silver", "Color Apparent")


table_generator_sec <- function(x,y,z) {
  
  
  #variables---------
  variable_assignment(x,y,z)
  

  
  #generating Data Frame
  df2 <- fn1(z,st, en, si, sc)
  
  
  df2 <- df2 %>%
    filter(sample_class != "Violation Check Samples") %>%
    mutate(
      `Location 2` = padep_id,
    ) %>%
    rename(`Loc/EPID`="padep_id", `Sample #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
    ) %>%
    select(site, parameter, PWSID, Transcode, Contam, AnalMeth, Result, LLD, CE, AnalDate, `Loc/EPID`, `Location 2`, SampDate,SampType, SampTime, LabID, blank1, blank2,  `Sender ID`, `Sample #`, blank3, blank4, ANALYZED_BY, VALIDATED_ON, TRESULT) %>%
    mutate(across(everything(), as.character))
  return(df2)
}
# 



#solution 1

# sec_check <- table_generator_sec("October", 2023, "Secondaries")%>% group_by(`Loc/EPID`) %>% summarise(`Number.of.Samples` = n(),
#                                                                                                        `Analysis.Methods` = paste(unique(AnalMeth), collapse = " ")) %>% rename("Loc.EPID" = `Loc/EPID`)
# 
# hey <- data.frame(`Loc/EPID` = c("Total"), `Number of Samples` = 2, `Analysis Methods` = " ")
# 
# summary <- rbind(sec_check, hey)
# 
# 
# summary$Number.of.Samples[length(summary$Number.of.Samples)] <- sum(summary$Number.of.Samples[1:length(summary$Number.of.Samples) - 1])
# 






# checking <- read_LIMS(
#   start_date = "2023-10-01",
#   end_date = "2024-01-01",
#   parameter = "Solids Dissolved Total",
#   sample_class = "Routine Daily",
#   select_additional = c("METHOD_USED", "ANALYZED_BY")
# ) 
