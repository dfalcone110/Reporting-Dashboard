# source("variables.R")
# source("fn1.R")
# source("variable_assignment.R")

#do we need a separate analysis method code for field-pH?
#Field pH and Orthophosphate are reported quarterly for OCCT sampling. 
#Plant labs report their average daily pH, we are not responsible for those results
#We report weekly hs Orthophosphate results


#


table_generator_ortho <- function(x,y,z) {
  
  #variables------
  variable_assignment(x,y,z)
  
  
  
  #generating dataframe --------------
  df2 <- fn1(z,st, en, si, sc)
  
  df2 <- df2 %>% 
    filter(sample_class != "Violation Check Samples") %>% 
    mutate(
      `Location 2` = padep_id,
    ) %>% 
    rename(`Loc/EPID`="padep_id", `Sample #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
    ) %>% 
    select(site, parameter,PWSID, Transcode, Contam, AnalMeth, Result, LLD, CE, AnalDate, `Loc/EPID`, `Location 2`, SampDate,SampType, SampTime, LabID, blank1, blank2,  `Sender ID`, `Sample #`, blank3, blank4, ANALYZED_BY, VALIDATED_ON ) %>% 
    mutate(across(everything(), as.character))
  return(df2)
}

# check1 <- table_generator_ortho("October", 2023, "Orthophosphate & pH")
#
# # 
# check <- read_LIMS(
#   start_date = "2023-01-01",
#   end_date = "2024-01-01",
#   site = c(hs_sites,occt_sites),
#   parameter = c("Field-pH", "Orthophosphate", "pH"),
#   sample_class = c("Routine Daily", "THMs/HAAs Monthly"),
#   select_additional = c("ANALYZED_BY", "METHOD_USED","ENTERED_BY")
# ) %>% group_by(month(date_time), METHOD_USED) %>% summarise(count = n())
# 
# unique(check$sample_class)
# unique(check$METHOD_USED)

