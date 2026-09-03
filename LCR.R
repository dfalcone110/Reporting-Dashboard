# source("variables.R")
# source("fn1.R")
# source("variable_assignment.R")


table_generator_lcr <- function(x,y,z) {
  variable_assignment(x,y,z)
  
  df2 <- fn1(z, st, en, si, sc)
  df2 <- df2  %>%
    mutate(
      `Loc/EPID2` = padep_id,
    ) %>% 
    rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
    ) %>%
    select(site,parameter, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2, ANALYZED_BY, VALIDATED_ON) %>%
    mutate(across(everything(), as.character))
  return(df2)
} 

# variable_assignment("June", 2025, "Metals")
# check1 <- fn1(c("Lead","Copper"),"2025-06-01","2025-07-01",c(), "Lead/Copper")
# check <- table_generator_lcr("June", 2025, "LCR")

# check2 <- read_LIMS(
#   start_date = "2025-06-01",
#   end_date = "2025-07-01",
#   site = c(),
#   sample_class = "Lead/Copper"
# )
# 
# variable_assignment("June",2025, "LCR")
# 
# check3 <- fn1()