# library(tidyverse)
# library(wqr)
# library(kableExtra)
# library(rmarkdown)
# library(knitr)
# library(lubridate)
# library(stringr)
# library(blastula)
# 

# source("variables.R")
# source("fn1.R")
# source("variable_assignment.R")


table_generator_cl2_col <- function(x,y,z) {
  
  #variables------
  

  variable_assignment(x,y,z)

  #generating lims numbers to filter out if there are coliform samples without matching FCl2-------
  if("Coliforms Total (Colilert)" %in% z & st > "2020-11-30") {
    df3 <- read_LIMS(
      parameter = c(z, "Field-Chlorine Residual Total"),
      site = si,
      sample_class = sc,
      start_date = st,
      end_date = en+months(1)
    ) %>% filter(!is.na(result)) %>% 
      pivot_wider(
        id_cols = lims_number,
        names_from = parameter,
        values_from = result
      )
    df4 <- df3 %>% filter(
      is.na(`Field-Chlorine Residual Total`) & !is.na(`Coliforms Total (Colilert)`)
    )
    col_ln <- unique(df4$lims_number) #lims numbers for coliform samples without matching cl2
    ln_for_q <- setdiff(unique(df3$lims_number), unique(df4$lims_number)) #lims numbers to be used for query. All lims numbers minus those coliforms without matching cl2
  }else(col_ln <- c())

  
  #generating Data Frame
  df2 <- fn1(z,st,en,si,sc)

  
  if(z %in% SDWA1_params & is_empty(col_ln)==T) {
    df2 <- df2 %>% filter(is.na(project_no) | project_no == "THM_HAA Monthly",
                          sample_class != "Violation Check Samples") %>%
      mutate(
        `Loc/EPID2` = padep_id,
      ) %>% 
      rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
      ) %>%
      select(site, parameter, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2, ANALYZED_BY, VALIDATED_ON, TRESULT) %>%
      mutate(across(everything(), as.character))
    return(df2) 
  }else{
    if(z %in% SDWA1_params & is_empty(col_ln)==F){
      df2 <- df2 %>% filter(is.na(project_no) | project_no == "THM_HAA Monthly",
                            lims_number %in% ln_for_q,
                            sample_class != "Violation Check Samples") %>%
        mutate(
          `Loc/EPID2` = padep_id,
        ) %>% 
        rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
        ) %>%
        select(site, parameter, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2, ANALYZED_BY, VALIDATED_ON, TRESULT) %>%
        mutate(across(everything(), as.character))
      return(df2) 
    }
  }
}



#check1 <- table_generator_cl2_col("July", 2026, "Field-Chlorine Residual Total")
# check2 <- table_generator_cl2_col("January", 2020, "Coliforms Total (Colilert)")

