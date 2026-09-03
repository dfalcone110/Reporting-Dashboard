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

table_generator_nit <- function(x,y,z) {
  
  #variables------
  variable_assignment(x,y,z)
  
  
  #original code--------------
  # if(z %in% c("Nitrite", "Nitrate")){
  #   si <- hs_sites
  #   sc <-  "Routine Daily"
  # }else(
  #   if(z %in% c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)")){ 
  #     si <- drr_sites
  #     sc <- c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples")
  #   } else (
  #     if(z == "Violation Check Samples"){
  #       si <- drr_sites
  #       sc <- c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples")
  #       z <- c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)", "E. coli (Colilert)")
  #     }else(
  #       if(z == "THMs"){
  #         z <-  c("Total THMs", "Bromoform","Bromodichloromethane", "Dibromochloromethane", "Chloroform")
  #         si <- dbp_sites
  #       }else(
  #         if(z == "HAAs"){
  #           z <-  c("5 Haloacetic acids", "Dibromoacetic acid", "Dichloroacetic acid", "Bromoacetic acid", "Chloroacetic acid", "Trichloroacetic acid" )
  #           si <- dbp_sites
  #         }else(
  #           if( z == "Secondaries"){
  #             z <- c("Chloride", "Iron", "Sulfate", 
  #                    "Solids Dissolved Total", "Manganese",
  #                    "Silver", "Color True")
  #             si <- hs_sites
  #             sc <- "Routine Daily"
  #           }else(
  #             if(z %in% c("Chloride", "Iron", "Sulfate", 
  #                         "Solids Dissolved Total", "Manganese",
  #                         "Silver", "Color True")){
  #               si <- hs_sites
  #               sc <- "Routine Daily"
  #             }
  #           )
  #         )
  #       )
  #     ))
  # )
  
  #generating dataframe --------------
  df2 <- fn1(z,st, en, si, sc)
  
  df2 <- df2 %>% 
    filter(sample_class != "Violation Check Samples") %>% 
    mutate(
      `Location 2` = padep_id,
    ) %>% 
    rename(`Loc/EPID`="padep_id", `Sample #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
    ) %>% 
    select(site, parameter, PWSID, Transcode, Contam, AnalMeth, Result, LLD, CE, AnalDate, `Loc/EPID`, `Location 2`, SampDate,SampType, SampTime, LabID, blank1, blank2,  `Sender ID`, `Sample #`, blank3, blank4, ANALYZED_BY, VALIDATED_ON, TRESULT ) %>% 
    mutate(across(everything(), as.character))
  return(df2)
}
# 
# check1 <- table_generator_nit("January", 2026, "Nitrite/Nitrate")
# check2 <- table_generator_nit("December", 2023, "Nitrate")