library(tidyverse)
library(wqr)
library(kableExtra)
library(rmarkdown)
library(knitr)
library(lubridate)
library(stringr)
library(blastula)
# #
#
source("variables.R")
source("fn1.R")
source("variable_assignment.R")
source("overarching.R")
# 
# 
# FCl2vsCol <- function(x,y) {
#   
#   fcl2 <- table_generator(x, y, "Field-Chlorine Residual Total") %>% mutate(
#     contract_lab = ifelse(grepl("*ConLab*", ANALYZED_BY), TRUE, FALSE),
#     not_validated = ifelse(is.na(VALIDATED_ON), TRUE ,FALSE)) %>%  
#     # filter(not_validated == FALSE) %>%
#     pivot_wider(id_cols = `Samp #`,
#                 names_from = parameter,
#                 values_from = Result)
# 
#   col <- table_generator(x, y, "Coliforms Total (Colilert)")%>% mutate(
#     contract_lab = ifelse(grepl("*ConLab*", ANALYZED_BY), TRUE, FALSE),
#     not_validated = ifelse(is.na(VALIDATED_ON), TRUE ,FALSE)) %>% 
#     # filter(not_validated == FALSE) %>% 
#     pivot_wider(id_cols = `Samp #`,
#                 names_from = parameter,
#                 values_from = Result)
#   
#   total <- merge(fcl2, col, all = T)%>% pivot_longer(c(`Field-Chlorine Residual Total`, `Coliforms Total (Colilert)`), names_to = "parameter", values_to = "result", values_drop_na = FALSE) 
# 
#   col_lims <- total$`Samp #`[which(total$parameter == "Coliforms Total (Colilert)" & is.na(total$result))]
#   cl2_lims<- total$`Samp #`[which(total$parameter == "Field-Chlorine Residual Total" & is.na(total$result))]
#   
#   
#   total2 <- total %>%
#     group_by(parameter) %>% summarise(
#       FCl2 = sum(parameter == "Field-Chlorine Residual Total"),
#       Col = sum(parameter == "Coliforms Total (Colilert)"),
#       FCl2_NA = sum(parameter == "Field-Chlorine Residual Total" & is.na(result)),
#       Col_NA = sum(parameter == "Coliforms Total (Colilert)" & is.na(result))
#     ) %>% mutate(
#       total = ifelse(parameter == "Coliforms Total (Colilert)", `Col`-`Col_NA`, ifelse(
#         parameter == "Field-Chlorine Residual Total", `FCl2`- `FCl2_NA`, ""
#       ))
#     )
#   total2$lims[1] <- toString(col_lims)
#   total2$lims[2] <- toString(cl2_lims)
#   return(total2)
# 
# 
# }
# 
# check <- FCl2vsCol("April", 2024, "Coliforms Total (Colilert)")

fcl2vscol <- function(x,y,z) {
  
 variable_assignment(x,y,z)
  
  if("Coliforms Total (Colilert)" %in% z & st > "2020-11-30") {
    df3 <- read_LIMS(
      parameter = c(z, "Field-Chlorine Residual Total"),
      site = si,
      sample_class = sc,
      start_date = st,
      end_date = en
    ) %>% filter(!is.na(result)) %>% 
      pivot_wider(
        id_cols = lims_number,
        names_from = parameter,
        values_from = result
      )
    df4 <- df3 %>% filter(
      is.na(`Field-Chlorine Residual Total`) & !is.na(`Coliforms Total (Colilert)`)
    )
    
    ln <- toString(unique(df4$lims_number)) #lims numbers for coliform samples without matching cl2
    
  }else(if("Field-Chlorine Residual Total" %in% z & st > "2020-11-30"){
    df3 <- read_LIMS(
      parameter = c(z, "Coliforms Total (Colilert)"),
      site = si,
      sample_class = sc,
      start_date = st,
      end_date = en
    ) %>% filter(!is.na(result)) %>% 
      pivot_wider(
        id_cols = lims_number,
        names_from = parameter,
        values_from = result
      )
    df4 <- df3 %>% filter(
      !is.na(`Field-Chlorine Residual Total`) & is.na(`Coliforms Total (Colilert)`)
    )
    ln <- toString(unique(df4$lims_number)) #lims numbers for cl2 samples without matching col
  }else(ln <- c()))
  

  return(ln)
}


check2 <- fcl2vscol("August", 2023, "Field-Chlorine Residual Total")

