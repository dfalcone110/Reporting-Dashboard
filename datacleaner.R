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
# source("nit.R")
# source("cl2_col.R")
# source("vcs.R")
# source("dbps.R")
# source("secondaries.R")
# source("TOC.R")
# source("Metals.R")
# source("VOCs.R")
# source("pH_Ortho.R")

table_clean <- function(x,y,z) {
  df <- table_generator(x,y,z) %>% filter(grepl("*ConLab*", ANALYZED_BY)==F,
                                          !is.na(VALIDATED_ON),
                                        !isna(Result)) %>% 
    subset(select = -c(site, parameter, ANALYZED_BY, VALIDATED_ON, TRESULT))
  
  
}
#quick_check <- table_generator("Quarter 4", 2021, "TOC")

# check <- read_LIMS(
#   start_date = "2024-01-01",
#   end_date = "2024-03-07",
#   site = drr_sites,
#   parameter = c("Field-Chlorine Residual Total"),
#   sample_class = c("Routine Daily", "THMs/HAAs Monthly"),
#   select_additional = c("ANALYZED_BY", "METHOD_USED","ENTERED_BY", "VALIDATED_ON")
# ) %>% subset(select = -c(parameter)) %>% filter(!is.na(VALIDATED_ON))
# 

