library(tidyverse)
library(wqr)
library(kableExtra)
library(rmarkdown)
library(knitr)
library(lubridate)
library(stringr)
library(blastula)


# source("variables.R")
# source("fn1.R")
# source("overarching.R")
# source("variable_assignment.R")

#Creating check function to check the total number of samples by month----------
check <- function(x,y){
  if(x == "Field-Chlorine Residual Total"){
    n = "cl2"
  } else{ if (x == "Coliforms Total (Colilert)") {
    n = "col"
  }else{
    if(x == "Violation Check Samples") {
      n = "vcs"
    }else{
      if(x == "HAAs"){
        n = "haas"
      }else{
        if(x == "THMs")
          n = "thms"
      }
    }
  }
  }
  
  month <- c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  monthnum <- as.integer(factor(month, levels = month.name))
  
  if(x %in% c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)")) { 
    for(i in month) {
      assign(paste0(n, i),table_generator(i,y,x) %>% group_by(month = substr(SampDate, start = 1, stop =2)) %>% summarise(count = n()))
      
    }}else{
      if(x == "Violation Check Samples"){
        for(i in month) {
          assign(paste0(n, i),table_generator(i,y,x) %>% filter(Contam %in% c("3100", "1000")) %>% pivot_wider(id_cols = c(SampDate, site, `Samp #`), names_from = Contam, values_from = Result)%>% group_by(month = substr(SampDate, start = 1, stop =2)) %>% summarise(count = n()))
        }}else{
          if (x %in% c("THMs", "HAAs")){
            for(i in month) {
              assign(paste0(n, i),table_generator(i,y,x)%>% group_by(month = substr(SampDate, start = 1, stop =2)) %>% summarise(count = n())) 
            }
          }
        }
    }
  
  yearcount <- rbind(get(paste0(n,"January")),get(paste0(n,"February")) , get(paste0(n,"March")), get(paste0(n,"April")), get(paste0(n,"May")), get(paste0(n,"June")), get(paste0(n,"July")), get(paste0(n,"August")), get(paste0(n,"September")), get(paste0(n,"October")), get(paste0(n,"November")), get(paste0(n,"December")))
  return(yearcount)
}

# check23_col <- check("Coliforms Total (Colilert)", 2023) %>% rename("count_col"= count)
# check23_cl2 <- check("Field-Chlorine Residual Total", 2023) %>% rename("count_cl2" = count)
# check23_vcs <- check("Violation Check Samples", 2023) %>% rename("count_vcs" = count)
# # check23_thms <- check("THMs", 2023)
# # check23_haas <- check("HAAs", 2023)
# 
# year <- merge(check23_cl2,check23_col, by = "month")
# year <- merge(year, check23_vcs, all.x = T) %>% mutate(count_vcs = ifelse(is.na(count_vcs), 0, count_vcs)) %>% mutate(total_cl2 = count_cl2 + count_vcs,
#                                                                                                                       total_col = count_col + count_vcs)

