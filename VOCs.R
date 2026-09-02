# source("variables.R")
# source("fn1.R")
# source("variable_assignment.R")

#only report on SOCs listed in the contaminant codes sheet. We sample more SOCs than are required to report. 
#SOCs have been analyzed by contract lab dating back to 2019
#Does PWD have lab certifications to analyze any SOCs?
#VOCs are reported on second month of the quarter. 

#VOCs were reported in march 2023 for trinsio spill with SampleType = S

#Xylenes are reported as "Xylenes Total" but are shown in lims as two separate species. 

#Months when VOCs were analyzed by BLS: November 2022, August 2022, May 2022, February 2022, November 2021, etc. 


#function------------

table_generator_voc <- function(x,y,z) {
  
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
    select(site, parameter, PWSID, Transcode, Contam, AnalMeth, Result, LLD, CE, AnalDate, `Loc/EPID`, `Location 2`, SampDate,SampType, SampTime, LabID, blank1, blank2,  `Sender ID`, `Sample #`, blank3, blank4, ANALYZED_BY, VALIDATED_ON ) %>% 
    mutate(across(everything(), as.character))
  
  #Calculating Total Xylenes----
  xylene <- df2 %>% filter(parameter %in% c("m,p-Xylenes", "o-Xylene")) %>% group_by(`Sample #`, site, AnalMeth, AnalDate) %>% summarise(Total = sum())
  
  for (i in unique(df2$`Sample #`)){
    df3 <-  df2 %>% filter(`Sample #` == i) %>% mutate(Result = as.numeric(Result))
    df4 <- df3 %>% filter(parameter %in% c("o-Xylene")) %>%
      mutate(parameter = "Xylenes Total",
             Result = df3$Result[which(df3$parameter == "o-Xylene")] + df3$Result[which(df3$parameter == "m,p-Xylenes")])
    df2 <- rbind(df2, df4)
  }
  
  
df2 <- df2 %>% filter(!(parameter %in% c("o-Xylene", "m,p-Xylenes"))) %>% 
  mutate(across(everything(), as.character))
  
  return(df2)
}

check1 <- table_generator_voc("November", 2022, "VOCs")






#test------------
# soc_check <- read_LIMS(
#   start_date = "2023-01-01",
#   end_date = "2024-01-01",
#   parameter = soc_params,
#   site = c(4001, 5004, 6001),
#   select_additional = c("ANALYZED_BY", "METHOD_USED")
# )
# 
# soc_check2 <- read_LIMS(
#   start_date = "2019-01-01",
#   end_date = "2024-01-01",
#   sample_class = "Annual SOCs",
#   select_additional = c("ANALYZED_BY", "METHOD_USED")
# )
# 
# voc_check <- read_LIMS(
#   start_date = "2019-01-01",
#   end_date = "2024-01-01",
#   parameter = c(voc_params),
#   select_additional = c("ANALYZED_BY", "METHOD_USED"),
#   site = hs_sites,
#   sample_class = "Routine Daily"
# ) %>%
#   filter(!is.na(result),
#          sample_type == "Grab",
#          # grepl("*ConLab*", ANALYZED_BY)==F
#   ) %>% mutate(
#     result2 = ifelse(grepl("^<", result_as_entered),0, result_as_entered)
#   ) %>% filter(result2 != 0)

# setdiff(unique(voc_check$parameter), unique(check1$parameter))

# 
# 
# 
# length(voc_params)
# length(unique(voc_check$parameter))
# 
# cont_voc <- contaminant_codes %>% filter(grepl("*(Voc)", Contaminant))
# 
# cont_voc <- cont_voc %>% mutate(Contaminant = gsub(" \\(Voc\\)", "", cont_voc$Contaminant))
# 
# cont_voc <- str_to_lower(cont_voc$Contaminant)
# 
# voc_params <- str_to_lower(voc_params)
# 
# sampled_not_contam <- setdiff(voc_params, cont_voc)
# contaminaint_not_sampled<- setdiff(cont_voc, voc_params)
# 
# contained_in_both <- intersect(voc_params, cont_voc)

 
