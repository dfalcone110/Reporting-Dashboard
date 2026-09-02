# source("variables.R")
# source("fn1.R")
# source("variable_assignment.R")

#Sampled quarterly, reported quarterly: ARSENIC (IOC) ,ANTIMONY (IOC),  BARIUM (IOC), BERYLLIUM (IOC), CADMIUM (IOC), CHROMIUM (IOC), NICKEL (IOC), SELENIUM (IOC), THALLIUM (IOC)
  # will all be sample type S until the quarter four, then it will be sample type E - Arsenic will always be sample type E
#Sampled annually, reported annually: Cyanide, Fluoride, and Mercury - double check
  # assuming these will be sample type E

#Sampled and reported in the first month of every quarter: jan, apr, jul, oct

#methods in contaminant codes sheet look accurate for all contams using EPA 200.8 rev 5.4, look into the second mercury method and all non epa 200.8 rev 5.4 methods


# check <- read_LIMS(
#   start_date = "2019-01-01",
#   end_date = " 2024-02-01",
#   parameter = "Fluoride",
#   sample_class = "Routine Daily",
#   site = hs_sites,
#   select_additional = c("METHOD_USED", "ANALYZED_BY")
# ) %>% group_by(year(date_time), month(date_time), METHOD_USED) %>% summarise(
#   count = n()
# )
#   
# check2 <- read_LIMS(
#   start_date = "2019-01-01",
#   end_date = " 2024-02-01",
#   parameter = metals,
#   sample_class = "Routine Daily",
#   site = hs_sites,
#   select_additional = c("METHOD_USED", "ANALYZED_BY")
# ) %>% filter(quarter(date_time)==4)


table_generator_metals <- function(x,y,z) {
  
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
  return(df2)
}

# metals_check <- table_generator_metals("October", 2023, "Fluoride")

mercury <- read_LIMS(
  start_date = "2023-10-01",
  end_date = "2024-01-01",
  parameter = "Mercury",
  site = c(4001, 5004, 6001),
  select_additional = c("ANALYZED_BY", "METHOD_USED")
)