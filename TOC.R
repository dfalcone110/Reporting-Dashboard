# source("variables.R")
# source("fn1.R")
# source("variable_assignment.R")


#need to consider a way to filter out plant samples on the first of the next quarter.

#sometimes TOC that is not sampled is removed from LIMS and sometimes it is kept as NM


table_generator_toc <- function(x,y,z) {
  variable_assignment(x,y,z)

  en <- en + days(1)


  #generating dataframe
  df <- fn1(z,st, en, si, sc)%>%
    mutate(
      date = as.Date(SampDate2, "%Y-%m-%d"),
      match_date =  ifelse(site %in% plant_sites, format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d")),
      pairvalue = paste0(service_area, "_", match_date)
    ) %>% filter(!(date == en - days(1) & SampType == "R"))
  

  
  #Filtering out Non-Pairs-----------
  

  toc_pairs <- df  %>%  group_by(match_date, service_area, SampType) %>%
    summarise(avg = mean(result)) %>% arrange(desc(match_date)) %>%
    pivot_wider(
      id_cols = match_date,
      names_from = c(service_area,SampType),
      values_from = avg
    ) %>% filter(
      if_any(everything(), ~is.na(.)),
      match_date >= st | match_date <= en
    ) %>% pivot_longer(cols = !match_date, names_to= "plant_type", values_to = "avg" ) %>%
    separate(plant_type,
             into = c("plant", "type"),
             sep = "_") %>%
    mutate(
      original_date = ifelse(type == "P", format(as.Date(match_date, "%Y-%m-%d") + days(1),  "%Y-%m-%d"), format(as.Date(match_date), "%Y-%m-%d")),
      pairvalue = paste0(plant, "_", match_date)
    )

  non_pair <- toc_pairs$pairvalue[which(is.na(toc_pairs$avg))]
  correct_pairs <- setdiff(df$pairvalue, non_pair)
  
  df <- df %>% filter(pairvalue %in% correct_pairs)

  
  #-------

  df2 <- df %>%
    group_by(
      padep_id,match_date,SampType,Contam.Code
    ) %>% summarise(
      avg = mean(as.numeric(Result))
    ) %>% mutate(
      SampDate2 = ifelse(SampType == "P", format(as.Date(match_date, "%Y-%m-%d") + days(1),  "%Y-%m-%d"), format(as.Date(match_date), "%Y-%m-%d"))
    )

  df3 <- df %>% merge(df2, by = c("SampDate2", "padep_id", "SampType", "Contam.Code"), all.x = T) %>%
    filter(site %in% c(4503, 4903, 5501, 5903, 6501, 6903))%>%
    mutate(
      `Loc/EPID2` = padep_id,
      Result = ifelse(Contam.Code == "2920", avg, ifelse(Contam.Code == "1927", Result, ""))
    ) %>%
    rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
    ) %>%
    select(site, parameter, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2, ANALYZED_BY, VALIDATED_ON) %>%
    mutate(across(everything(), as.character))

  return(df3)
  
}

# toc_dec <- table_generator_toc("Quarter 2", 2023, "TOC & Alkalinity") 


#quarters starting on a Wednesday: Quarter 1 2020, Quarter 2 2020, Quarter 3 2020
#quarters starting on a tuesday: Quarter 4 2019
#quarters with a missing intake/effluent pair: Quarter 3 2021 (2019-09-07 & 2019-09-08 at Belmont)



##*** when querying for just Alkalinity, the function will not filter out Alkalinity without a pair.  Will do this for "TOC" and "TOC & Alkalinity"




#Trying to come up with a way to filter out samples without a pair. 

# df <-  table_generator_toc("September", 2021, "TOC")%>%
#   mutate(
#     date = as.Date(SampDate2, "%Y-%m-%d"),
#     match_date =  ifelse(site %in% plant_sites, format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d"))
#   )
# 
# df2 <- df %>%
#   filter(parameter == "TOC") %>%
#   group_by(
#     padep_id,match_date,SampType,Contam.Code
#   ) %>% summarise(
#     avg = mean(as.numeric(Result))
#   ) %>% mutate(
#     SampDate2 = ifelse(SampType == "P", format(as.Date(match_date, "%Y-%m-%d") + days(1),  "%Y-%m-%d"), format(as.Date(match_date), "%Y-%m-%d"))
#   )
# 
# df3 <- df %>% merge(df2, by = c("SampDate2", "padep_id", "SampType", "Contam.Code"), all.x = T) %>%
#   filter(site %in% c(4503, 4903, 5501, 5903, 6501, 6903))%>%
#   mutate(
#     `Loc/EPID2` = padep_id,
#     Result = ifelse(Contam.Code == "2920", avg, ifelse(Contam.Code == "1927", Result, ""))
#   ) %>%
#   rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
#   ) %>%
#   select(site, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2) %>%
#   mutate(across(everything(), as.character))


# st <- as.Date("2021-08-01", "%Y-%m-%d")
# toc_years <- read_LIMS(
#   start_date = st,
#   end_date = "2021-09-01",
#   parameter = "TOC",
#   site = c(plant_sites, river_sites),
#   sample_class = c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily")
# )%>%
#   mutate(
#     date = as.Date(date_time, "%Y-%m-%d"),
#     match_date =  ifelse(site %in% plant_sites, format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d")),
#     plant = ifelse(site %in% c(4903, 4503), "Baxter", ifelse(site %in% c(5501, 5502, 5903), "Queen Lane", ifelse(site %in% c(6501, 6502, 6903), "Belmont", ""))),
#     type = ifelse(site %in% c(5501, 5502, 4503, 6501, 6502), "P", ifelse(site %in% c(4903, 5903, 6903), "R", ""))
#   ) %>% group_by(match_date, plant, type) %>%
#   summarise(avg = mean(result)) %>% arrange(desc(match_date)) %>%
#   pivot_wider(
#     id_cols = match_date,
#     names_from = c(plant,type),
#     values_from = avg
#   ) %>% filter(
#     if_any(everything(), ~is.na(.)),
#     # month(match_date) == month(st)
#   )

# attempt <- toc_years %>% pivot_longer(cols = !match_date, names_to= "plant_type", values_to = "avg" ) %>% 
#   separate(plant_type,
#            into = c("plant", "type"),
#            sep = "_") %>% 
#   mutate(
#     original_date = ifelse(type == "P", format(as.Date(match_date, "%Y-%m-%d") + days(1),  "%Y-%m-%d"), format(as.Date(match_date), "%Y-%m-%d")),
#     pairvalue = paste0(plant, "_", match_date)
#   )
# 
# non_pair <- attempt$pairvalue[which(is.na(attempt$avg))]
# 
# 
# 
# toc_years2 <- read_LIMS(
#   start_date = "2019-01-01",
#   end_date = "2024-01-01",
#   parameter = "TOC", 
#   site = c(plant_sites, river_sites),
#   sample_class = c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily")
# )%>% 
#   mutate(
#     date = as.Date(date_time, "%Y-%m-%d"),
#     match_date =  ifelse(site %in% plant_sites, format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d")),
#     plant = ifelse(site %in% c(4903, 4503), "Baxter", ifelse(site %in% c(5501, 5502, 5903), "Queen Lane", ifelse(site %in% c(6501, 6502, 6903), "Belmont", ""))),
#     type = ifelse(site %in% c(5501, 5502, 4503, 6501, 6502), "P", ifelse(site %in% c(4903, 5903, 6903), "R", "")),
#     pairvalue = paste0(plant, "_", match_date)
#   )
# 
# correctpairs <- setdiff(toc_years2$pairvalue, non_pair)
# 
# toc_years2 <- toc_years2%>% filter(pairvalue %in% correctpairs,
#                                   date> "2021-09-01" & date< "2021-10-01")
# 
# 
# 
# check <- read_LIMS(
#   start_date = "2021-09-01",
#   end_date = "2021-10-01",
#   parameter = "TOC", 
#   site = c(plant_sites, river_sites),
#   sample_class = c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily")
# )

# #creating calendar table to analyze the quarters that started on a wednesday or tuesday--------
# 
# calendar <- data.frame(
#   days = seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by="days")
# ) %>% mutate(
#   name = wday(days, label = TRUE),
#   month = month(days),
#   quarter = quarter(days),
#   year = year(days)
# )
# 
# 
# quarters_wed <- calendar %>% filter(name == "Wed" & quarter != lag(quarter, n = 1)) #quarters where the plant sample wouldve been taken on the first of the next quarter
# quarters_tues<- calendar %>% filter(name == "Tue" & quarter != lag(quarter, n = 1)) #quarters starting on tuesday, river samples taken on first day of quarter. Want to make sure that these samples are not included in the df for the previous quarter
# months <- wed <- calendar %>% filter(name == "Wed" & month != lag(month, n = 1))
# 2023: february, march, november - months starting on a wednesday in 2023
# 2022: june starting on a wednesday
# 2021: december, september start on a weds
# 2020: january, april,  july start on a weds





# #COME BACK TOO ALL OF BELOOW -- uncomment all code below this----
# toc_check <- function(x,y){
#   st <-  ymd(mdy(paste0(x, "01", y)))
#   en <- st + months(1)+days(1)
#   
# toc <- read_LIMS(
#   start_date= st,
#   end_date = en,             
#   parameter = c("TOC", "Alkalinity"),
#   site = c(river_sites, plant_sites),
#   sample_class = c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily")
# ) %>%
#     mutate(
#       date = as.Date(date_time, "%Y-%m-%d"),
#       match_date =  ifelse(site %in% plant_sites, format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d")),
#       plant = ifelse(site %in% c(4903, 4503), "Baxter", ifelse(site %in% c(5501, 5502, 5903), "Queen Lane", ifelse(site %in% c(6501, 6502, 6903), "Belmont", ""))),
#       type = ifelse(site %in% c(5501, 5502, 4503, 6501, 6502), "P", ifelse(site %in% c(4903, 5903, 6903), "R", ""))
#     ) %>% filter(month(match_date)==month(st))
# 
# toc2 <- toc %>%filter(parameter == "TOC") %>%  pivot_wider(
#   id_cols = match_date,
#   names_from = c(site,plant),
#   values_from = result
# )
# 
# alk <- toc %>%filter(parameter == "Alkalinity") %>%  pivot_wider(
#   id_cols = match_date,
#   names_from = c(site,plant,parameter),
#   values_from = result
# )   
# 
# combo <- merge(toc2, alk, by = "match_date") %>% mutate(
#   QL_avg_eff = (`5501_Queen Lane`+ `5502_Queen Lane`)/2,
#   Bel_avg_eff = (`6501_Belmont`+ `6502_Belmont`)/2,
#   bax_rem_req = ifelse(
#     `4903_Baxter`>2.0 & `4903_Baxter`<=4.0 & `4903_Baxter_Alkalinity`<=60, 0.35,ifelse(
#       `4903_Baxter`>2.0 & `4903_Baxter`<=4.0 & `4903_Baxter_Alkalinity`>60 & `4903_Baxter_Alkalinity`<=120, 0.25, ifelse(
#         `4903_Baxter`>2.0 & `4903_Baxter`<=4.0 & `4903_Baxter_Alkalinity`>120, 0.15, ifelse(
#           `4903_Baxter`>4.0 & `4903_Baxter`<=8.0 & `4903_Baxter_Alkalinity`<=60, 0.45, ifelse(
#             `4903_Baxter`>4.0 & `4903_Baxter`<=8.0 & `4903_Baxter_Alkalinity`>60 & `4903_Baxter_Alkalinity`<=120 , 0.25, ifelse(
#               `4903_Baxter`>4.0 & `4903_Baxter`<=8.0 & `4903_Baxter_Alkalinity`>120 , 0.25, ifelse(
#                 `4903_Baxter`>8.0 & `4903_Baxter_Alkalinity`<=60, 0.45, ifelse(
#                   `4903_Baxter`>8.0 & `4903_Baxter_Alkalinity`>60 & `4903_Baxter_Alkalinity`<=120 , 0.25, ifelse(
#                     `4903_Baxter`>8.0 & `4903_Baxter_Alkalinity`>120 , 0.25, ""
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   ),
#   ql_rem_req = ifelse(
#     `5903_Queen Lane`>2.0 & `5903_Queen Lane`<=4.0 & `5903_Queen Lane_Alkalinity`<=60, 0.35,ifelse(
#       `5903_Queen Lane`>2.0 & `5903_Queen Lane`<=4.0 & `5903_Queen Lane_Alkalinity`>60 & `5903_Queen Lane_Alkalinity`<=120, 0.25, ifelse(
#         `5903_Queen Lane`>2.0 & `5903_Queen Lane`<=4.0 & `5903_Queen Lane_Alkalinity`>120, 0.15, ifelse(
#           `5903_Queen Lane`>4.0 & `5903_Queen Lane`<=8.0 & `5903_Queen Lane_Alkalinity`<=60, 0.45, ifelse(
#             `5903_Queen Lane`>4.0 & `5903_Queen Lane`<=8.0 & `5903_Queen Lane_Alkalinity`>60 & `5903_Queen Lane_Alkalinity`<=120 , 0.25, ifelse(
#               `5903_Queen Lane`>4.0 & `5903_Queen Lane`<=8.0 & `5903_Queen Lane_Alkalinity`>120 , 0.25, ifelse(
#                 `5903_Queen Lane`>8.0 & `5903_Queen Lane_Alkalinity`<=60, 0.45, ifelse(
#                   `5903_Queen Lane`>8.0 & `5903_Queen Lane_Alkalinity`>60 & `5903_Queen Lane_Alkalinity`<=120 , 0.25, ifelse(
#                     `5903_Queen Lane`>8.0 & `5903_Queen Lane_Alkalinity`>120 , 0.25, ""
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   ),
#   bel_rem_req = ifelse(
#     `6903_Belmont`>2.0 & `6903_Belmont`<=4.0 & `6903_Belmont_Alkalinity`<=60, 0.35,ifelse(
#       `6903_Belmont`>2.0 & `6903_Belmont`<=4.0 & `6903_Belmont_Alkalinity`>60 & `6903_Belmont_Alkalinity`<=120, 0.25, ifelse(
#         `6903_Belmont`>2.0 & `6903_Belmont`<=4.0 & `6903_Belmont_Alkalinity`>120, 0.15, ifelse(
#           `6903_Belmont`>4.0 & `6903_Belmont`<=8.0 & `6903_Belmont_Alkalinity`<=60, 0.45, ifelse(
#             `6903_Belmont`>4.0 & `6903_Belmont`<=8.0 & `6903_Belmont_Alkalinity`>60 & `6903_Belmont_Alkalinity`<=120 , 0.25, ifelse(
#               `6903_Belmont`>4.0 & `6903_Belmont`<=8.0 & `6903_Belmont_Alkalinity`>120 , 0.25, ifelse(
#                 `6903_Belmont`>8.0 & `6903_Belmont_Alkalinity`<=60, 0.45, ifelse(
#                   `6903_Belmont`>8.0 & `6903_Belmont_Alkalinity`>60 & `6903_Belmont_Alkalinity`<=120 , 0.25, ifelse(
#                     `6903_Belmont`>8.0 & `6903_Belmont_Alkalinity`>120 , 0.25, ""
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   ),
#   bax_rem_ach = ((`4903_Baxter` - `4503_Baxter`)/`4903_Baxter`)*100,
#   ql_rem_ach = ((`5903_Queen Lane` - `QL_avg_eff`)/`5903_Queen Lane`)*100,
#   bel_rem_ach = ((`6903_Belmont` - `Bel_avg_eff`)/`6903_Belmont`)*100,
#   
# ) %>%  select("match_date" ,"4903_Baxter", "4903_Baxter_Alkalinity", "4503_Baxter", "bax_rem_req","bax_rem_ach", "5903_Queen Lane", "5903_Queen Lane_Alkalinity", "5501_Queen Lane", "5502_Queen Lane", "QL_avg_eff","ql_rem_req", "ql_rem_ach", "6903_Belmont",  "6903_Belmont_Alkalinity", "6501_Belmont", "6502_Belmont", "Bel_avg_eff", "bel_rem_req", "bel_rem_ach") 
# 
# 
# return(combo)
# }
# 
# toc_year <- function(y){
#   months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#   
#   df <- data.frame(
#     match_date = character(),
#     `4903_Baxter` = character(),
#     `4903_Baxter_Alkalinity` = character(),
#     `4503_Baxter` = character(),
#     `bax_rem_req`  = character(),
#     `bax_rem_ach` = character(),
#     `5903_Queen Lane`  = character(),
#     `5903_Queen Lane_Alkalinity` = character(),
#     `5501_Queen Lane`  = character(),
#     `5502_Queen Lane` = character(),
#     `QL_avg_eff`  = character(),
#     `ql_rem_req` = character(),
#     `ql_rem_ach` = character(),
#     `6903_Belmont` = character(),
#     `6903_Belmont_Alkalinity`  = character(),
#     `6501_Belmont`  = character(),
#     `6502_Belmont` = character(),
#     `Bel_avg_eff`  = character(),
#     `bel_rem_req` = character(),
#     `bel_rem_ach` = character()
#     
#   )
#   
#   for (i in months) {
#     assign(paste0(i,"_toc"), toc_check(i, y))
#     
#     df <- df %>% rbind(get(paste0(i,"_toc")))
#   }
# 
#   return(df)
#   
# }
# 
# toc_formatter <- function(y){ 
# df <- toc_year(y) %>% group_by(month(match_date)) %>% summarise(
#   avg_bax_inf = round(mean(`4903_Baxter`), 1),
#   avg_bax_eff = round(mean(`4503_Baxter`),1),
#   avg_ql_inf = round(mean(`5903_Queen Lane`),1),
#   avg_ql_eff = round(mean(`QL_avg_eff`),1),
#   avg_bel_inf = round(mean(`6903_Belmont`),1),
#   avg_bel_eff = round(mean(`Bel_avg_eff`),1)
#   ) 
# 
# 
# }
# 
# 
# 
# toc_20 <- toc_formatter(2020)
# 
# toc_22 <- toc_formatter(2022)
# 
# # toc_23 <- toc_formatter(2023)
# 
# #trying to replicate how DWRS does their summary calcs
# dwrs_toc_all_data <- function(y) { 
#  
#   
#    
#   st <-  ymd(mdy(paste0("01", "01", y)))
#   en <- st + years(1)
#   
#   toc <- read_LIMS(
#     start_date= st,
#     end_date = en,             
#     parameter = c("TOC", "Alkalinity"),
#     site = c(river_sites, plant_sites),
#     sample_class = c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily")
#   ) %>%
#     mutate(
#       date = as.Date(date_time, "%Y-%m-%d"),
#       match_date =  ifelse(site %in% plant_sites, format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d")),
#       plant = ifelse(site %in% c(4903, 4503), "Baxter", ifelse(site %in% c(5501, 5502, 5903), "Queen Lane", ifelse(site %in% c(6501, 6502, 6903), "Belmont", ""))),
#       type = ifelse(site %in% c(5501, 5502, 4503, 6501, 6502), "P", ifelse(site %in% c(4903, 5903, 6903), "R", ""))
#     ) 
#   
#   eff <- toc %>% filter(type == "P", parameter == "TOC") %>% group_by(date, plant) %>% summarise(avg = mean(result)) %>% mutate(
#     match_date =  as.Date(format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"))
#     
#   )
#   river <- toc %>% filter(type == "R", parameter == "TOC") %>% group_by(date,plant) %>% summarise(avg = mean(result))
#   
#   combo <- merge(eff, river, by.x = c("match_date", "plant"), by.y = c("date", "plant"), no.dups = T) %>% rename(
#     intake_date = match_date,
#     eff_date = date,
#     eff_avg = avg.x,
#     intake = avg.y
#   )
#   return(combo)
# }
# dwrs20 <- dwrs_toc_all_data(2020)
# 
# 
# 
# dwrs_month <- function(y){
# 
# st <-  ymd(mdy(paste0("01", "01", y)))
# en <- st + years(1)
# 
# toc <- read_LIMS(
#   start_date= st,
#   end_date = en,             
#   parameter = c("TOC", "Alkalinity"),
#   site = c(river_sites, plant_sites),
#   sample_class = c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily")
# ) %>%
#   mutate(
#     date = as.Date(date_time, "%Y-%m-%d"),
#     match_date =  ifelse(site %in% plant_sites, format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d")),
#     plant = ifelse(site %in% c(4903, 4503), "Baxter", ifelse(site %in% c(5501, 5502, 5903), "Queen Lane", ifelse(site %in% c(6501, 6502, 6903), "Belmont", ""))),
#     type = ifelse(site %in% c(5501, 5502, 4503, 6501, 6502), "P", ifelse(site %in% c(4903, 5903, 6903), "R", ""))
#   ) 
# 
# eff <- toc %>% filter(type == "P", parameter == "TOC") %>% group_by(date, plant) %>% summarise(avg = mean(result)) %>% mutate(
#   match_date =  as.Date(format(as.Date(date, "%Y-%m-%d") - days(1),  "%Y-%m-%d"))
# ) %>% group_by(month(date), plant) %>% summarise(avg_eff = mean(avg))
# river <- toc %>% filter(type == "R", parameter == "TOC") %>% group_by(date,plant) %>% summarise(avg = mean(result))%>% group_by(month(date), plant) %>% summarise(avg_riv = mean(avg))
# 
# combo <- merge(eff, river, by = c("month(date)", "plant")) 
# 
# eff2 <- combo %>% pivot_wider(id_cols = `month(date)`,
#                               names_from = plant,
#                               values_from = avg_eff)%>% 
#   rename(
#     month = `month(date)`,
#     bax_eff = Baxter,
#     bel_eff = Belmont,
#     ql_eff = `Queen Lane`
#   )
# 
# riv2 <- combo %>% pivot_wider(id_cols = `month(date)`,
#                               names_from = plant,
#                               values_from = avg_riv)%>% 
#   rename(
#     month = `month(date)`,
#     bax_intake = Baxter,
#     bel_intake = Belmont,
#     ql_intake = `Queen Lane`
#   )
# 
# combo <- merge(riv2, eff2, by = "month") %>% select(month, bax_intake, bax_eff, ql_intake, ql_eff, bel_intake, bel_eff)
# combo[,-1] <- round(combo[-1],1)
# 
# return(combo)
# 
# }
# 
# dwrs20 <- dwrs_month(2020)
# 
# dwrs21 <- dwrs_month(2021)
# 
# dwrs22 <- dwrs_month(2022)
# 
# #dwrs23 <- dwrs_month(2023)
# 
# 
# 
# #2020--------------
# #lims-dwrs matches dwrs
# #all numbers are same except for July bel and ql effluents
# 
# #2021----------------
#   #difference in lims data vs dwrs data calculations for september 2021 at belmont intake averages
#     toc_sep_21 <- toc_check("September", 2021)
#       #this is because of missing pair at belmont - pair not reported to dwrs but intake value is included in average from lims data. 
#   #lims-dwrs query data for june is shown as NA because of missing data points in lims as NA for 06/08/2021
#     toc_jun_21 <- toc_check("June", 2021)
# 
# #2022---------
# 
# #all numbers are same except for june bel effluent
# toc_jun_22 <- toc_check("June", 2022)
# #might be including plant samples taken on first of month which should be included in previous month
#   #includes plant sample on first of month - (1.52+1.46+1.65+1.545+1.56)/5 = 1.547 - rounds down to 1.5 somehow
#   #does not include plant sample on first of month - (1.46+1.65+1.545+1.56)/4 = 1.553 - rounds up to 1.6
# 
# 
# #lims-dwrs df shows 2.9 for ql influent in april and dwrs shows 3.0 (rounding?)
# toc_apr_22 <- toc_check("April", 2022)
# #2.5+2.32+4.97+2.01 = 2.95 which should round up to 3.0 - dwrs seems to be correct. likely a rounding issue with code
# 
# #2023---------
# #
# 
# 
# 
# # tocaug23 <- toc_check("August", 2023)
# # tocmar23 <- toc_check("March", 2023)
# # tocfeb23 <- toc_check("February", 2023)
# # tocnov23 <- toc_check("November", 2023)
# # 
# # tocjun22 <- toc_check("June", 2022)
# 
# # write.csv(tocaug23, "tocaug23.csv")

  
  