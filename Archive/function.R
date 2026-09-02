#code for function



library(tidyverse)
library(wqr)
library(kableExtra)
library(rmarkdown)
library(knitr)
library(lubridate)
library(stringr)
library(blastula)


#lims_dsn
#lims_uid
#lims_pwd
#owqm_dsn
#owqm_uid
#owqm_pwd
#ops_dsn
#ops_uid
#ops_pwd
#sradb_dsn
#sradb_uid
#sradb_pwd
# 
# file.edit("~/.Renviron")


#Sites from Sample Siting Plan--------
#SSP gives a list of all sites compliant with RTCR, DRR, OCCT, and DBP monitoring. 
#High service sites are not compliant with those programs 
# 4001 - Baxter HS DEP - ID = EP 101
# 5004, Queen Lane HS - DEP ID = EP 103
# 6001, Belmont HS - DEP ID = EP 102
# 4005 Ladners Point Pump Station - DEP Field Order
# 6002 Belmont Gravity - No DEP ID
#4001, 5004, 6001 are all compliant with other programs. Not sure what 4005, 6002 are compliant with

# 7303, removed from routine sampling on 2/1/2022


#external files/variable-----------
df <- read_sradb_table("si_site_info")
# df2 <- read.csv("SiteMasterListSRA.csv")

contaminant_codes <- read.csv("P:\\users\\Dante Falcone\\R\\Projects\\Projects\\Reporting Dashboard\\contaminant_codes.csv") 

contaminant_codes <-  contaminant_codes%>% mutate(
  Contaminant = str_to_title(str_to_lower(contaminant_codes$Contaminant)),
  Analysis.Method = str_to_title(str_to_lower(contaminant_codes$Analysis.Method)),
  LIMS_method = ifelse(Analysis.Method == "Ion Chrom, Suppress", "EPA 300.0 rev 2.1", ifelse(
    Analysis.Method == "Colormtrc,Cd Redct,Auto (Nox)", "SM 4500 NO3 F", ifelse(
      Analysis.Method == "Hach Method 10260", "Hach Method 10260", ifelse(
        Analysis.Method == "Chromo/Fluorogen (Colilert/18)", "SM 9223 Colilert", ifelse(
          Analysis.Method == "Hach Method 10250", "Hach Method 10250", "")
      )))),
  Contaminant = ifelse(Contaminant == "Total Coliform Presence", "Coliforms Total (Colilert)", ifelse(
    Contaminant == "E. Coliform Presence", "E. coli (Colilert)" ,Contaminant))
)

drr_sites <- df$loc_id[df$rtcr_site == TRUE]
drr_sites <- c(drr_sites, paste0(drr_sites, c("U")), paste0(drr_sites, c("D")))

dbp_sites <- df$loc_id[df$dbp_site == TRUE]
occt_sites <- df$loc_id[df$occt_Site == TRUE]
nitrification_sites <- df$loc_id[df$nitrification_site == TRUE]
hs_sites <- c(4001, 5004, 6001)
# st_sites <- df$LOC_ID[df$STORAGE == "Y"]


# pump_sites <- df$LOC_ID[df$PUMPING_ST == "Y"]
st_eff_sites <- c(7101, 7207, 7204, 7301, 7302, 7401, 7502, 7601)






additional <- lims_column_names$column_names

SDWA4_col_csv <-c("PWSID","Transcode","Contam","AnalMeth","Result","LLD","CE","AnalDate","Loc/EPID","Location 2","SampDate","SampType","SampTime","LabID","blank","blank","Sender ID","Sample #")
SDWA1_Col_csv <- c("PWSID",	"Transcode",	"Contam",	"AnalMeth",	"Result",	"AnalDate",	"Loc/EPID",	"SampDate",	"SampType",	'SampTim',	'LabID',	"Sender ID",	"Samp #","blank",	"Loc/EPID2",	"blank")
SDWA1_params <- c("Coliforms Total (Colilert)","Field-Chlorine Residual Total", "E. Coli (Colilert)", "5 Haloacetic acids", "Total THMs", "Alkalinity")
SDWA4_params <- c("Arsenic", "Nitrate", "Nitrite", "Orthophosphate", "pH", "Cyanide Total", "Chloride", "Iron", "Manganese", "Silver", "Solids Dissolved Total") #still need to add SOCs and VOCs, Color, Sulfate


#Function--------------

table_generator <- function(x,y,z){
  
  #site definition
  if(z %in% c("Nitrite", "Nitrate")){
    s = hs_sites
    sc = "Routine Daily"} else {
      if(z %in% c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)")){
        s = drr_sites
        sc = c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples")
      } else {
        if(z == "Violation Check Samples"){
          sc <- c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples")
          z <- c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)", "E. coli (Colilert)")
          s <- drr_sites
        }
      }
    }
  
  month <- ymd(mdy(paste0(x, "01", y)))
  
  #generating lims numbers to filter out if there are coliform samples without matching FCl2
  if("Coliforms Total (Colilert)" %in% z & length(z) == 1) {
    df3 <- read_LIMS(
      parameter = c(z, "Field-Chlorine Residual Total"),
      site = s,
      sample_class = sc,
      start_date = month,
      end_date = month+months(1)
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
  
  
  
  # generating data frame
  
  df2 <- read_LIMS(
    start_date = month,
    end_date = month + months(1),
    parameter = z,
    site = s,
    sample_class = sc,
    select_additional = c("METHOD_USED", "ANALYZED_ON")
  )%>%  
    separate(date_time, into = c("SampDate", "SampTime"), sep = " ") %>%
    separate(ANALYZED_ON, into = c("AnalDate", "AnalTime"), sep = " ")%>%  
    mutate(SampDate = gsub('-','',(as.character(as.Date(SampDate, format = '%Y-%m-%d'), format = "%m-%d-%y"))),
           SampTime = str_remove(gsub(":", '', SampTime),"00$"),
           AnalDate = gsub('-','',(as.character(as.Date(AnalDate, format = '%Y-%m-%d'), format = "%m-%d-%y"))),
           PWSID = "1510001",
           Transcode = "03",
           LLD = " ",
           CE = " ",
           `Sender ID`= "SenderID",
           LabID = "51016",
           blank1 = " ",
           blank2 = " ",
           blank3 = " ",
           blank4 = " ",
           FilterID = " ",
           SampType = ifelse(
             site %in% drr_sites & sample_class %in% c("Routine Daily", "THMs/HAAs Monthly", "Violation Check Samples") & parameter == "Field-Chlorine Residual Total", "D", ifelse(
               site %in% c(4001, 5004, 6001), "E", ifelse(
                 site %in% drr_sites & sample_class == "Violation Check Samples" & parameter %in% c("Coliforms Total (Colilert)") , "C",ifelse(
                   site %in% drr_sites & sample_class %in% c("Routine Daily", "THMs/HAAs Monthly") & parameter %in% c("Coliforms Total (Colilert)"), "D", ifelse(
                     site %in% drr_sites & sample_class %in% c("Routine Daily", "THMs/HAAs Monthly") & parameter %in% c("E. coli (Colilert)"), "D", ifelse(
                       site %in% drr_sites & sample_class %in% c("Violation Check Samples") & parameter %in% c("E. coli (Colilert)"), "C", ""
                     )
                   )
                 )
               )
             )
           ),
           Result = ifelse(grepl("^<", result_as_entered), 0, result_as_entered),
           Site2 = ifelse(grepl("*U", site), str_remove(site, "U$"), ifelse(grepl("*D", site), str_remove(site, "D$"), site))) %>%
    filter(!is.na(result))
  
  df2 <- df2 %>% mutate(
    METHOD_USED = ifelse(grepl("*SM*", df2$METHOD_USED),  "SM 9223 Colilert", METHOD_USED) #iconv(METHOD_USED, "UTF-8", "ASCII", sub = ""),
  ) %>% 
    merge(df, by.x = "Site2", by.y = "loc_id") %>% 
    merge(contaminant_codes, by.x = c("parameter" ,"METHOD_USED"), by.y = c("Contaminant", "LIMS_method"), no.dups = T)
  
  
  
  #table
  if (z %in% SDWA4_params & length(z)==1) {
    
    df2 <- df2 %>% 
      filter(sample_class != "Violation Check Samples") %>% 
      mutate(
        `Location 2` = padep_id,
      ) %>% 
      rename(`Loc/EPID`="padep_id", `Sample #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
      ) %>% 
      select(PWSID, Transcode, Contam, AnalMeth, Result, LLD, CE, AnalDate, `Loc/EPID`, `Location 2`, SampDate,SampType, SampTime, LabID, blank1, blank2,  `Sender ID`, `Sample #`, blank3, blank4 ) %>% 
      mutate(across(everything(), as.character))
    return(df2)
  } else(
    if(z %in% SDWA1_params & is_empty(col_ln)==T & length(z)==1){
      df2 <- df2 %>% filter(is.na(project_no) | project_no == "THM_HAA Monthly",
                            sample_class != "Violation Check Samples") %>%
        mutate(
          `Loc/EPID2` = padep_id,
        ) %>% 
        rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
        ) %>%
        select(site, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2) %>%
        mutate(across(everything(), as.character))
      return(df2) 
    }else(
      if(z %in% SDWA1_params & is_empty(col_ln)==F & length(z) == 1){
        df2 <- df2 %>% filter(is.na(project_no) | project_no == "THM_HAA Monthly",
                              lims_number %in% ln_for_q,
                              sample_class != "Violation Check Samples") %>%
          mutate(
            `Loc/EPID2` = padep_id,
          ) %>% 
          rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
          ) %>%
          select(site, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2) %>%
          mutate(across(everything(), as.character))
        return(df2) 
      }else(
        if(length(z) == 3){
          df2 <- df2 %>%  filter(
            parameter == "E. coli (Colilert)" & lims_number %in% df2$lims_number[which(df2$parameter == "Coliforms Total (Colilert)" & df2$result != 0)] | sample_class == "Violation Check Samples"
          ) %>%
            mutate(
              `Loc/EPID2` = padep_id,
            ) %>% 
            rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
            ) %>%
            select(site, PWSID, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Samp #`, FilterID)
          return(df2) 
        }
      ) 
    )
  )
  
}




jan <- table_generator("January", "2023", "Violation Check Samples")
feb <- table_generator("February", "2023", "Coliforms Total (Colilert)")
mar <- table_generator("March", "2023", "Coliforms Total (Colilert)")
apr <- table_generator("April", "2023", "Coliforms Total (Colilert)")
may <- table_generator("May", "2023", "Coliforms Total (Colilert)")
jun <- table_generator("June", "2023", "Coliforms Total (Colilert)")
jul <- table_generator("July", "2023", "Coliforms Total (Colilert)")
aug <- table_generator("August", "2023", "Coliforms Total (Colilert)")
sep <- table_generator("September", "2023", "Coliforms Total (Colilert)")
oct <- table_generator("October", "2023", "Coliforms Total (Colilert)")
nov <- table_generator("November", "2023", "Coliforms Total (Colilert)")
dec <- table_generator("December", "2023", "Coliforms Total (Colilert)")
hey2<- table_generator("August", "2023", "Violation Check Samples") %>% group_by(Contam) %>% summarise(count =n())


#Creating check function to check the total number of samples by month----------
check <- function(x,y){
  if(x == "Field-Chlorine Residual Total"){
    n = "cl2"
  } else{ if (x == "Coliforms Total (Colilert)") {
    n = "col"
  }else{
    if(x == "Violation Check Samples") {
      n = "vcs"
    }
  }}
  
  month <- c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  monthnum <- as.integer(factor(month, levels = month.name))
  
  
  for(i in month) {
    assign(paste0(n, i),table_generator(i,y,x) %>% group_by(month = substr(SampDate, start = 1, stop =2)) %>% summarise(count = n()))
    
  } 
  yearcount <- rbind(get(paste0(n,"January")),get(paste0(n,"February")) , get(paste0(n,"March")), get(paste0(n,"April")), get(paste0(n,"May")), get(paste0(n,"June")), get(paste0(n,"July")), get(paste0(n,"August")), get(paste0(n,"September")), get(paste0(n,"October")), get(paste0(n,"November")), get(paste0(n,"December")))
  return(yearcount)
}

vcs_may <- table_generator("May", 2023, "Violation Check Samples")


may <- read_LIMS(
  start_date = "2023-05-01",
  end_date = "2023-06-01",
  sample_class = "Violation Check Samples"
)

#Checking for discrepensies in total counts------------------------
cl2_23 <- check("Field-Chlorine Residual Total", 2023) %>% rename(count.cl2 = "count")
col_23 <- check("Coliforms Total (Colilert)", 2023) %>%  rename(count.col = "count")
vcs_23 <- check("Violation Check Samples", 2023) %>% rename(count.vcs = "count") # UNDERSTAND WHERE THESE NUMBERS ARE COMING





combo <- merge(cl2_23, col_23, by="month")
combo <- merge(combo, vcs_23, by = "month", all.x = T) %>% mutate(count.vcs = ifelse(is.na(count.vcs), 0, count.vcs)) #vcs numbers are not representative of the number of vcs samples for month








