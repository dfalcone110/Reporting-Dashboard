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
          Analysis.Method == "Hach Method 10250", "Hach Method 10250", ifelse(
            Analysis.Method == "Gc, Ecd, Llw (Sm 6251b)", "SM 6251 B", ifelse(
              Analysis.Method == "Gc,Ecd,Lle,Deriv (Epa 552.2/3)", "EPA 552.2", ifelse(
                Analysis.Method == "Gc, Ms, P&T (Epa 524.2)", "EPA 524.2 rev 4.1", ifelse(
                  Analysis.Method == "Visual, Pt-Co (Color)", "SM 2120 B-01", ifelse(
                    Analysis.Method == "Induct Couple Plasma", "EPA 200.7 rev 4.4", ifelse(
                      Analysis.Method == "Icp, Mass Spec", "EPA 200.8 rev 5.4", ifelse(
                        Analysis.Method == "Grav, Filt, Dry At 180 (Tds)", "SM 2540 C",""
                      )
                    )
                  )
                )
              )
            )
          ))
      )))),
  Contaminant = ifelse(Contaminant == "Total Coliform Presence", "Coliforms Total (Colilert)", ifelse(
    Contaminant == "E. Coliform Presence", "E. coli (Colilert)" , ifelse(
      Contaminant == "Bromoform (Thm)", "Bromoform", ifelse(
        Contaminant == "Bromodichloromethane (Thm)", "Bromodichloromethane", ifelse(
          Contaminant == "Chlorodibromomethane (Thm)", "Dibromochloromethane", ifelse(
            Contaminant == "Chloroform (Thm)", "Chloroform", ifelse(
              Contaminant == "Trihalomethanes (Tthm)", "Total THMs", ifelse(
                Contaminant == "Haloacetic Acids (Haa5)", "5 Haloacetic acids", ifelse(
                  Contaminant == "Dibromoacetic Acid (Haa)", "Dibromoacetic acid", ifelse(
                    Contaminant == "Dichloroacetic Acid (Haa)", "Dichloroacetic acid", ifelse(
                      Contaminant == "Monobromoacetic Acid (Haa)", "Bromoacetic acid", ifelse(
                        Contaminant == "Monochloroacetic Acid (Haa)", "Chloroacetic acid", ifelse(
                          Contaminant == "Trichloroacetic Acid (Haa)", "Trichloroacetic acid", ifelse(
                            Contaminant == "Color", "Color Apparent", ifelse(
                              Contaminant == "Tds (Filterable)", "Solids Dissolved Total", Contaminant
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )))
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


#Function Nitrite/Nitrate---------

table_generator_nit <- function(x,y,z) {
  s = hs_sites
  sc = "Routine Daily"
  month <- ymd(mdy(paste0(x, "01", y)))
  
  #generating dataframe 
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
    METHOD_USED = ifelse(grepl("*9223*", df2$METHOD_USED),  "SM 9223 Colilert", ifelse(
      grepl("*524.2*", df2$METHOD_USED), "EPA 524.2 rev 4.1", ifelse(
        grepl("*6251*", df2$METHOD_USED), "SM 6251 B", ifelse(
          grepl("*552.2*", df2$METHOD_USED), "EPA 552.2", ifelse(
            grepl("*300.0*", df2$METHOD_USED), "EPA 300.0 rev 2.1", ifelse(
              grepl("*2120*", df2$METHOD_USED), "SM 2120 B-01", ifelse(
                grepl("*200.7*", df2$METHOD_USED), "EPA 200.7 rev 4.4", ifelse(
                  grepl("*200.8*", df2$METHOD_USED), "EPA 200.8 rev 5.4", ifelse(
                    grepl("*2540*", df2$METHOD_USED), "SM 2540 C", METHOD_USED
                  )
                )
              )
            )
          )
        )
      )
    )) #iconv(METHOD_USED, "UTF-8", "ASCII", sub = ""),
  )%>% 
    merge(df, by.x = "Site2", by.y = "loc_id") %>% 
    merge(contaminant_codes, by.x = c("parameter" ,"METHOD_USED"), by.y = c("Contaminant", "LIMS_method"), no.dups = T)
  
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
}

#Function Cl2/Col-------------

table_generator_cl2_col <- function(x,y,z) {
  s = drr_sites
  sc = c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples")
  
  month <- ymd(mdy(paste0(x, "01", y)))
  
  #generating lims numbers to filter out if there are coliform samples without matching FCl2
  if("Coliforms Total (Colilert)" %in% z) {
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
  
  #generating Data Frame
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
    METHOD_USED = ifelse(grepl("*9223*", df2$METHOD_USED),  "SM 9223 Colilert", ifelse(
      grepl("*524.2*", df2$METHOD_USED), "EPA 524.2 rev 4.1", ifelse(
        grepl("*6251*", df2$METHOD_USED), "SM 6251 B", ifelse(
          grepl("*552.2*", df2$METHOD_USED), "EPA 552.2", ifelse(
            grepl("*300.0*", df2$METHOD_USED), "EPA 300.0 rev 2.1", ifelse(
              grepl("*2120*", df2$METHOD_USED), "SM 2120 B-01", ifelse(
                grepl("*200.7*", df2$METHOD_USED), "EPA 200.7 rev 4.4", ifelse(
                  grepl("*200.8*", df2$METHOD_USED), "EPA 200.8 rev 5.4", ifelse(
                    grepl("*2540*", df2$METHOD_USED), "SM 2540 C", METHOD_USED
                  )
                )
              )
            )
          )
        )
      )
    )) #iconv(METHOD_USED, "UTF-8", "ASCII", sub = ""),
  ) %>% 
    merge(df, by.x = "Site2", by.y = "loc_id") %>% 
    merge(contaminant_codes, by.x = c("parameter" ,"METHOD_USED"), by.y = c("Contaminant", "LIMS_method"), no.dups = T)

  if(z %in% SDWA1_params & is_empty(col_ln)==T) {
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
  } else{
    if(z %in% SDWA1_params & is_empty(col_ln)==F){
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
    }
  }
}

check_cl2 <- table_generator_cl2_col("September", 2023, "Field-Chlorine Residual Total")


#Function Violation Check Samples--------------

table_generator_vcs <- function(x,y,z){
  sc <- c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples")
  z <- c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)", "E. coli (Colilert)")
  s <- drr_sites
  month <- ymd(mdy(paste0(x, "01", y)))
  
  #generating Data Frame
  
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
    METHOD_USED = ifelse(grepl("*9223*", df2$METHOD_USED),  "SM 9223 Colilert", ifelse(
      grepl("*524.2*", df2$METHOD_USED), "EPA 524.2 rev 4.1", ifelse(
        grepl("*6251*", df2$METHOD_USED), "SM 6251 B", ifelse(
          grepl("*552.2*", df2$METHOD_USED), "EPA 552.2", ifelse(
            grepl("*300.0*", df2$METHOD_USED), "EPA 300.0 rev 2.1", ifelse(
              grepl("*2120*", df2$METHOD_USED), "SM 2120 B-01", ifelse(
                grepl("*200.7*", df2$METHOD_USED), "EPA 200.7 rev 4.4", ifelse(
                  grepl("*200.8*", df2$METHOD_USED), "EPA 200.8 rev 5.4", ifelse(
                    grepl("*2540*", df2$METHOD_USED), "SM 2540 C", METHOD_USED
                  )
                )
              )
            )
          )
        )
      )
    )) #iconv(METHOD_USED, "UTF-8", "ASCII", sub = ""),
  )%>% 
    merge(df, by.x = "Site2", by.y = "loc_id") %>% 
    merge(contaminant_codes, by.x = c("parameter" ,"METHOD_USED"), by.y = c("Contaminant", "LIMS_method"), no.dups = T)
  
  df2 <- df2 %>%  filter(
    parameter == "E. coli (Colilert)" & lims_number %in% df2$lims_number[which(df2$parameter == "Coliforms Total (Colilert)" & df2$result != 0)] | sample_class == "Violation Check Samples"
  ) %>%
    mutate(
      `Loc/EPID2` = padep_id,
    ) %>% 
    rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
    ) %>%
    select(site, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2)
  
  return(df2)
  
}




#DBPs Function-------

#param names from sample_class = "THMs/HAAS Monthly
dbp_params2 <- c("5 Haloacetic acids", "6 Haloacetic acids", "Bromoacetic acid", "Bromochloroacetic acid", "Bromodichloromethane" ,"Bromoform", "Chloroacetic acid", "Chloroform", "Dibromoacetic acid", "Dibromochloromethane", "Dichloroacetic acid", "Total THMs", "Trichloroacetic acid")
#dbp params for reporting
dbp_params <- c("5 Haloacetic acids", "Total THMs", "Bromoform", "Bromodichloromethane", "Dibromochloromethane", "Chloroform", "Dibromoacetic acid", "Dichloroacetic acid", "Bromoacetic acid", "Chloroacetic acid", "Trichloroacetic acid" ) 
#Monobromoacetic acid = Bromoacetic acid in LIMS. Monochloroacetic acid = Chloroacetic acid in LIMS.  "6 Haloacetic acids" & "Bromochloroacetic acid" are sampled in DBP monthly sampling but not included in reporting. 
thms <- c("Total THMs", "Bromoform","Bromodichloromethane", "Dibromochloromethane", "Chloroform" )
haas <- c("5 Haloacetic acids", "Dibromoacetic acid", "Dichloroacetic acid", "Bromoacetic acid", "Chloroacetic acid", "Trichloroacetic acid" )


table_generator_dbps <- function(x,y,z) {
  s = dbp_sites
  pn = "THM_HAA Monthly"
  if(z == "THMs"){z <-  c("Total THMs", "Bromoform","Bromodichloromethane", "Dibromochloromethane", "Chloroform" )}else{
    if(z == "HAAs"){z <-  c("5 Haloacetic acids", "Dibromoacetic acid", "Dichloroacetic acid", "Bromoacetic acid", "Chloroacetic acid", "Trichloroacetic acid" )}
  }
  
  month <- ymd(mdy(paste0(x, "01", y)))
  
  #generating Data Frame
  df2 <- read_LIMS(
    start_date = month,
    end_date = month + months(1),
    parameter = z,
    site = s,
    project_no = pn,
    select_additional = c("METHOD_USED", "ANALYZED_ON", "ANALYZED_BY")
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
                       site %in% drr_sites & sample_class %in% c("Violation Check Samples") & parameter %in% c("E. coli (Colilert)"), "C", ifelse(
                         site %in% dbp_sites & project_no == "THM_HAA Monthly", "D", ""
                       )
                     )
                   )
                 )
               )
             )
           ),
           Result = as.character(round(as.numeric(ifelse(grepl("^<", result_as_entered), 0, result_as_entered))/1000, digits = 4)),
           Site2 = ifelse(grepl("*U", site), str_remove(site, "U$"), ifelse(grepl("*D", site), str_remove(site, "D$"), site))) %>%
    filter(!is.na(result),
           sample_type == "Grab",
           grepl("*ConLab*", ANALYZED_BY)==F)
  
  df2 <- df2 %>% mutate(
    METHOD_USED = ifelse(grepl("*9223*", df2$METHOD_USED),  "SM 9223 Colilert", ifelse(
      grepl("*524.2*", df2$METHOD_USED), "EPA 524.2 rev 4.1", ifelse(
        grepl("*6251*", df2$METHOD_USED), "SM 6251 B", ifelse(
          grepl("*552.2*", df2$METHOD_USED), "EPA 552.2", ifelse(
            grepl("*300.0*", df2$METHOD_USED), "EPA 300.0 rev 2.1", ifelse(
              grepl("*2120*", df2$METHOD_USED), "SM 2120 B-01", ifelse(
                grepl("*200.7*", df2$METHOD_USED), "EPA 200.7 rev 4.4", ifelse(
                  grepl("*200.8*", df2$METHOD_USED), "EPA 200.8 rev 5.4", ifelse(
                    grepl("*2540*", df2$METHOD_USED), "SM 2540 C", METHOD_USED
                  )
                )
              )
            )
          )
        )
      )
    )) #iconv(METHOD_USED, "UTF-8", "ASCII", sub = ""),
  )%>% 
    merge(df, by.x = "Site2", by.y = "loc_id") %>% 
    merge(contaminant_codes, by.x = c("parameter" ,"METHOD_USED"), by.y = c("Contaminant", "LIMS_method"), no.dups = T)
 
    df2 <- df2 %>% 
      mutate(
        `Loc/EPID2` = padep_id,
      ) %>% 
      rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
      ) %>%
      select(site, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2) %>%
      mutate(across(everything(), as.character))
    return(df2) 
}

#for message board, add a count of samples done at contract lab, total count by parameter. 

#Secondary Contaminants (inorganics)--------------
secondaries <- c("Chloride", "Iron", "Sulfate", 
              "Solids Dissolved Total", "Manganese",
              "Silver", "Color Apparent")

secondary_df <- read_LIMS(
  start_date = "2020-11-01",
  end_date = "2023-12-01",
  parameter = secondaries, 
  site = hs_sites,
  sample_class = "Routine Daily",
  select_additional = "METHOD_USED"
)  %>% 
  group_by(parameter, METHOD_USED) %>% summarise(count = n())

#get analysis.method for:  TDS (verify contam code for TDS), Silver
#check site type

table_generator_sec <- function(x,y,z) {
  s = hs_sites
  sc = "Routine Daily"
  month <- ymd(mdy(paste0(x, "01", y)))
  if(z == "Secondaries"){
    z <- c("Chloride", "Iron", "Sulfate", 
           "Solids Dissolved Total", "Manganese",
           "Silver", "Color True")
  }
  
  #generating dataframe 
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
    METHOD_USED = ifelse(grepl("*9223*", df2$METHOD_USED),  "SM 9223 Colilert", ifelse(
      grepl("*524.2*", df2$METHOD_USED), "EPA 524.2 rev 4.1", ifelse(
        grepl("*6251*", df2$METHOD_USED), "SM 6251 B", ifelse(
          grepl("*552.2*", df2$METHOD_USED), "EPA 552.2", ifelse(
            grepl("*300.0*", df2$METHOD_USED), "EPA 300.0 rev 2.1", ifelse(
              grepl("*2120*", df2$METHOD_USED), "SM 2120 B-01", ifelse(
                grepl("*200.7*", df2$METHOD_USED), "EPA 200.7 rev 4.4", ifelse(
                  grepl("*200.8*", df2$METHOD_USED), "EPA 200.8 rev 5.4", ifelse(
                    grepl("*2540*", df2$METHOD_USED), "SM 2540 C", METHOD_USED
                  )
                )
              )
            )
          )
        )
      )
    )) #iconv(METHOD_USED, "UTF-8", "ASCII", sub = ""),
  ) %>% 
    merge(df, by.x = "Site2", by.y = "loc_id") %>% 
    merge(contaminant_codes, by.x = c("parameter" ,"METHOD_USED"), by.y = c("Contaminant", "LIMS_method"), no.dups = T)
  
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
}

sec_check <- table_generator_sec("November", 2023, "Secondaries")


#Creating overarching function----------------

table_generator <- function(x,y,z) {
if(z %in% c("Nitrite", "Nitrate")) {
  df3 <- table_generator_nit(x,y,z)
}else{
  if(z %in% c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)")){
    df3 <- table_generator_cl2_col(x,y,z)
  } else {
    if(z == "Violation Check Samples") {
      df3 <- table_generator_vcs(x,y,z)
    } else{
      if(z =="THMs" | z ==  "HAAs"){
        df3 <- table_generator_dbps(x,y,z)
      } else {
        if(z == "Secondaries" | z %in% secondaries){
          df3 <- table_generator_sec(x,y,z)
        }
      }
    }
  }
}
}

check_sec <- table_generator("December", 2023, "Secondaries")
check_sec2 <- table_generator("December", 2023, "Manganese")




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
          assign(paste0(n, i),table_generator(i,y,x) %>% filter(Contam %in% c("3100", "1000")) %>% pivot_wider(id_cols = c(SampDate, site, `Samp #`), names_from = Contam, values_from = Result))
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


cl2_23 <- check("Field-Chlorine Residual Total", 2022) %>% rename(count.cl2 = "count")
col_23 <- check("Coliforms Total (Colilert)", 2022) %>%  rename(count.col = "count")
vcs_23 <- check("Violation Check Samples", 2022) %>% group_by(month = substr(SampDate, start = 1, stop =2)) %>% summarise(count.vcs = length(unique(`Samp #`)))
combo <- merge(cl2_23, col_23, by="month")
combo <- merge(combo, vcs_23, by = "month", all.x = T) %>%
  mutate(
    count.vcs = ifelse(is.na(count.vcs), 0, count.vcs),
    total.cl2 = count.cl2 + count.vcs,
    total.col = count.col + count.vcs
  )

haas_23 <- check("HAAs", 2023)
thms_23 <- check("THMs", 2023)

