
# source("variables.R")



#all functions contain the following code----------

  #DBP filters by pn (project number) instead of sample class, DBPS has a separate calculation done on result

fn1 <- function(my_param, st, en, my_site, samp_class){
  #alternate code for using same inputs as outer function and calling the below variables into inner funtion-----
  # my_month <- get("my_month", parent.frame())
  # sc <- get("sc", parent.frame())
  # si <- get("si", parent.frame())
  # z <- get("z", parent.frame())

  # defining variables for query------
  df2 <- read_LIMS(
    start_date =  as.Date(st),
    end_date = as.Date(en),
    parameter = my_param,
    site = my_site,
    sample_class = samp_class,
    select_additional = c("METHOD_USED", "ANALYZED_ON", "ANALYZED_BY", "VALIDATED_ON")
  ) %>% 
    separate(date_time, into = c("SampDate", "SampTime"), sep = " ") %>%
    separate(ANALYZED_ON, into = c("AnalDate", "AnalTime"), sep = " ")%>%  
    mutate(SampDate2 = SampDate,
           SampDate = gsub('-','',(as.character(as.Date(SampDate, format = '%Y-%m-%d'), format = "%m-%d-%y"))),
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
               site %in% c(4001, 5004, 6001) & parameter %in% c("Nitrite", "Nitrate"), "E", ifelse(
                 site %in% drr_sites & sample_class == "Violation Check Samples" & parameter %in% c("Coliforms Total (Colilert)") , "C",ifelse(
                   site %in% drr_sites & sample_class %in% c("Routine Daily", "THMs/HAAs Monthly") & parameter %in% c("Coliforms Total (Colilert)"), "D", ifelse(
                     site %in% drr_sites & sample_class %in% c("Routine Daily", "THMs/HAAs Monthly") & parameter %in% c("E. coli (Colilert)"), "D", ifelse(
                       site %in% drr_sites & sample_class %in% c("Violation Check Samples") & parameter %in% c("E. coli (Colilert)"), "C", ifelse(
                         site %in% dbp_sites & project_no == "THM_HAA Monthly", "D", ifelse(
                           site %in% plant_sites, "P", ifelse(
                             site %in% river_sites, "R", ifelse(
                               site %in% c(4001, 5004, 6001) & parameter %in% c("Chloride", "Iron", "Sulfate","Solids Dissolved Total", "Manganese","Silver", "Color Apparent"), "S", ifelse(
                                 site %in% c(4001, 5004, 6001) & parameter %in% setdiff(metals, "Arsenic") & quarter(st) < 4, "S", ifelse(
                                   site %in% c(4001, 5004, 6001) & parameter %in% setdiff(metals, "Arsenic") & quarter(st) == 4, "E", ifelse(
                                     site %in% c(4001, 5004, 6001) & parameter == "Arsenic", "E", ifelse(
                                       site %in% c(4001, 5004, 6001) & parameter %in% voc_params, "E", ifelse(
                                         site %in% c(4001, 5004, 6001) & parameter %in% c("Orthophosphate", "Field-pH"),"E", ifelse(
                                           site %in% occt_sites & parameter %in% c("Orthophosphate", "Field-pH"), "D", ""
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
                   )
                 )
               )
             ),
           Result = ifelse(grepl("^<", result_as_entered), 0, result_as_entered),
           Site2 = ifelse(grepl("*U", site), str_remove(site, "U$"), ifelse(grepl("*D", site), str_remove(site, "D$"), site))) %>%
    filter(!is.na(result),
           sample_type == "Grab",
           # grepl("*ConLab*", ANALYZED_BY)==F
           )
  

  df2 <- df2 %>% mutate(
    METHOD_USED = ifelse(str_detect(df2$METHOD_USED, "9223"),  "SM 9223 Colilert", ifelse(
      grepl("*524.2*", df2$METHOD_USED), "EPA 524.2 rev 4.1", ifelse(
        grepl("*6251*", df2$METHOD_USED), "SM 6251 B", ifelse(
          grepl("*552.2*", df2$METHOD_USED), "EPA 552.2", ifelse(
            grepl("*300.0*", df2$METHOD_USED), "EPA 300.0 rev 2.1", ifelse(
              grepl("*2120*", df2$METHOD_USED), "SM 2120 B-01", ifelse(
                grepl("*200.7*", df2$METHOD_USED, fixed = TRUE), "EPA 200.7 rev 4.4", ifelse(
                  grepl("*200.8*", df2$METHOD_USED, fixed = TRUE), "EPA 200.8 rev 5.4", ifelse(
                    grepl("*2540*", df2$METHOD_USED), "SM 2540 C", ifelse(
                      grepl("*2320*", df2$METHOD_USED), "SM 2320 B-2011", ifelse(
                        grepl("*5310*", df2$METHOD_USED), "SM 5310 B", ifelse(
                          grepl("*10-204*", df2$METHOD_USED), "Lachat 10-204-00-1X", ifelse(
                            grepl("*4500", df2$METHOD_USED) ==T & grepl("*F", df2$METHOD_USED) ==T, "SM 4500-F-C", ifelse(
                              grepl("*4500", df2$METHOD_USED) ==T & grepl("*P", df2$METHOD_USED) ==T, "SM 4500 P E", ifelse(
                                grepl("*4500", df2$METHOD_USED) ==T & grepl("*H", df2$METHOD_USED) ==T, "SM 4500 ", ifelse(
                                  grepl("*8156", df2$METHOD_USED)==T, "SM 4500-H+ B", ifelse(
                                    grepl("*3112", df2$METHOD_USED)==T & grepl("*B", df2$METHOD_USED)==T, "SM 3112-B", METHOD_USED
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
          )
        )
      )
    )) 
  )%>%
  merge(df, by.x = "Site2", by.y = "loc_id")%>%
  merge(contaminant_codes, by.x = c("parameter" ,"METHOD_USED"), by.y = c("Contaminant", "LIMS_method"), no.dups = T)
}



#check <- fn1("Nitrite", "2023-06-01", "2023-07-01", c(4001, 5004, 6001), "Routine Daily")
#check <- fn1("Nitrite", "2023-06-01", "2023-07-01", c(4001, 5004, 6001), "Routine Daily")
# check <- fn1("5 Haloacetic Acids", "2024-01-01", "2024-02-01", dbp_sites, c("Routine Daily", "THMs/HAAs Monthly"))

# fl <- "SM 4500-F-C"
# orth <- "SM 4500 P E"
# hey <- "hello"
# test1 <- grepl("*10-204*", fl)
# test2 <- grepl("*F", orth)
# 
# x <- "SM 4500 P E"
# x <- "SM 4500-F-C"
# x <- "SM 4500-H+ B"
# 
# y <- ifelse(grepl("*4500", x) == T & grepl("*H", x)==T, "True", "False")
# 
# check2 <- read_LIMS(
#   start_date = "2018-12-01",
#   end_date = "2024-01-01",
#   parameter = c( c("Total THMs", "Bromoform","Bromodichloromethane", "Dibromochloromethane", "Chloroform"), c("5 Haloacetic acids", "Dibromoacetic acid", "Dichloroacetic acid", "Bromoacetic acid", "Chloroacetic acid", "Trichloroacetic acid" )),
#   project_no = "THM_HAA Monthly",
#   select_additional = additional
# )

