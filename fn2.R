
# source("variables.R")



#all functions contain the following code----------

  #DBP filters by pn (project number) instead of sample class, DBPS has a separate calculation done on result

fn2 <- function(my_param, st, en, my_site, samp_class){
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
    select_additional = c("METHOD_USED", "ANALYZED_ON", "ANALYZED_BY", "VALIDATED_ON", "TRESULT")
  ) %>% 
    separate(date_time, into = c("SampDate", "SampTime"), sep = " ") %>%
    separate(ANALYZED_ON, into = c("AnalDate", "AnalTime"), sep = " ")%>%  
    mutate(SampDate2 = SampDate,
           SampDate = gsub('-', '', format(as.Date(SampDate, format = '%Y-%m-%d'), format = "%m-%d-%y")),
           SampTime = str_remove(gsub(":", '', SampTime),"00$"),
           AnalDate = gsub('-', '', format(as.Date(AnalDate, format = '%Y-%m-%d'), format = "%m-%d-%y")),
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
    filter(#!is.na(result),
           sample_type == "Grab",
           # grepl("*ConLab*", ANALYZED_BY)==F
           ) 



  df2 <- df2 %>% mutate(
    METHOD_USED = ifelse(str_detect(df2$METHOD_USED, "9223"),  "SM 9223 Colilert", ifelse(
      str_detect(df2$METHOD_USED, "524.2"), "EPA 524.2 rev 4.1", ifelse(
        str_detect(df2$METHOD_USED, "6251"), "SM 6251 B", ifelse(
          str_detect(df2$METHOD_USED, "552.2"), "EPA 552.2", ifelse(
            str_detect(df2$METHOD_USED, "300.0"), "EPA 300.0 rev 2.1", ifelse(
              str_detect(df2$METHOD_USED, "2120"), "SM 2120 B-01", ifelse(
                str_detect(df2$METHOD_USED, "200.7"), "EPA 200.7 rev 4.4", ifelse(
                  str_detect(df2$METHOD_USED, "200.8"), "EPA 200.8 rev 5.4", ifelse(
                    str_detect(df2$METHOD_USED, "2540"), "SM 2540 C", ifelse(
                      str_detect(df2$METHOD_USED, "2320"), "SM 2320 B-2011", ifelse(
                        str_detect(df2$METHOD_USED, "5310"), "SM 5310 B", ifelse(
                          str_detect(df2$METHOD_USED, "10-204"), "Lachat 10-204-00-1X", ifelse(
                            str_detect(df2$METHOD_USED, "4500") ==T & str_detect(df2$METHOD_USED, "F") ==T, "SM 4500-F-C", ifelse(
                              str_detect(df2$METHOD_USED, "4500") ==T & str_detect(df2$METHOD_USED, "P") ==T, "SM 4500 P E", ifelse(
                                str_detect(df2$METHOD_USED, "4500") ==T & str_detect(df2$METHOD_USED, "H") ==T, "SM 4500 ", ifelse(
                                  str_detect(df2$METHOD_USED, "8156")==T, "SM 4500-H+ B", ifelse( #Method 8156 is for field-pH
                                    str_detect(df2$METHOD_USED, "3112")==T & str_detect(df2$METHOD_USED, "B")==T, "SM 3112-B", METHOD_USED
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
  )

  df2 <- df2 %>% mutate(METHOD_USED = ifelse(parameter == "Lead", "hey", METHOD_USED))
  
  missing_method <- unique(setdiff(df2$METHOD_USED, contaminant_codes$LIMS_method))

  return(missing_method)
}

#check2 <- fn2(c("Lead","Copper"),"2025-06-01","2025-07-01",c(), "Lead/Copper")