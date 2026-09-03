# source("variables.R")

variable_assignment <- function(x,y,z){
  
  #dates-------
  
  
 
  if(x %in% c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")){
    assign("st", ymd(mdy(paste0(x, "01", y))), envir = parent.frame())
    assign("en", ymd(mdy(paste0(x, "01", y))) + months(1), envir = parent.frame())
  }else(
    if(x %in% c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4")){
      assign("st", as.Date(ifelse(x == "Quarter 1", ymd(mdy(paste0("01", "01", y))),ifelse(
        x == "Quarter 2", ymd(mdy(paste0("04", "01", y))), ifelse(
          x == "Quarter 3", ymd(mdy(paste0("07", "01", y))), ifelse(
            x == "Quarter 4", ymd(mdy(paste0("10", "01", y))), ""
          )
        )
      )), origin = "1970-01-01"), envir = parent.frame())
      
      assign("en", as.Date(ifelse(x == "Quarter 1", ymd(mdy(paste0("01", "01", y))) + months(3),ifelse(
        x == "Quarter 2", ymd(mdy(paste0("04", "01", y))) + months(3), ifelse(
          x == "Quarter 3", ymd(mdy(paste0("07", "01", y))) +months(3), ifelse(
            x == "Quarter 4", ymd(mdy(paste0("10", "01", y))) + months(3), ""
          )
        )
      )), origin = "1970-01-01"), envir = parent.frame())
      
      }
  )

  
  if(z %in% c("Nitrite/Nitrate")){
    assign("si", hs_sites, envir = parent.frame())
    assign("sc", "Routine Daily", envir = parent.frame())
    assign("z", c("Nitrite", "Nitrate"), envir = parent.frame())
  }else(
    if(z %in% c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)")){
      assign("si", drr_sites, envir = parent.frame())
      assign("sc", c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples"), envir = parent.frame())
    } else (
      if(z == "Violation Check Samples"){
        assign("si", drr_sites, envir = parent.frame())
        assign("sc", c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples"), envir = parent.frame())
        assign("z", c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)", "E. coli (Colilert)"), envir = parent.frame())
      }else(
        if(z == "THMs"){
          assign("si", dbp_sites, envir = parent.frame())
          assign("z", c("Total THMs", "Bromoform","Bromodichloromethane", "Dibromochloromethane", "Chloroform"), envir = parent.frame())
          assign("sc", c(), envir = parent.frame())
        }else(
          if(z == "HAAs"){
            assign("si", dbp_sites, envir = parent.frame())
            assign("z", c("5 Haloacetic acids", "Dibromoacetic acid", "Dichloroacetic acid", "Bromoacetic acid", "Chloroacetic acid", "Trichloroacetic acid" ), envir = parent.frame())
            assign("sc", c(), envir = parent.frame())
          }else(
            if( z == "Secondaries"){
              assign("si", hs_sites, envir = parent.frame())
              assign("z", secondaries, envir = parent.frame())
              assign("sc", "Routine Daily", envir = parent.frame())
              
            }else(
              if(z %in% secondaries){
                assign("si", hs_sites, envir = parent.frame())
                assign("sc", "Routine Daily", envir = parent.frame())
              }else(
                if(z %in% c("TOC","Alkalinity")){
                  assign("si", c(plant_sites, river_sites), envir = parent.frame())
                  assign("sc",  c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily"), envir = parent.frame())
                } else(
                  if( z == "TOC & Alkalinity"){
                    assign("si", c(plant_sites, river_sites), envir = parent.frame())
                    assign("sc",  c("Routine Intakes Weekly", "Routine Intakes Monthly", "Routine Daily"), envir = parent.frame())
                    assign("z", c("TOC", "Alkalinity"), envir = parent.frame())
                  } else(
                    if (z == "Metals"){
                      assign("si",  hs_sites, envir = parent.frame())
                      assign("sc", "Routine Daily", envir = parent.frame())
                      assign("z", metals, envir = parent.frame())
                    } else(
                      if( z %in% metals){
                        assign("si", hs_sites, envir = parent.frame())
                        assign("sc", "Routine Daily", envir = parent.frame())
                      } else (
                        if( z == "VOCs"){
                          assign("si", hs_sites, envir = parent.frame())
                          assign("sc", "Routine Daily", envir = parent.frame())
                          assign("z", voc_params, envir = parent.frame())
                        }else(
                          if( z == "Orthophosphate & pH"){
                            assign("si", c(hs_sites,occt_sites), envir = parent.frame())
                            assign("sc", "Routine Daily",envir = parent.frame())
                            assign("z", c("Orthophosphate", "Field-pH"), envir = parent.frame())
                          }else(
                            if( z == "LCR"){
                              assign("si", c(), envir = parent.frame())
                              assign("sc", "Lead/Copper", envir = parent.frame())
                              assign("z", c("Lead", "Copper"), envir = parent.frame())
                            }
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
}



# variable_assignment("January", 2026, "Nitrite/Nitrate")

#check <- variable_assignment("Solids Dissolved Total")
# variable_assignment("Secondaries")