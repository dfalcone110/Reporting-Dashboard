library(shiny)
library(tidyverse)
library(wqr)
library(kableExtra)
library(rmarkdown)
library(knitr)
library(lubridate)
library(stringr)
library(blastula)
library(DT)




#external files/variable-----------
# file.edit("~/.Renviron")

df <- read_sradb_table("si_site_info")
# df2 <- read.csv("SiteMasterListSRA.csv")

# df[nrow(df) + 1,] = c("3913A",FALSE, "Southwest WWTP Alternate Tap", FALSE, FALSE, FALSE, "PWD Facility (8200 Enterprise Ave)", NA, "39.884119", "-75.220746", 745,"Sludge Thickener Building Alternate Tap", FALSE, NA, FALSE, FALSE, FALSE, NA, NA, FALSE, "Belmont", "Belmont Gravity", FALSE, NA,NA)

ioc_params <- c("Antimony", "Arsenic", "Barium", "Beryllium", "Cadmium", "Chromium", "Nickel", "Cyanide Total", "Fluoride", "Mercury", "Selenium", "Thallium")


contaminant_codes <- read.csv("contaminant_codes.csv") 

contaminant_codes <-  contaminant_codes%>% mutate(
  Contaminant = str_to_title(str_to_lower(contaminant_codes$Contaminant)),
  Contaminant = gsub(" \\(Voc\\)", "",Contaminant),
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
                        Analysis.Method == "Grav, Filt, Dry At 180 (Tds)", "SM 2540 C", ifelse(
                          Analysis.Method == "High Temp Combust (Doc/Toc)", "SM 5310 B", ifelse(
                            Analysis.Method == "Titration (Alkalinity)", "SM 2320 B-2011", ifelse(
                              Analysis.Method == "Ion Sel Electr, Manual", "SM 4500-F-C", ifelse(
                                Analysis.Method == "Microdist,Flow Inj, Spec (Cn)", "Lachat 10-204-00-1X", ifelse(
                                  Analysis.Method == "Colrmtrc,Ascrb Acd,Discr (Op)", "SM 4500 P E", ifelse(
                                    Analysis.Method == "Electrometric (Ph)", "SM 4500-H+ B", ifelse(
                                      Analysis.Method == "Aa, Cold Vapor, Manual", "SM 3112-B",ifelse(
                                        Analysis.Method == "Hach Method 8167", "Hach Method 8167",""
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
                              Contaminant == "Tds (Filterable)", "Solids Dissolved Total", ifelse(
                                Contaminant == "Toc", "TOC", ifelse(
                                  Contaminant == "Alkalinity - Total", "Alkalinity", ifelse(
                                    Contaminant == "Antimony (Ioc)", "Antimony", ifelse(
                                      Contaminant == "Arsenic (Ioc)", "Arsenic", ifelse(
                                        Contaminant == "Barium (Ioc)", "Barium", ifelse(
                                          Contaminant == "Beryllium (Ioc)", "Beryllium", ifelse(
                                            Contaminant == "Cadmium (Ioc)", "Cadmium", ifelse(
                                              Contaminant == "Chromium (Ioc)", "Chromium", ifelse(
                                                Contaminant == "Nickel (Ioc)", "Nickel", ifelse(
                                                  Contaminant == "Cyanide (Free) (Ioc)", "Cyanide Total", ifelse(
                                                    Contaminant == "Fluoride (Ioc)", "Fluoride", ifelse(
                                                      Contaminant == "Mercury (Ioc)", "Mercury", ifelse(
                                                        Contaminant == "Selenium (Ioc)", "Selenium", ifelse(
                                                          Contaminant == "Thallium (Ioc)", "Thallium", ifelse(
                                                            Contaminant == "1,1-Dichloroethylene", "1,1-Dichloroethene", ifelse(
                                                              Contaminant == "Cis-1,2-Dichloroethylene", "cis-1,2-Dichloroethene", ifelse(
                                                                Contaminant == "O-Dichlorobenzene", "1,2-Dichlorobenzene", ifelse(
                                                                  Contaminant == "P-Dichlorobenzene", "1,4-Dichlorobenzene", ifelse(
                                                                    Contaminant == "Tetrachloroethylene", "Tetrachloroethene", ifelse(
                                                                      Contaminant == "Trichloroethylene", "Trichloroethene", ifelse(
                                                                        Contaminant == "Trans-1,2-Dichloroethene", "trans-1,2-Dichloroethene", ifelse(
                                                                          Contaminant == "Carbon Tetrachloride", "Carbon tetrachloride" ,ifelse(
                                                                            Contaminant == "Ph", "pH", ifelse(
                                                                              Contaminant == "Field-Ph", "Field-pH", ifelse(
                                                                                Contaminant == "Vinyl Chloride", "Vinyl chloride", ifelse(
                                                                                  Contaminant == "O-Xylene", "o-Xylene", ifelse(
                                                                                    Contaminant == "M,P-Xylenes", "m,p-Xylenes", Contaminant
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

drr_sites2 <- str_replace_all(drr_sites, 'U', "") %>%  unique()
drr_sites2 <- str_replace_all(drr_sites2, 'D', "") %>% unique()

dbp_sites <- df$loc_id[df$dbp_site == TRUE]
occt_sites <- df$loc_id[df$occt_site == TRUE]
nitrification_sites <- df$loc_id[df$nitrification_site == TRUE]
hs_sites <- c(4001, 5004, 6001)
plant_sites <- c(4503, 6501, 6502, 5501, 5502)
river_sites <- c(4903, 6903, 5903)
# st_sites <- df$LOC_ID[df$STORAGE == "Y"]



# pump_sites <- df$LOC_ID[df$PUMPING_ST == "Y"]
st_eff_sites <- c(7101, 7207, 7204, 7301, 7302, 7401, 7502, 7601)

metals <- c("Arsenic", "Antimony", "Barium", "Beryllium", "Cadmium", "Chromium", "Nickel", "Selenium", "Thallium", "Cyanide Total", "Fluoride", "Mercury")
secondaries <- c("Chloride", "Iron", "Sulfate", 
                 "Solids Dissolved Total", "Manganese",
                 "Silver", "Color Apparent")
soc_params <- c("Alachlor","Atrazine","Benzo(a)pyrene","Carbofuran","Chlordane","2,4-D","Dalapon","1,2-Dibromo-3-chloropropane","Bis(2-Ethylhexyl)adipate","Bis(2-Ethylhexyl)phthalate","Dinoseb","Diquat","Endothall","Endrin","Ethylene dibromide","Glyphosate","Heptachlor","Heptachlor epoxide","Hexachlorobenzene","Hexachlorocyclopentadiene","Lindane","Methoxychlor","Oxamyl","PCBs Total","Pentachlorophenol","Picloram","Simazine","Dioxin","Toxaphene","2,4,5-T")
voc_params <- c("Benzene","cis-1,2-Dichloroethene","Chlorobenzene","1,2-Dichlorobenzene","1,2-Dichloroethane","1,2-Dichloropropane","1,4-Dichlorobenzene","1,1-Dichloroethene","Ethylbenzene","Dichloromethane","m,p-Xylenes","o-Xylene","Styrene","trans-1,2-Dichloroethene","Tetrachloroethene","1,2,4-Trichlorobenzene","1,1,1-Trichloroethane","1,1,2-Trichloroethane","Trichloroethene","Toluene","Carbon tetrachloride","Vinyl chloride")
radio_params <- c("Uranium Combined", "Alpha Total", "Radium 226 Total", "Radium 228 Total", "Beta Total")





additional <- lims_column_names$column_names

SDWA4_col_csv <-c("PWSID","Transcode","Contam","AnalMeth","Result","LLD","CE","AnalDate","Loc/EPID","Location 2","SampDate","SampType","SampTime","LabID","blank","blank","Sender ID","Sample #")
SDWA1_Col_csv <- c("PWSID",	"Transcode",	"Contam",	"AnalMeth",	"Result",	"AnalDate",	"Loc/EPID",	"SampDate",	"SampType",	'SampTim',	'LabID',	"Sender ID",	"Samp #","blank",	"Loc/EPID2",	"blank")
SDWA1_params <- c("Coliforms Total (Colilert)","Field-Chlorine Residual Total", "E. Coli (Colilert)", "5 Haloacetic acids", "Total THMs", "Alkalinity")
SDWA4_params <- c("Arsenic", "Nitrate", "Nitrite", "Orthophosphate", "pH", "Cyanide Total", "Chloride", "Iron", "Manganese", "Silver", "Solids Dissolved Total") #still need to add SOCs and VOCs, Color, Sulfate


