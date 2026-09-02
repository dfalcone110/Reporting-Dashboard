
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


#Data Frames and Variables-------

#When the app is ran, it assumes that the working directory is the folder which the app is contained in.
  #If there are files contained in folders prior to the working directory, you will need to specify absolute paths



df <- read_sradb_table("si_site_info")
# df2 <- read.csv("SiteMasterListSRA.csv")

contaminant_codes <- read.csv("contaminant_codes.csv") 

contaminant_codes <-  contaminant_codes%>% mutate(
  Contaminant = str_to_title(str_to_lower(contaminant_codes$Contaminant)),
  Analysis.Method = str_to_title(str_to_lower(contaminant_codes$Analysis.Method)),
  LIMS_method = ifelse(Analysis.Method == "Ion Chrom, Suppress", "EPA 300.0 rev 2.1", ifelse(
    Analysis.Method == "Colormtrc,Cd Redct,Auto (Nox)", "SM 4500 NO3 F", ifelse(
      Analysis.Method == "Hach Method 10260", "Hach Method 10260", ifelse(
        Analysis.Method == "Chromo/Fluorogen (Colilert/18)", "SM 9223 Colilert", ifelse(
          Analysis.Method == "Hach Method 10250", "Hach Method 10250", "")
      )))),
  Contaminant = ifelse(Contaminant == "Total Coliform Presence", "Coliforms Total (Colilert)", Contaminant)
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



#Fucntions------



table_generator <- function(x,y,z){
  
  #site definition
  if(z %in% c("Nitrite", "Nitrate")){s = hs_sites} else {
    if(z %in% c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)")){s = drr_sites
    }
  }
  #sample_class definition
  if(z %in% c("Nitrite", "Nitrate")){sc = "Routine Daily"} else {
    if(z %in% c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)")) {sc = c("Routine Daily","THMs/HAAs Monthly", "Violation Check Samples")
    }
  }
  
  month <- ymd(mdy(paste0(x, "01", y)))
  
  #generating lims numbers to filter out if there are coliform samples without matching FCl2
  if(z == "Coliforms Total (Colilert)") {
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
           SampType = ifelse(
             site %in% drr_sites & sample_class %in% c("Routine Daily", "THMs/HAAs Monthly", "Violation Check Samples") & parameter == "Field-Chlorine Residual Total", "D", ifelse(
               site %in% c(4001, 5004, 6001), "E", ifelse(
                 site %in% drr_sites & sample_class == "Violation Check Samples" & parameter %in% c("Coliforms Total (Colilert)") , "C",ifelse(
                   site %in% drr_sites & sample_class %in% c("Routine Daily", "THMs/HAAs Monthly") & parameter %in% c("Coliforms Total (Colilert)"), "D", " ")
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
  if (z %in% SDWA4_params) {
    
    df2 <- df2 %>% 
      mutate(
        `Location 2` = padep_id,
      ) %>% 
      rename(`Loc/EPID`="padep_id", `Sample #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
      ) %>% 
      select(PWSID, Transcode, Contam, AnalMeth, Result, LLD, CE, AnalDate, `Loc/EPID`, `Location 2`, SampDate,SampType, SampTime, LabID, blank1, blank2,  `Sender ID`, `Sample #`, blank3, blank4 ) %>% 
      mutate(across(everything(), as.character))
    return(df2)
  } else(
    if(z %in% SDWA1_params & is_empty(col_ln)==T){
      df2 <- df2 %>% filter(is.na(project_no) | project_no == "THM_HAA Monthly") %>%
        mutate(
          `Loc/EPID2` = padep_id,
        ) %>% 
        rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
        ) %>%
        select(site, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2) %>%
        mutate(across(everything(), as.character))
      return(df2) 
    }else(
      if(z %in% SDWA1_params & is_empty(col_ln)==F){
        df2 <- df2 %>% filter(is.na(project_no) | project_no == "THM_HAA Monthly",
                              lims_number %in% ln_for_q) %>%
          mutate(
            `Loc/EPID2` = padep_id,
          ) %>% 
          rename(`Loc/EPID`="padep_id", `Samp #` = "lims_number", Contam = "Contam.Code", AnalMeth = "Method.Code"
          ) %>%
          select(site, PWSID, Transcode, Contam, AnalMeth, Result, AnalDate, `Loc/EPID`, SampDate, SampType, SampTime, LabID, `Sender ID`, `Samp #` , blank1, `Loc/EPID2`, blank2) %>%
          mutate(across(everything(), as.character))
        return(df2) 
      }
    )
  )
}



#UI------
ui <- fluidPage(
  
    tabsetPanel(
      tabPanel("Reporting Data", fluid = TRUE,
               titlePanel("Reporting"),
               #inputs 
               sidebarPanel(width = 12,
                 div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput("parameter",
                             label = "Parameter",
                             choices = sort(c(SDWA1_params, SDWA4_params)))),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>")),
                 
                 div(style = "display: inline-block;vertical-align:top; width: 200px;", selectInput("month",
                             label = "Month",
                             choices = c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>")),
                 
                 div(style = "display: inline-block;vertical-align:top; width: 200px;", textInput("year", label = "Year")),
                 
                 submitButton(text = "Submit"),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>"))
               ),
               mainPanel(
                 DTOutput("table"),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;", downloadButton('downloadData', 'Download'))
               )
               )
    )
)

#Server----
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
 
  table <- reactive({
    
    req(input$month)
    req(input$year)
    req(input$parameter)
    table_generator(input$month, input$year, input$parameter)
  })
 
  
    output$table <- renderDT(
     
       table()
      
      # table <- table_generator(input$month, input$year, input$parameter)
      # # ,
      # # extensions = 'Buttons',
      # # 
      # # options = list(
      # #   paging = FALSE,
      # #   searching = TRUE,
      # #   fixedColumns = TRUE,
      # #   autoWidth = TRUE,
      # #   ordering = TRUE,
      # #   dom = 'tB',
      # #   buttons = c('copy', 'csv')
      # # ),
      # # 
      # # class = "display"
    )
    
    
    output$downloadData <- downloadHandler(
      filename = function(){
        paste0(input$month, '_', input$year,'_', input$parameter, '.csv')
      },
      content = function(file) {
        write.table(table_generator(input$month, input$year, input$parameter), file, col.names = FALSE, row.names = FALSE, sep = ",")
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
