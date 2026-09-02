source("variables.R")
source("overarching.R")
source("variables.R")
source("fn1.R")
source("variable_assignment.R")
source("nit.R")
source("cl2_col.R")
source("vcs.R")
source("dbps.R")
source("secondaries.R")
source("TOC.R")
source("Metals.R")
source("VOCs.R")
source("pH_Ortho.R")
source("datacleaner.R")
source("text2output.R")
source("FCl2vsCol.R")



#UI------
ui <- fluidPage(
theme = shinythemes::shinytheme("sandstone"),
                
            
  titlePanel("Reporting Data"),
  
  
               #inputs 
               sidebarLayout(
                 sidebarPanel(width = 3,
                   selectInput("parameter",
                               label = "Parameter",
                               choices = sort(c("Nitrite", "Nitrate", "Field-Chlorine Residual Total", "Coliforms Total (Colilert)", "Violation Check Samples", "HAAs", "THMs", "Secondaries", "TOC", "TOC & Alkalinity", "Metals", "VOCs", "Orthophosphate & pH"))),
                   selectInput("month", 
                               label = "Month", 
                               choices = c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4")),
                   textInput("year", label = "Year"),
                   actionButton(
                     inputId = "Submit",
                     label = "Submit"),
                   downloadButton('downloadData', 'Download'),
                   verbatimTextOutput("text", placeholder = TRUE),
                   tags$head(tags$style(HTML("
                            #text { 
                            title: Message Board;
                            width: auto;
                            height: auto;
                            color: black;
                            font-size: 12px;
                            position: relative;
                            text-align: center;
                            }
                            ")
                   )),
                   selectInput("summaryinput", 
                               label = "Count By", 
                               choices = c("Parameter", "Site")),
                   tableOutput("summary"),
                   tags$head(tags$style(HTML("
                            #summary { 
                            width: auto;
                            overflow-x: auto;
                            height: 500px;
                            overflow-y:auto
                            color: black;
                            font-size: 12px;
                            position: relative;
                            text-align: center;
                            }
                            "))),
                 ),
                 mainPanel(width = 8,
                   fluidRow(
                            verbatimTextOutput("text2", placeholder = TRUE),
                            tags$head(tags$style(HTML("
                            #text2 { 
                            float:left;
                            title: Message Board;
                            width: 1400;
                            height: 250px;
                            color: black;
                            font-size: 12px;
                            position: relative;
                            text-align:left;
                            }
                            "))
                     )
                   ),
                   fluidRow(
                     p(style = "font-size:15px;font-weight:1000", "Legend:"),
                     p(style = "font-size:12px; background-color: yellow; width:200px;font-weight:500;", "Samples analyzed by Contract lab"),
                     p(style = "font-size:12px; background-color: orange; width:200px; font-weight:500;position: relative;", "Samples not Validated"),

                   ),
                   fluidRow(
                          DTOutput("table")
                          
                          #https://www.w3schools.com/css/css_font_size.asp - css basics 
                          #tags$head(tags$style(HTML("
                          #            #table { 
                          #            color: white;
                          #            }
                          #            "))),

                   
                 )
                 )
                 )

                  
)


#Server----
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  table <- eventReactive(input$Submit,{

    req(input$month)
    req(input$year)
    req(input$parameter)
    table_generator(input$month, input$year, input$parameter)%>% mutate(
      contract_lab = ifelse(grepl("*ConLab*", ANALYZED_BY), TRUE, FALSE),
      not_validated = ifelse(is.na(VALIDATED_ON), TRUE ,FALSE)
    ) %>% subset(select=-c(VALIDATED_ON, ANALYZED_BY)) %>% arrange(AnalDate, `Loc/EPID`,SampType) 
  })
  

  lms <- eventReactive(input$Submit,{
    req(input$month)
    req(input$year)
    req(input$parameter)
    fcl2vscol(input$month, input$year, input$parameter)
  })


    output$table <- renderDT({

       df <- table()
       df <- datatable(df, options = list(iDisplayLength = 50, dom = 'pl')) %>% formatStyle(
         'contract_lab',
         target = 'row',
         backgroundColor = styleEqual(1, 'yellow')
       ) %>% formatStyle(
         'not_validated',
         target = 'row',
         backgroundColor = styleEqual(1, 'orange')
       ) 
       

  #
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
    })

    output$summary <- renderTable(
      if (input$summaryinput == "Site") {
        table2 <- table() %>% group_by(site, `Loc/EPID`) %>% summarise(`Number of Samples` = n(),
                                                                                                        `Contract Samples` = sum(contract_lab == TRUE),
                                                                                                        `Analysis.Methods` = paste(unique(AnalMeth), collapse = " ")) %>% rename("Loc.EPID" = `Loc/EPID`)
      } else {if (input$summaryinput == "Parameter") {
        table2 <- table() %>% group_by(parameter) %>% summarise(`Number of Samples` = n(),
                                                                       `Contract Samples` = sum(contract_lab == TRUE),
                                                                       `Analysis.Methods` = paste(unique(AnalMeth), collapse = " "))
        }}

    )
    


    output$text <- renderText(
      paste0("Total Samples Taken: ", 
             as.character(length(table()$Result)), 
             "\n", 
             "Samples Analyzed by Contract Lab: ",
             as.character(sum(table()$contract_lab == TRUE)),
             "\n",
             
             "Samples Validated: ",
             as.character(sum(table()$not_validated == FALSE)),
             
             ifelse("Coliforms Total (Colilert)" %in% table()$parameter, paste0("\n", "Coliform Results Without Matching Cl2: ", lms()), ifelse("Field-Chlorine Residual Total" %in% table()$parameter, paste0("\n", "FCl2 Results Without Matching Coliforms: ", lms()),"")
             ),
             ifelse("Coliforms Total (Colilert)" %in% table()$parameter, paste0("\n", "Coliform Positives: ", length(table()$Result[which(table()$Result != 0 & table()$parameter == "Coliforms Total (Colilert)")])), ifelse(
               "Field-Chlorine Residual Total" %in% table()$parameter, paste0("\n", "FCl2 Samples less than 0.14 mg/L: ", length(table()$Result[which(table()$Result < 0.14 & table()$parameter == "Field-Chlorine Residual Total")])),""
             ))
    )
    )

    output$text2 <- renderText(
     text2output(input$parameter)
      
      )
  





    output$downloadData <- downloadHandler(
      filename = function(){
        paste0(input$month, '_', input$year,'_', input$parameter, '.csv')
      },
      content = function(file) {
        write.table(table_clean(input$month, input$year, input$parameter), file, col.names = FALSE, row.names = FALSE, sep = ",")
      }
    )
}
# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

# check2 <- read_LIMS(
#   start_date = "2024-04-01",
#   end_date = "2024-07-01",
#   parameter = c("Coliforms Total (Colilert)", "Field-Chlorine Residual Total"),
#   sample_class = c("Routine Daily", "Violation Check Samples", "THMs/HAAs Monthly"),
#   site = drr_sites,
#   select_additional = additional
# ) %>% pivot_wider(id_cols = c(lims_number, VALIDATED_ON),
#                   names_from = parameter,
#                   values_from = result)
# # 
# check3 <- read_LIMS(
#   start_date = "2024-04-01",
#   end_date = " 2024-05-01",
#   # lims_number = "DW240415-018",
#   parameter = c("Coliforms Total (Colilert)", "Field-Chlorine Residual Total"),
#   sample_class = c("Routine Daily", "Violation Check Samples", "THMs/HAAs Monthly"),
#   site = drr_sites,
#   select_additional = "VALIDATED_ON"
# ) %>% filter(is.na(VALIDATED_ON))

# check <- read_LIMS(
#   start_date = "2024-06-01",
#   end_date = "2024-07-01",
#   parameter = c("Field-Chlorine Residual Total", "Coliforms Total (Colilert)"),
#   lims_number  = c("DW240614-025", "DW240614-026", "DW240614-001", "DW240614-020", "DW240614-037", "DW240614-038", "DW240614-019", "DW240614-018", "DW240614-033", "DW240614-034", "DW240614-035", "DW240614-036", "DW240614-002", "DW240614-016", "DW240614-017", "DW240614-011", "DW240614-012", "DW240614-013")
# )
