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


# Col1 = c(9,5,8)
# Col2 = c(9,4,7)
# Col3 = c(9,9,5)
# Col4 = c(8,8,7)
# Argu = c(7,6,7)
# df = data.frame(Col1,Col2,Col3,Col4,Argu)
# 
# # Build hidden logical columns for conditional formatting
# dataCol_df <- ncol(df) - 1
# dataColRng <- 1:dataCol_df
# argColRng <- (dataCol_df + 2):(dataCol_df * 2 + 1)
# df[, argColRng] <- df[, dataColRng] < Argu

#UI------
ui <- fluidPage(
                theme = shinythemes::shinytheme("sandstone"),
                
      #'           tags$head(
      #'             tags$style(HTML("
      #'             @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      #' body {
      #'   background-color: black;
      #'   color: white;
      #' }
      #' h2 {
      #'   font-family: 'Yusei Magic', sans-serif;
      #' }
      #' .shiny-input-container {
      #'   color: #474747;
      #' }"
      #'           ))),
      #'           
  titlePanel("Reporting Data"),
  
  # tags$style(HTML("
  # #first {
  # outline: 3px solid black;
  #                 }
  #                 ")),
  # 
  
               #inputs 
               fluidRow(align = "center", style = 'height:400px;',
                 column(2, 
                        selectInput("parameter",
                                    label = "Parameter",
                                    choices = sort(c("Nitrite", "Nitrate", "Field-Chlorine Residual Total", "Coliforms Total (Colilert)", "Violation Check Samples", "HAAs", "THMs", "Secondaries", "TOC", "TOC & Alkalinity", "Metals", "VOCs", "Orthophosphate & pH"))),
                               column(1,
                                      style = 'width:615px;overflow-x: auto;height:200px;overflow-y: auto;',
                                      tableOutput("summary")
                                      
                                      # tags$head(tags$style(HTML("
                                      #       #summary { 
                                      #       color: white;
                                      #       }
                                      #       ")))
                               ),
                        column(2,
                               verbatimTextOutput("text", placeholder = TRUE)
                        )),
                 column(1,selectInput("month", 
                                      label = "Month", 
                                      choices = c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"))
                        ),
                 column(1,
                       textInput("year", label = "Year"),
                       ),
                 column(3,offset = 1,
                        verbatimTextOutput("text2", placeholder = TRUE),
                        column(3,
                                      actionButton(
                                        inputId = "Submit",
                                        label = "Submit")
                                )
                        ),
               
               tags$head(tags$style(HTML("
                            #text { 
                            title: Message Board;
                            width: 600px;
                            height: 100px;
                            color: black;
                            font-size: 15px;
                            position: relative;
                            text-align: center;
                            }
                            "))
               ),
               tags$head(tags$style(HTML("
                            #text2 { 
                            float:left;
                            title: Message Board;
                            width: 1000px;
                            height: 350px;
                            color: black;
                            font-size: 15px;
                            position: relative;
                            text-align:left;
                            }
                            "))
               )
               ),
               fluidRow(
               column(12,
                 DTOutput("table"),
                 
                 #https://www.w3schools.com/css/css_font_size.asp - css basics 
                 #tags$head(tags$style(HTML("
                 #            #table { 
                 #            color: white;
                 #            }
                 #            "))),
                 
                 downloadButton('downloadData', 'Download')
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
    ) %>% subset(select=-c(VALIDATED_ON, ANALYZED_BY)) 
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
         backgroundColor = styleEqual(1, 'yellow')
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
      table2 <- table() %>% group_by(site, `Loc/EPID`) %>% summarise(`Number of Samples` = n(),
                                                      `Contract Samples` = sum(contract_lab == TRUE),
                                                     `Analysis.Methods` = paste(unique(AnalMeth), collapse = " ")) %>% rename("Loc.EPID" = `Loc/EPID`)
      #figure out how to include total column

    )

    output$text <- renderText(
      paste0("Total Samples Taken:", 
             as.character(length(table()$Result)), 
             "\n", 
             "Samples Analyzed by Contract Lab:",
             as.character(sum(table()$contract_lab == TRUE)),
             "\n",
             "Samples Validated:",
             as.character(sum(table()$not_validated == FALSE))
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
shinyApp(ui = ui, server = server)
