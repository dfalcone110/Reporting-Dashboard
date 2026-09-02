source("variables.R")
source("overarching.R")


#UI------
ui <- fluidPage(
  
    tabsetPanel(
      tabPanel("Reporting Data", fluid = TRUE,
               titlePanel("Reporting"),
               #inputs 
               sidebarPanel(width = 12,
                 div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput("parameter",
                             label = "Parameter",
                             choices = sort(c("Nitrite", "Nitrate", "Field-Chlorine Residual Total", "Coliforms Total (Colilert)", "Violation Check Samples", "HAAs", "THMs", "Secondaries", "TOC", "TOC & Alkalinity", "Metals", "VOCs", "Orthophosphate & pH")))),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>")),
                 
                 div(style = "display: inline-block;vertical-align:top; width: 200px;", selectInput("month",
                             label = "Month",
                             choices = c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"))),
                 
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
