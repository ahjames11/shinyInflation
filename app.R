#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(costTools)
library(DT)

service <- c("All", "Army", "Navy", "Marine", "DoD")


# Define UI
ui <- fluidPage(theme = "technomics.css",
   
   # Application title
   titlePanel("Joint Inflation Calculator"),
   
   fluidRow(
     column(4,
            h3("Basic Options"),
            wellPanel(
              h4("Select Index"),
              selectInput("service",
                          "Service:",
                          choice = service,
                          selected = "All"),
              selectInput("index",
                          "Index:",
                          choice = NULL),
              sliderInput("daterange",
                          "Date Range:",
                          min = 1900,
                          max = 2100,
                          value = c(1900, 2100),
                          step = 1,
                          sep = ""
                          )
              
            )
            
            ),
     column(8,
            h3("Query Results"),
            wellPanel(
              h4(textOutput("longname")),
              dataTableOutput("table")
              )
            
            
            )
   )

   
   
)

# Define Server
server <- function(input, output, session) {
  
  #makeReactiveBinding("indexData")
  
  newIndexData <- reactive({
    
    indexData <- getIndex(input$index)
    
    return(indexData)
    
  })
  
  observe({
    updateSelectInput(session,
                      "index",
                      choice = viewIndex(input$service)$ShortTitle)
    
    indexData <- newIndexData()
    
    updateSliderInput(session,
                      "daterange",
                      min = min(indexData$Year),
                      max = max(indexData$Year))
    
  })
  
  
  
  output$table <- renderDataTable({
    
    indexData <- newIndexData() %>%
      select(-FYStart, -FYEnd) %>%
      filter(Year >= input$daterange[1], Year <= input$daterange[2])
    
    # DataTable Options:
    # t = table
    # p = pagination
    # l = length changing
    # order specifies top or bottom (relative to t)
    dt <- datatable(indexData, options = list(dom = 'ltlp'), rownames= FALSE) %>%
      formatRound(columns = c("Annual", "Outlay", "Raw", "Weighted"), digits = 3)
    
    return(dt)
  })
  
  output$longname <- renderText({
    
    indexData <- newIndexData()
    
    return(attr(indexData, "metadata")$LongTitle)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

