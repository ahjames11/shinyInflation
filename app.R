## Shiny Application to view Inflation data from the Joint Inflation Calculator (through CostTools)

library(shiny)
library(costTools)
library(DT)
library(dplyr)
library(tidyr)

library(dygraphs)
library(xts)
library(tbl2xts)

library(dplyr)

service <- c("All", "Army", "Navy", "Marine", "DoD")

# Function to extrapolate an index (to move into costTools)
extrapIndex <- function(SingleIndex, Extrap_End = 2060) {
  
  base_year <- attr(SingleIndex, 'metadata')$Base

  dat <- SingleIndex %>%
    ungroup() %>%
    add_row(Year = seq.int(max(.$Year) + 1, Extrap_End)) %>%
    fill(ID, ShortTitle, Annual, Outlay) %>%
    mutate(Raw = cumprod(Annual))
  
  raw_val <- dat %>%
    filter(Year == base_year) %>%
    select(Raw) %>%
    pull()
  
  dat <- dat %>%
    mutate(Raw = Raw / raw_val,
           Weighted = Raw * Outlay,
           FYStart = CalYear_GovFY(Year)[[1]],
           FYEnd = CalYear_GovFY(Year)[[2]])
  
  # mostattributes(dat) <- attributes(SingleIndex)
  
  return(dat)
}


# Define UI
ui <- fluidPage(theme = "technomics.css",
   
   # Application title
   titlePanel("Joint Inflation Calculator"),
   h5(paste0("Data Version: ", viewVersion())),
   
   fluidRow(
     column(4,
            #h3("Basic Options"),
            wellPanel(
              h4("Select Index"),
              selectInput("service",
                          "Service:",
                          choice = service,
                          selected = "All"),
              selectInput("index",
                          "Index:",
                          choice = NULL),
              br(),
              numericInput("baseyear",
                           "Base Year:",
                           2018,
                           min = 1900,
                           max = 2100,
                           step = 1)
            )
            
            ),
     column(8,
            #h3("Query Results"),
            wellPanel(
              #h4(textOutput("longname")),
              dygraphOutput("graphic", height = "300px")
              ),
            wellPanel(
              dataTableOutput("table")
            )
            )
   )

   
   
)

# Define Server
server <- function(input, output, session) {
  
  #makeReactiveBinding("indexData")
  
  newIndexData <- reactive({
    
    indexData <- extrapIndex(getIndex(input$index, Base = input$baseyear))
    
    return(indexData)
    
  })
  
  observe({
    
    service <- input$service
    
    updateSelectInput(session,
                      "index",
                      choice = viewIndex(service)$ShortTitle)
    
  })
  
  output$table <- renderDataTable(server = FALSE, {
    # server = FALSE has Buttons return all data, not just visible
    
    indexData <- newIndexData() %>%
      select(-FYStart, -FYEnd)
    
    # DataTable Options:
    # t = table
    # p = pagination
    # l = length changing
    # order specifies top or bottom (relative to t)
    # note: may not render correctly in RStudio browswer - open in an external browser
    dt <- datatable(indexData,
                    extensions = "Buttons",
                    options = list(dom = 'Btlp', ordering = F, buttons = c('copy', 'csv', 'excel'), pageLength = 10),
                    rownames= FALSE) %>%
      formatRound(columns = c("Annual", "Outlay", "Raw", "Weighted"), digits = 3)
    
    return(dt)
  })
  
  output$graphic <- renderDygraph({
    
    indexData <- newIndexData()
    
    d <- indexData %>%
      ungroup() %>%
      select(FYStart, Raw, Weighted) %>%
      rename(Date = FYStart) %>%
      tbl_xts()
    
    p <- dygraph(d, main = attr(indexData, "metadata")$LongTitle) %>%
      dySeries("Raw", label = "Raw Index") %>%
      dySeries("Weighted", label = "Weighted Index")
    
    return(p)
  })
  
  output$longname <- renderText({
    
    indexData <- newIndexData()
    
    return(attr(indexData, "metadata")$LongTitle)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


