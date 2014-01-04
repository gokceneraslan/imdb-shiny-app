## ui.R
require(rCharts)
#library(googleVis)
library(shinyIncubator)

shinyUI(pageWithSidebar(
  headerPanel("IMDB Trends"),
  
  sidebarPanel(
    selectInput(inputId = "event",
                label = h4("Choose an Event"),
                choices = c('World War 2', 'Great Depression', 'Vietnam War', '9/11'),
                selected = "World War 2"),
    h4(' or '),
    textInput("keywords", h4("Search for keywords:"), value = ""),    
    submitButton("Search"),
    br(),
    htmlOutput('wikipedia')
  ),
  mainPanel(
    progressInit(),
    showOutput("trendChart", 'nvd3'),
    chartOutput("trendTable", 'datatables')
    #dataTableOutput("trendTable")
  )
))
