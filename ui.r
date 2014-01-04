## ui.R
require(rCharts)
#library(googleVis)
library(shinyIncubator)

shinyUI(pageWithSidebar(
  headerPanel("IMDB Trends"),
  
  sidebarPanel(
#     selectInput(inputId = "event",
#                 label = h4("Choose an Event"),
#                 choices = c('World War 2', 'Great Depression', 'Vietnam War', '9/11'),
#                 selected = "World War 2"),
#     h4(' or '),
#     textInput("keywords", h4("Search for keywords:"), value = ""),    
#     submitButton("Search"),
#     br(),
    htmlOutput('wikipedia')
  ),
  mainPanel(
    progressInit(),
    tabsetPanel(
      tabPanel("Great Depression", showOutput("trendGD", 'nvd3'), chartOutput("trendTable", 'datatables'), value='Great Depression'),
      tabPanel("World War 2", showOutput("trendWW2", 'nvd3'), chartOutput("trendTable2", 'datatables'), value='World War 2'),
      tabPanel("Vietnam War", showOutput("trendVW", 'nvd3'), chartOutput("trendTable3", 'datatables'), value='Vietnam War'),
      tabPanel("9/11 Attacks", showOutput("trend911", 'nvd3'), chartOutput("trendTable4", 'datatables'), value='9/11'),
      id='event')
  )
))
