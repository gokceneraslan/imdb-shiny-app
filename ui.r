## ui.R
require(rCharts)
#library(googleVis)
library(shinyIncubator)

# Define UI
shinyUI(fluidPage(
  
  titlePanel("Impacts of Catastrophic Events on Cinema"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    conditionalPanel(condition='input.event != "Main" && input.event != "Final"',
                     sidebarPanel(
                       uiOutput('keywordsearch'),
                       htmlOutput('wikipedia')
                     )),
    mainPanel(
      progressInit(),
      tabsetPanel(
        tabPanel("Welcome", htmlOutput('mainText'), br(), showOutput("trendMain", 'nvd3'), value='Main'),
        tabPanel("Great Depression", htmlOutput('GDText'), br(), showOutput("trendGD", 'nvd3'), br(), hr(), showOutput("trendGD2", 'nvd3'), chartOutput("trendTable", 'datatables'), value='Great Depression'),
        tabPanel("World War 2", htmlOutput('WW2Text'), br(), showOutput("trendWW2", 'nvd3'), br(), hr(), showOutput("trendWW22", 'nvd3'), chartOutput("trendTable2", 'datatables'), value='World War 2'),
        tabPanel("Vietnam War", htmlOutput('VIETNAMText'), br(), showOutput("trendVW", 'nvd3'), br(), hr(), showOutput("trendVW2", 'nvd3'), chartOutput("trendTable3", 'datatables'), value='Vietnam War'),
        tabPanel("9/11 Attacks", htmlOutput('SepText'), br(), showOutput("trend911", 'nvd3'), br(), hr(), showOutput("trend9112", 'nvd3'), chartOutput("trendTable4", 'datatables'), value='9/11'),
        #tabPanel("Custom", showOutput("customChart", 'nvd3'), chartOutput("customTable", 'datatables'), value='custom'),
        tabPanel("Final", htmlOutput('finalText'), showOutput("trendFinal", 'nvd3'), value='Final'),
        id='event')
    ),
    position = "right", fluid=T)
  ))


# shinyUI(pageWithSidebar(
#   headerPanel("IMDB Trends"),
#   conditionalPanel(condition='input.event != "Main"',
#   sidebarPanel(
#       uiOutput('keywordsearch'),
#       htmlOutput('wikipedia')
#   )),
#   mainPanel(
#     progressInit(),
#     tabsetPanel(
#       tabPanel("Main", htmlOutput('mainText'), br(), showOutput("trendMain", 'nvd3'), value='Main'),
#       tabPanel("Great Depression", htmlOutput('GDText'), br(), showOutput("trendGD", 'nvd3'), br(), showOutput("trendGD2", 'nvd3'), chartOutput("trendTable", 'datatables'), value='Great Depression'),
#       tabPanel("World War 2", htmlOutput('WW2Text'), br(), showOutput("trendWW2", 'nvd3'), br(), showOutput("trendWW22", 'nvd3'), chartOutput("trendTable2", 'datatables'), value='World War 2'),
#       tabPanel("Vietnam War", htmlOutput('VIETNAMText'), br(), showOutput("trendVW", 'nvd3'), br(), showOutput("trendVW2", 'nvd3'), chartOutput("trendTable3", 'datatables'), value='Vietnam War'),
#       tabPanel("9/11 Attacks", htmlOutput('SepText'), br(), showOutput("trend911", 'nvd3'), br(), showOutput("trend9112", 'nvd3'), chartOutput("trendTable4", 'datatables'), value='9/11'),
#       #tabPanel("Custom", showOutput("customChart", 'nvd3'), chartOutput("customTable", 'datatables'), value='custom'),
#       tabPanel("Final", htmlOutput('finalText'), showOutput("trendFinal", 'nvd3'), value='Final'),
#       id='event')
#   )
# ))
