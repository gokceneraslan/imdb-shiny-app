## server.r
require(rCharts)
library(googleVis)
library(plyr)
library(reshape2)
library(data.table)
library(stringr)
library(shinyIncubator)

plot.Genre.Year <- function(df) {
  df <- copy(df)
  ngenres <- 7 #get most frequent 7 genres and mark the rest as "Other"
  top.genres <- count(df, 'Genre')
  top.genres <- top.genres[order(top.genres$freq, decreasing=T),][1:ngenres, 1]
  df[, Genre:=ifelse(Genre %in% top.genres, Genre, 'Other')]
  
  genre.counts <- na.omit(ddply(df, c('Year', 'Genre'), summarise, Count=length(Genre)))
  genre.counts <- melt(dcast(genre.counts, ...~Genre, fill=0), id='Year', value.name='Count', variable.name='Genre') #include counts of ALL genres to make nPlot happy
  
  genre.plot <- nPlot(Count ~ Year, group =  'Genre', data = genre.counts, type = 'stackedAreaChart', id = 'chart')
#   genre.plot <- nPlot(Count ~ Year, group =  'Genre', data = genre.counts, type = 'lineWithFocusChart', id = 'chart')

  #genre.plot$chart(useInteractiveGuideline=TRUE)
  #print(genre.plot)

   genre.plot$chart(showControls=T)

  return(genre.plot)
}

keywords.to.regex <- function (keywords) {
  #first split comma separated keywords and trim whitespace
  keywords <- str_trim(strsplit(keywords, ',')[[1]])
  
  #then transform keywords into REGEX terms combined with AND operator:
  #so 'apple,orange' becomes '(?=.*\\bapple\\b)(?=.*\\borange\\b)'
  #simply, '\\b': word boundries, '(?=.*)': lookahead regex to achieve AND operation
  
  regex <- paste(sapply(keywords, function(x){paste0('(?=.*\\b', x, '\\b)')}), collapse='')
}

keywords.to.wpsearch <- function (keywords) {
  #first split comma separated keywords and trim whitespace
  keywords <- str_trim(strsplit(keywords, ',')[[1]])
  paste(sapply(keywords, function(x){paste0('"', x, '"')}), collapse=' OR ')
}

shinyServer(function(input, output, session) {
  
  df <- reactive({
    
    var <- switch(input$event, 
                  'World War 2' = 'WW2',
                  'Great Depression' = 'DEP',
                  'Vietnam War' = 'VIETNAM',
                  '9/11' = 'SEP11')                     
#     if (str_trim(input$keywords) == '') {
       return(imdb[imdb[[var]] == T][1:1000]) 
#     }
#     else {
#       regex <- keywords.to.regex(input$keywords)
#       print(regex)
#       system.time(imdb[, USER:=grepl(regex, Plot, ignore.case=T, perl=T), by=Key])
#       print(regex)
#       return(imdb[imdb$USER == T][1:1000])  #limit results by first 1000 movies
#     }
  })

  generate.table <- function() {
    withProgress(session, min=1, max=15, expr={
      
      setProgress(message = 'Calculation in progress',
                  detail = 'This may take a while...', value=0)
      
      gt <- df()
      gt <- gt[!is.na(gt$Title)] #remove entries without titles
      gt <- gt[order(gt$NormRating, decreasing=T)] #sort movies by normalized ratings
#       print(str(gt))
      gt <- gt[, list(Title, Year, Genre, Category, Language, NormRating,Plot)] #only include meaningful col.s
      callback = "#! 
        function (nRow) {
              $('td', nRow).each(function (index) {
                  var maxChars = 80;
                      var unfilteredText = $(this).text();
                      if (unfilteredText.length > maxChars && maxChars > 3) {
                          $(this).attr('title', unfilteredText);
                          $(this).html(unfilteredText.substring(0, maxChars-4) + '...');
                      }
              });
              return nRow;
          } !#"
      
      gt <- dTable(gt, aaSorting = list(c(5, "desc")), sPaginationType="bootstrap",
                   fnRowCallback=callback)
      
      setProgress(message = 'Calculation in progress',
                  detail = 'This may take a while...',
                  value=15)
      
      return(gt)
    })
    
  }

  generate.plot <- function(id) {
    withProgress(session, min=1, max=15, expr={
      
      setProgress(message = 'Calculation in progress',
                  detail = 'This may take a while...', value=0)
      dd <- df()
      
      if (nrow(dd) == 0) {
        return(NULL)
      }
      n1 <<- plot.Genre.Year(dd)
      n1$set(dom=id)
      
      #     setProgress(message = 'Calculation in progress',
      #                 detail = 'This may take a while...',
      #                 value=15)
      return(n1)
      
    }) 
  }

  output$trendGD <- renderChart({
    generate.plot(id='trendGD')
  })

  output$trendWW2 <- renderChart({
    generate.plot(id='trendWW2')
  })
  
  output$trendVW <- renderChart({
    generate.plot(id='trendVW')
  })
  
  output$trend911 <- renderChart({
    generate.plot(id='trend911')
  })

  output$trendTable <- renderChart2({
    generate.table()
  })
  
  output$trendTable2 <- renderChart2({
    generate.table()
  })
  
  output$trendTable3 <- renderChart2({
    generate.table()
  })
  
  output$trendTable4 <- renderChart2({
    generate.table()
  })

  output$wikipedia <- renderUI({
      URI <- switch(input$event, 
                    'World War 2' = 'http://en.m.wikipedia.org/wiki/World_war_2',
                    'Great Depression' = 'http://en.m.wikipedia.org/wiki/Great_Depression',
                    'Vietnam War' = 'http://en.m.wikipedia.org/wiki/Vietnam_war',
                    '9/11' = 'http://en.m.wikipedia.org/wiki/September_11_attacks')
#     }
#     else {
#       wpkeywords <- keywords.to.wpsearch(input$keywords)
#       URI <- paste0('http://en.m.wikipedia.org/w/index.php?search=', wpkeywords)
#     }
    tags$iframe(src=URI, width=400, height=1100, tags$p('Your browser does not support iframes.'))
  })
})
