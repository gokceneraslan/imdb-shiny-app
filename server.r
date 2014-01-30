## server.r
require(rCharts)
# library(googleVis)
library(plyr)
library(reshape2)
library(data.table)
library(stringr)
library(shinyIncubator)

plot.Genre.Year <- function(df, style, event, ratio=F) {
  if (is.null(event) || is.na(event)) event <- 'Main'
  if ((event == 'Main') || (event == 'Final')) return(nPlot(x~y, data=data.frame(x=NA, y=NA), type = 'stackedAreaChart', id = 'chart'))
  
  df <- copy(df)
  event <- eventname.to.variable(event)
  ngenres <- 8 #get most frequent 8 genres and mark the rest as "Other"
  top.genres <- count(df, 'Genre')
  top.genres <- top.genres[order(top.genres$freq, decreasing=T),][1:ngenres, 1]
  df[, Genre:=ifelse(Genre %in% top.genres, Genre, 'Other')]
  
  if (ratio) {
    df[, GenreRatio:=round(sum(get(event))/length(Category), 5), by=list(Year, Genre)] #ratio of genre over all movies
  } else {
    df[get(event)==T, GenreRatio:=length(Category), by=list(Year, Genre)] #count of the movies
  }
  
  df <- na.omit(unique(df[, list(Year, Genre, GenreRatio)]))
  
  genre.counts <- melt(dcast(df, ...~Genre, fill=0), id='Year', value.name='GenreRatio', variable.name='Genre') #include counts of ALL genres to make nPlot happy
  
  if (ratio) {
    genre.plot <- nPlot(GenreRatio ~ Year, group = 'Genre', data = genre.counts, type = 'lineWithFocusChart', id = 'chart')
    genre.plot$yAxis(axisLabel = 'Percent of the related movies')
    genre.plot$chart(margin = list(left = 100))
  } else {
    genre.plot <- nPlot(GenreRatio ~ Year, group = 'Genre', data = genre.counts, type = 'stackedAreaChart', id = 'chart')
    #genre.plot$chart(useInteractiveGuideline=TRUE)
    
    genre.plot$chart(showControls=T)
    genre.plot$yAxis(axisLabel = 'Total count of the related movies')
    genre.plot$chart(style=style, margin = list(left = 100))    
  }
  
  return(genre.plot)
}

plot.Main.Chart <- function(df) {
  df <- copy(df)
  ngenres <- 8 #get most frequent 8 genres and mark the rest as "Other"
  
  top.genres <- count(df, 'Genre')
  
  top.genres <- top.genres[order(top.genres$freq, decreasing=T),][1:ngenres, 1]
  df[, Genre:=ifelse(Genre %in% top.genres, Genre, 'Other')]
  
  df[, GenreRatio:=length(Language), by=list(Year, Genre)]
  df <- unique(na.omit(df[,list(Year, Genre, GenreRatio)]))
  
  genre.counts <- melt(dcast(df, ...~Genre, fill=0), id='Year', value.name='GenreRatio', variable.name='Genre') #include counts of ALL genres to make nPlot happy
  
  genre.plot <- nPlot(GenreRatio ~ Year, group = 'Genre', data = genre.counts, type = 'stackedAreaChart', id = 'chart')    
  genre.plot$chart(showControls=T)
  genre.plot$chart(style="expand")
  
  return(genre.plot) 
}

plot.Final.Chart <- function(df) {
  df <- copy(df)
  
  df <- melt(df, measure=c('WW2', 'SEP11','VIETNAM', 'DEP'), variable.name='Event', value.name='EventValue')
  df[, EventCount:=sum(EventValue), by=list(Year, Event)]
  df <- unique(na.omit(df[,list(Year, Event, EventCount)]))
  
  genre.counts <- data.table(melt(dcast(df, ...~Event, fill=0), id='Year', value.name='EventCount', variable.name='Event')) #include counts of ALL genres to make nPlot happy
  
  variable.to.eventname <- function(var) {
    switch(as.character(var), 
           'WW2' = 'World War 2',
           'DEP' = 'Great Depression',
           'VIETNAM' = 'Vietnam War',
           'SEP11' = '9/11 Attacks') 
  }
  genre.counts[, Event2:=variable.to.eventname(Event), by=list(Year, Event)]
  
  #genre.plot <- nPlot(EventCount ~ Year, group = 'Event2', data = genre.counts, type = 'stackedAreaChart', id = 'chart')    
  genre.plot <- nPlot(EventCount ~ Year, group = 'Event2', data = genre.counts, type = 'lineWithFocusChart', id = 'chart')    

  genre.plot$yAxis(axisLabel = 'Total count of the related movies')
  genre.plot$chart(margin = list(left = 100))    
  
  #genre.plot$chart(showControls=T)
  #genre.plot$chart(style="expand")
  
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

eventname.to.date <- function(event) {
  if (is.null(event) || is.na(event)) return(1800)
  switch(as.character(event), 
         'World War 2' = 1937,
         'Great Depression' = 1929,
         'Vietnam War' = 1956,
         '9/11' = 2001,
         #'custom' = 'custom',
         'Main' = 1800,
         'Final'= 1920)
}

eventname.to.variable <- function(event) {
  if (is.null(event) || is.na(event)) return('main')
  switch(as.character(event), 
         'World War 2' = 'WW2',
         'Great Depression' = 'DEP',
         'Vietnam War' = 'VIETNAM',
         '9/11' = 'SEP11',
         #'custom' = 'custom',
         'Main' = 'main')
}

shinyServer(function(input, output, session) {
  
  df <- reactive({
    
#     var <- eventname.to.variable(input$event)
    
#     if (var != 'main') {
#        return(imdb[imdb[[var]] == T]) 
#     }
#     else {
      year <- eventname.to.date(input$event)
      return(imdb[Year >= year])
#     }
#     } else if (var == 'custom') {
#       input$search
#       if (is.null(input[['keywords']]) || (str_trim(input$keywords) == '')) {
#         return(data.frame())
#       }
#       else {
#         regex <- keywords.to.regex(input$keywords)
#         print(regex)
#         system.time(imdb[, USER:=grepl(regex, Plot, ignore.case=T, perl=T), by=Key])
#         print(regex)
#         return(imdb[imdb$USER == T][1:1000])  #limit results by first 1000 movies
#       }
#     }
})

  generate.table <- function() {
    event <- eventname.to.variable(input$event)
    if (is.null(event) || is.na(event)) event <- 'main'
    if ((event == 'main') || (event == 'final')) return(dTable(data.frame(x='Loading')))
    
    withProgress(session, min=1, max=15, expr={
      
      setProgress(message = 'Calculation in progress',
                  detail = 'This may take a while...', value=0)
      
    
      gt <- df()
      gt <- gt[(get(event) == T) & (!is.na(Title))]
      gt <- gt[order(NormRating, decreasing=T)] #sort movies by normalized ratings
      if (nrow(gt) > 1000) gt <- gt[1:1000]

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

  generate.plot <- function(id, style='stacked', ...) {
    withProgress(session, min=1, max=15, expr={
      
      setProgress(message = 'Calculation in progress',
                  detail = 'This may take a while...', value=0)
      dd <- df()
      
      if (nrow(dd) == 0) {
        return(NULL)
      }
      n1 <<- plot.Genre.Year(dd, style, input$event, ...)
      n1$set(dom=id)
      
      #     setProgress(message = 'Calculation in progress',
      #                 detail = 'This may take a while...',
      #                 value=15)
      return(n1)
      
    }) 
  }

  output$trendGD <- renderChart({
    generate.plot(id='trendGD', ratio=T)
  })

  output$trendGD2 <- renderChart({
    generate.plot(id='trendGD2')
  })

  output$trendWW2 <- renderChart({
    generate.plot(id='trendWW2', ratio=T)
  })
  output$trendWW22 <- renderChart({
    generate.plot(id='trendWW22')
  })

  output$trendVW <- renderChart({
    generate.plot(id='trendVW', ratio=T)
  })
  output$trendVW2 <- renderChart({
    generate.plot(id='trendVW2')
  })

  output$trend911 <- renderChart({
    generate.plot(id='trend911', ratio=T)
  })
  output$trend9112 <- renderChart({
    generate.plot(id='trend9112')
  })

  output$customChart <- renderChart({
    print(input$search)
    if (!is.null(input[['keywords']]) && (str_trim(input$keywords) != '')) {
      print('RENDERING CUSTOM chart')
       generate.plot(id='customChart')
    }
    else {
      print('RENDERING dummy chart...')
      nPlot(x~y,data = data.frame(x=c(1), y=c(1)), type = 'stackedAreaChart', id = 'chart')    
    }
  })


  output$trendMain <- renderChart({

    withProgress(session, min=1, max=15, expr={
      
      setProgress(message = 'Calculation in progress',
                  detail = 'This may take a while...', value=0)
      dd <- df()
      
      if (nrow(dd) == 0) {
        return(NULL)
      }
      n1 <<- plot.Main.Chart(dd)

      n1$set(dom='trendMain')
      
      #     setProgress(message = 'Calculation in progress',
      #                 detail = 'This may take a while...',
      #                 value=15)
      return(n1)
    })
  })

output$trendFinal <- renderChart({
  
  withProgress(session, min=1, max=15, expr={
    
    setProgress(message = 'Calculation in progress',
                detail = 'This may take a while...', value=0)
    dd <- df()
    
    if (nrow(dd) == 0) {
      return(NULL)
    }
    n1 <<- plot.Final.Chart(dd)
    
    n1$set(dom='trendFinal')
    
    #     setProgress(message = 'Calculation in progress',
    #                 detail = 'This may take a while...',
    #                 value=15)
    return(n1)
  })
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

  output$customTable <- renderChart2({
    print(input$search)
    if (!is.null(input[['keywords']]) && (str_trim(input$keywords) != '')) {
      print('RENDERING CUSTOM TABLE')
       generate.table()
    }
    else {
      print('RENDERING dummy table...')
      dTable(data.frame())
    }
  })

  output$wikipedia <- renderUI({
      URI <- switch(input$event, 
                    'World War 2' = 'http://en.m.wikipedia.org/wiki/World_war_2',
                    'Great Depression' = 'http://en.m.wikipedia.org/wiki/Great_Depression',
                    'Vietnam War' = 'http://en.m.wikipedia.org/wiki/Vietnam_war',
                    '9/11' = 'http://en.m.wikipedia.org/wiki/September_11_attacks')

    if (input$event != 'custom') {
      tags$iframe(src=URI, width=400, height=1100, tags$p('Your browser does not support iframes.'))
    }
    else {
      input$search
      if (!is.null(input[['keywords']]) && (str_trim(input$keywords) != '')) {
        wpkeywords <- keywords.to.wpsearch(input$keywords)
      }
      else {
        wpkeywords <- ''
      }
      URI <- paste0('http://en.m.wikipedia.org/w/index.php?search=', wpkeywords)
      tags$iframe(src=URI, width=600, height=1500, tags$p('Your browser does not support iframes.'))
    }

  })

  output$mainText <- renderUI({
    "Cinema has evolved over time in parallel with the evolution of the technology and the evolution of the social norms. For instance, short films, which were the only type produced in 1880s, are not very popular in today's world and adult films could be produced only after 1960s."
  })

  output$GDText <- renderUI({
    "Important events in the history has reflections in all forms of art including cinema. The Great Depression (1929-1939) is one of these incidents about which a lot of films have been produced so far. Even though the number of such films increases remarkably after 2000s with the development of the technology, the proportion of the number of these films to the total number of produced films is higher in the years right after The Great Depression."
  })
  output$VIETNAMText <- renderUI({
    "Vietnam war (1956-1975) is another historical event which has been the subject of many films. Again the percent of the films related with Vietnam war has a peak in the years right after the end of the war."
  })
  output$WW2Text <- renderUI({
    "The same pattern holds for the World War 2(1939-1945) which happaned after The Great Depression. The total number of films produced about second world war has increased substantially after 2000s but the percent of such films were much bigger during the years following the war."
  })
  output$SepText <- renderUI({
    "Finally, 9/11 attacks which happened in September 11, 2001, has been mentioned directly or indirectly especially in many Hollywood films."
  })
  output$finalText <- renderUI({
    "Among these four historically important happenings, World War 2 is the one which has had the most influence over the produced films."
  })

  output$keywordsearch <- renderUI({
    if (input$event == 'custom') {
      return(div(textInput("keywords", h4("Search for keywords:"), value = ""), br(),
       actionButton("search", 'Search'), br(), br()))
    }
  })

})
