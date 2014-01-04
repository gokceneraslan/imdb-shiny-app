library(rCharts)
library(data.table) #must be the development version v1.8.11 (to use dcast.data.table)
library(plyr)       #must be the latest master branch from github (to fix a segfault error: issue #131)
library(reshape2)


if (!file.exists('imdbdataset.rds.xz')) {
  
  #TSV files preprocessed with Google Refine
  movies <- fread('imdb-export-Movies.tsv') #much faster than read.table
  setkey(movies, 'Key')
  movies <- unique(movies)
  
  genres <- fread('imdb-export-Genres.tsv')
  setkey(genres, 'Key')
  genres <- unique(genres)
  
  languages <- fread('imdb-export-Languages.tsv')
  setkey(languages, 'Key')
  languages <- unique(languages)
  
  ratings   <- fread('imdb-export-Ratings.tsv') #generated by dump_ratings.py file
  setkey(ratings, 'Key')
  ratings <- unique(ratings)
  
  plots     <- fread('imdb-export-Plots.tsv')
  setkey(plots, 'Key')
  plots <- unique(plots, by='Key')
  
#   ngenres <- 6
#   top.genres <- count(genres, 'Genre')
#   top.genres <- top.genres[order(top.genres$freq, decreasing=T),][1:ngenres, 1]
#   genres[, Genre:=ifelse(Genre %in% top.genres, Genre, 'Other Genre')] #label 20% of data as Other
#   
#   nlang <- 6
#   top.langs <- count(languages, 'Language')
#   top.langs <- top.langs[order(top.langs$freq, decreasing=T),][1:nlang, 1]
#   languages[, Language:=ifelse(Language %in% top.langs, Language, 'Other Language')] #label 20% of data as Other
  
  #genres    <- dcast.data.table(genres, ...~Genre, fun=length)
  #languages <- dcast.data.table(languages, ...~Language, fun=length)
  
  imdb <- merge(movies, genres, by='Key', all=T)
  imdb <- merge(imdb, languages, by='Key', all=T)
  imdb <- merge(imdb, ratings, by='Key', all=T)
  imdb <- merge(imdb, plots, by='Key', all=T)
  
  #rating normalization
  rating.constant = 1000
  imdb[, NormRating:=(VoteCount/(VoteCount+rating.constant)) * Rating]
  
  rm(movies, genres, languages, ratings, plots)
  
  #regex + PCRE ROCKSSS!!!
  ww2.regex <- '\\b(Second World War|WW(II|2)|World War (2|II)|Nazi|Hitler|concetration camp|Auschwitz|Stalingrad|Pearl Harbor|1939-1945|Normandy landing|D-day)\\b' # "|" for OR operation, \\b is for word boundries
  sep11.regex <- '\\b(9/11|September 11|Bin Laden)\\b'
  depression.regex <- '\\b(Great Depression|Stock market crash of 1929|Black Tuesday)\\b'
  vietnam.regex <- '\\b(Vietnam War|Second Indochina War)\\b'
  
  imdb[, WW2:=grepl(ww2.regex, Plot, ignore.case=T, perl=T), by=Key]
  imdb[, SEP11:=grepl(sep11.regex, Plot, ignore.case=T, perl=T), by=Key]
  imdb[, DEP:=grepl(depression.regex, Plot, ignore.case=T, perl=T), by=Key]
  imdb[, VIETNAM:=grepl(vietnam.regex, Plot, ignore.case=T, perl=T), by=Key]
  
  saveRDS(imdb, file='imdbdataset.rds.xz', compress='xz')
  
} else {
 imdb <- readRDS('imdbdataset.rds.xz') 
}

plot.Genre.Year <- function(df) {
  genre.counts <- na.omit(ddply(df, c('Year', 'Genre'), summarise, Count=length(Genre)))
  genre.counts <- melt(dcast(genre.counts, ...~Genre, fill=0), id='Year', value.name='Count', variable.name='Genre') #include counts of ALL genres to make nPlot happy
  
  genre.plot <- nPlot(Count ~ Year, group =  'Genre', data = genre.counts, type = 'stackedAreaChart', id = 'chart')
  genre.plot$chart(useInteractiveGuideline=TRUE)
  print(genre.plot)
  return(genre.plot)
}

plot.Genre.Year(imdb)

# imdb2    <- dcast(imdb, ...~Genre, value.var='Genre', fun=length)
# imdb    <- dcast.data.table(imdb, ...~Language)
# imdb[,'NA'] <- NULL
# imdb[,'NA.1'] <- NULL
# imdb <- imdb[order(imdb$NormRating, decreasing=T),]
# imdb <- imdb[1:500, ]
# plot(gvisBubbleChart(imdb, 'Key', 'Year', 'NormRating', 'Rating', 'NormRating'))

plot.Genre.Year(imdb[imdb$WW2 == T])
plot.Genre.Year(imdb[imdb$VIETNAM == T])