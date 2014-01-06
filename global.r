library(data.table)

if (file.exists('imdbdataset.tsv')) {
  imdb <- fread('imdbdataset.tsv')
} else {
  imdb <- readRDS('imdbdataset.rds')
}