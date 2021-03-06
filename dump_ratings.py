import csv
import re

ratingsFile = 'ratings.list.tsv' #generated by github.com/dedeler/imdb-data-parser
moviesFile = 'imdb-export-Movies.csv'
outputFile = 'imdb-export-Ratings.csv'

r = csv.reader(open(ratingsFile), delimiter='\t')

ratings = {}

for row in r:
    dist = row[0]
    votecount = long(row[1])
    rating = float(row[2])
    name = row[3].strip()

    if not re.search(' \(\d\d\d\d\)$', name):
        continue

    year = int(name[-5:-1])
    name = name[:-6].strip()

    ratings[(name, year)] = (dist, votecount, rating)

r = csv.reader(open(moviesFile), delimiter="\t")
next(r, None)

with open(outputFile, 'w') as output:
    outputCSV = csv.writer(output, delimiter='\t', quoting=csv.QUOTE_NONNUMERIC)
    outputCSV.writerow(['Key', 'VoteDist', 'VoteCount', 'Rating'])

    for row in r:
        key = row[0]
        name = row[1]
        subtitle = row[2]
        year = row[3]
        if year == 'NA':
            continue
        year = int(year)
        cat = row[4]

        ratingRec = ratings.get((name, year), None)

        if ratingRec:
            #print "Name: '%s' Year: '%s'" % (name, year)
            outputCSV.writerow([key, ratingRec[0], ratingRec[1], ratingRec[2]])
