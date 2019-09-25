library(rentrez)
library(dplyr)
library(stringr)


## Searchable elements
entrez_db_searchable("pubmed")


## Define my Pubmed queries

q0 = "replication"
q1 = "replication AND transcription"
q2 = "(replication[MeSH]) AND (transcription[MeSH])"
q3 = "(replication[MeSH Major Topic]) AND (skin[MeSH Major Topic])"
q4 = "replication AND transcription AND (clinical trial[Publication Type])"

## Retrieving articles associated to each query
s0 = entrez_search(db="pubmed",term=q0,retmax=50000,use_history=T)
s1 = entrez_search(db="pubmed",term=q1,retmax=10000,use_history=T)
s2 = entrez_search(db="pubmed",term=q2,retmax=10000)
s3 = entrez_search(db="pubmed",term=q3,retmax=10000)
s4 = entrez_search(db="pubmed",term=q4,retmax=10000)


## Number of results returned for each query
s0$count
s1$count
s2$count
s3$count
s4$count

## Extract journal names returned in search s1
summary1 = entrez_summary(db="pubmed",id=s1$ids)
journals1 = extract_from_esummary(summary1,"fulljournalname")
table(journals1)

## Extract publication types returned in search s1
## Pubtype can be "Clinical Trial","Randomized Controlled Trial","Multicenter Study","Review","Meta-Analysis","Evaluation Studies","Comment","Editorial","Letter","News"
pubtypes1 = extract_from_esummary(summ1,"pubtype")
table(pubtypes1)


## Find number of articles returned by a given query published each year

## Function querybyyear() takes as input a query and a year and returns
##   the number of articles returned by that query and published on that given year

querybyyear=function(query, year)
{
  queryyear = paste(query, "AND (", year, "[PDAT])")
  n = entrez_search(db="pubmed", term=queryyear, retmax=0)$count
  return(n)
}


## Call querybyyear() for query q1 for the years between 1960 and 2018

years = seq(1960,2018)
count1 = 0
for(i in 1:length(years))
{
    count1 = c(count1,querybyyear(q1,years[i]))
}
count1 = count1[-1]

## Call querybyyear() for an empty query "" for the years between 1960
##   and 2018, which will return all the articles published on those
##   years and found on Pubmed

countall = 0
for(i in 1:length(years))
{
    countall = c(countall,querybyyear("",years[i]))
}
countall = countall[-1]


## Deduce from count1 and countall the proportion of articles in Pubmed
##   per year associated to query1

decades = seq(1965,2015,by=10)
proportion = count/countall
proportiondecade = 0
for(i in 1:length(decades))
{
    proportiondecade = c(proportiondecade,sum(proportion[years[]>=decades[i]-5 & years[]<decades[i]+5]))
}
proportiondecade = proportiondecade[-1]
proportiondecade[6]=proportiondecade[6]*10/8



