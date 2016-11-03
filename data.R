## initialization
setwd("c:/R")
library(plyr)
library(gdata)
library(ggplot2)

## Download data
GDPurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
EDUurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

GDP <- read.csv(GDPurl, skip=3, header=TRUE)
EDU <- read.csv(EDUurl, header=TRUE)

## Check the data
head(GDP)
str(GDP)
head(EDU)
names(GDP)
str(EDU)

## make all variable names lower case
names(GDP) <- tolower(names(GDP))
names(EDU) <- tolower(names(EDU))
names(GDP)
names(EDU)

## rename columns in GDP
colnames(GDP)[1] <- "countrycode"
colnames(GDP)[6] <- "notes"

## drop empty columns from GDP
GDP$x.1 <- NULL
GDP$x.3 <- NULL
GDP$x.4 <- NULL
GDP$x.5 <- NULL
GDP$x.6 <- NULL


#remove non-country entries from GDP
GDP <- GDP[-192:-237,]

##coerce GDP variable to numeric
merged$us.dollars. <- as.numeric(gsub("[^[:digit:]]","", merged$us.dollars.))
class(merged$us.dollars.)

## merge data sets
merged <- merge(GDP,EDU,by="countrycode")
fix(merged)

## count matched IDs 
length(merged$us.dollars.)

## coerce GDP data to numeric, then sort by ascending GDP so that USA is last
merged$us.dollars. <- as.numeric(gsub("[^[:digit:]]","", merged$us.dollars.))
sort.merged.gdp <- merged[order(merged$us.dollars.) , ]


## pull country name with 13th biggest GDP
sort.merged.gdp$economy[13]

## calculate average GDP rankings for the "High income: OECD" 
highincomeOECD <- merged[which(merged$income.group == "High income: OECD"),]
highincomeOECD$ranking <- as.numeric(gsub("[^[:digit:]]","", highincomeOECD$ranking))
mean(highincomeOECD$ranking)

## calculate average GDP rankings for the "High income: OECD" (excluding NA values)
highincome.nonOECD <- merged[which(merged$income.group == "High income: nonOECD"),]
highincome.nonOECD$ranking <- as.numeric(gsub("[^[:digit:]]","", highincome.nonOECD$ranking))
mean(highincome.nonOECD$ranking)

## plot GDP for all countries, with observations colored by income group, remove x-labels
hist(log10(merged$us.dollars.))
ggplot(merged, aes(x=economy, y=log10(us.dollars.))) + geom_point(size=2,
     shape=23, aes(fill=merged$income.group)) + xlab(NULL) + ylab("GDP (Log Transformed)") + labs("Income Group)")+
       theme(axis.text.x=element_blank(),
             axis.ticks.x=element_blank(), legend.title=element_blank()) + ggtitle("All Countries GDP")

## Cut GDP ranking into 5 separate quantiles
sort.merged.gdp$ranking <- as.numeric(gsub("[^[:digit:]]","", sort.merged.gdp$ranking))
fix(sort.merged.gdp)
sort.merged.gdp$quintile <- ifelse(sort.merged.gdp$ranking < 39, 1,
     ifelse(sort.merged.gdp$ranking >= 39 & sort.merged.gdp$ranking <= 76, 2,
     ifelse(sort.merged.gdp$ranking >= 77 & sort.merged.gdp$ranking <= 115, 3,
     ifelse(sort.merged.gdp$ranking >= 116 & sort.merged.gdp$ranking <= 153, 4,
     ifelse(sort.merged.gdp$ranking >= 154 & sort.merged.gdp$ranking <= 190, 5, NA)))))
  
## Make a table for GDP quintile vs Income Group
ggplot(sort.merged.gdp, aes(x=income.group, y=quintile, fill=)) + geom_point(size=2,shape=23)
ggplot(data = sort.merged.gdp, aes(x = quintile, fill = income.group)) + 
  geom_bar()

## How many countries are lower middle income but among the 38 nations with highest GDP?
length(sort.merged.gdp$quintile[which(sort.merged.gdp$quintile == 1 & sort.merged.gdp$income.group == "Lower middle income")])
