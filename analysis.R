## data analysis

## count matched IDs 
length(merged$us.dollars.)

## coerce GDP data to numeric, then sort by ascending GDP so that USA is last
merged$us.dollars. <- as.numeric(gsub("[^[:digit:]]","", merged$us.dollars.))
sort.merged.gdp <- merged[order(merged$us.dollars.) , ]

## pull country name with 13th biggest GDP
sort.merged.gdp$economy[13]

## calculate average GDP rankings for the "High income: OECD" 
## create subset of data for high income: OECD countries only
highincomeOECD <- merged[which(merged$income.group == "High income: OECD"),]
## Run calculations on subsetted data
highincomeOECD$ranking <- as.numeric(gsub("[^[:digit:]]","", highincomeOECD$ranking))
mean(highincomeOECD$ranking)

## calculate average GDP rankings for the "High income: nonOECD"
## create subset of data for high income: nonOECD countries only
highincome.nonOECD <- merged[which(merged$income.group == "High income: nonOECD"),]
## Run calculations on subsetted data
highincome.nonOECD$ranking <- as.numeric(gsub("[^[:digit:]]","", highincome.nonOECD$ranking))
mean(highincome.nonOECD$ranking)

## plot GDP for all countries, with observations colored by income group, remove x-labels
hist(log10(merged$us.dollars.))
ggplot(merged, aes(x=economy, y=log10(us.dollars.))) + geom_point(size=2,
                                                                  shape=23, aes(fill=merged$income.group)) + xlab(NULL) + ylab("GDP (Log Transformed)") + labs("Income Group)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=element_blank()) + ggtitle("All Countries GDP")

## Cut GDP ranking into 5 separate quantiles by using nested ifelse statements to create a new column in the dataframe
sort.merged.gdp$ranking <- as.numeric(gsub("[^[:digit:]]","", sort.merged.gdp$ranking))
sort.merged.gdp$quintile <- ifelse(sort.merged.gdp$ranking < 39, 1,
                                   ifelse(sort.merged.gdp$ranking >= 39 & sort.merged.gdp$ranking <= 76, 2,
                                          ifelse(sort.merged.gdp$ranking >= 77 & sort.merged.gdp$ranking <= 115, 3,
                                                 ifelse(sort.merged.gdp$ranking >= 116 & sort.merged.gdp$ranking <= 153, 4,
                                                        ifelse(sort.merged.gdp$ranking >= 154 & sort.merged.gdp$ranking <= 190, 5, NA)))))

## Make a table for GDP quintile vs Income Group
ggplot(data = sort.merged.gdp, aes(x = quintile, fill = income.group)) + 
  geom_bar()

## How many countries are lower middle income but among the 38 nations with highest GDP?
length(sort.merged.gdp$quintile[which(sort.merged.gdp$quintile == 1 & sort.merged.gdp$income.group == "Lower middle income")])
