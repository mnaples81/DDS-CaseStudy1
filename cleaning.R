## Data cleaning

## Check the data
head(GDP)
str(GDP)
head(EDU)
str(EDU)

## make all variable names lower case
names(GDP) <- tolower(names(GDP))
names(EDU) <- tolower(names(EDU))


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

## merge data sets
merged <- merge(GDP,EDU,by="countrycode")
fix(merged)