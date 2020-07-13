# Loading the dataset
advertising <- read.csv("C:\\Users\\HP\\Downloads\\advertising.csv", header = TRUE)
head(advertising)

# Previewing the dataset

View(advertising)
str(advertising)
dim(advertising)
class(advertising)

# Data Cleaning
# Looking for null values

is.na(advertising)

# Finding out the number of missing values in each column

colSums(is.na(advertising))

# Checking the total number of missing values

sum(is.na(advertising))

# Screening for outliers on the numerical columns using boxplots

boxplot(advertising$Area.Income)
boxplot(advertising$Daily.Time.Spent.on.Site)
boxplot(advertising$Age)
boxplot(advertising$Daily.Internet.Usage)

# To check for outliers in the whole dataset

boxplot(advertising)
# We get the summary for the area income column

summary(advertising$Area.Income)

# We are going to deal with the outliers by winsorizing them
# Setting the benchmark

bench <- 47032 - 1.5 * IQR(advertising$Area.Income) 
bench

# Winsorizing
advertising$Area.Income <- advertising$Area.Income
advertising$Area.Income[advertising$Area.Income< bench]
advertising$Area.Income[advertising$Area.Income < bench]<- bench

summary(advertising$Area.Income)

boxplot(advertising$Area.Income)

# To get dulicated rows

duplicated <- advertising(duplicated(advertising),)
duplicated

# To remove duplicates if any

new_adv = unique(advertising)
new_adv

dim(new_adv)
# The entries are still the same, meaning our dataset has no duplicates.

# UNIVARIATE ANALYSIS
summary(advertising)

# From the summary we get a few summary statistics, further univariate statistical analysis are as below
# We first filter the numerical variables only as follows

install.packages("tidyverse")
library(tidyverse)

numerical_adv <- advertising %>% select(1, 2, 3, 4, 7, 10)
head(numerical_adv)

View(numerical_adv)
str(numerical_adv)
# We get the mode of the variables as follows

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Daily.Time.Spent.on.Site.mode <- getmode(numerical_adv$Daily.Time.Spent.on.Site.mode)
Daily.Time.Spent.on.Site.mode

Age.mode <- getmode(numerical_adv$Age.mode)
Age.mode

Area.Income.mode <- getmode(numerical_adv$Area.Income.mode)
Area.Income.mode

Daily.Internet.Usage.mode <- getmode(numerical_adv$Daily.Internet.Usage.mode)
Daily.Internet.Usage.mode

Male.mode <- getmode(numerical_adv$Male.mode)
Male.mode

Clicked.on.Ad.mode <- getmode(numerical_adv$Clicked.on.Ad.mode)
Clicked.on.Ad.mode
# Our variables do not have any modal values

# We then get the kurtosis of the different variables

install.packages("e1071")
library(e1071)

Daily.Time.Spent.on.Site.kurtosis <- numerical_adv$Daily.Time.Spent.on.Site
kurtosis(Daily.Time.Spent.on.Site.kurtosis)

Age.kurtosis <- numerical_adv$Age
kurtosis(Age.kurtosis)

Area.Income.kurtosis <- numerical_adv$Area.Income
kurtosis(Area.Income.kurtosis)

Daily.Internet.Usage.kurtosis <- numerical_adv$Daily.Internet.Usage
kurtosis(Daily.Internet.Usage.kurtosis)

Male.kurtosis <- numerical_adv$Male
kurtosis(Male.kurtosis)

Clicked.on.Ad.kurtosis <- numerical_adv$Clicked.on.Ad
kurtosis(Clicked.on.Ad.kurtosis)

# We then get the skewness of the variables

Daily.Time.Spent.on.Site.skewness <- numerical_adv$Daily.Time.Spent.on.Site
skewness(Daily.Time.Spent.on.Site.skewness)

Age.skewness<- numerical_adv$Age
skewness(Age.skewness)

Area.Income.skewness <- numerical_adv$Area.Income
skewness(Area.Income.skewness)

Daily.Internet.Usage.skewness <- numerical_adv$Daily.Internet.Usage
skewness(Daily.Internet.Usage.skewness)

Male.skewness <- numerical_adv$Male
skewness(Male.skewness)

Clicked.on.Ad.skewness <- numerical_adv$Clicked.on.Ad
skewness(Clicked.on.Ad.skewness)

# Graphical analysis
# Barplots 

age_frequency <- table(advertising$Age)
barplot(age_frequency)

male_freq <- table(advertising$Male)
barplot(male_freq)

click_freq <- table(advertising$Clicked.on.Ad)
barplot(click_freq)

# Histograms

hist(advertising$Area.Income)

hist(advertising$Daily.Time.Spent.on.Site)

hist(advertising$Daily.Internet.Usage)

# BIVARIATE ANALYSIS
# Encode the categorical variables to be numerical so we can check for correlation
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}
table(advertising[["Ad.Topic.Line"]], encode_ordinal(advertising[["Ad.Topic.Line"]]), useNA = "ifany")
table(advertising[["City"]], encode_ordinal(advertising[["City"]]), useNA = "ifany")
table(advertising[["Country"]], encode_ordinal(advertising[["Country"]]), useNA = "ifany")
table(advertising[["Timestamp"]], encode_ordinal(advertising[["Timestamp"]]), useNA = "ifany")
new_advert <- advertising
new_advert[["Ad.Topic.Line_encoded"]] <- encode_ordinal(advertising[["Ad.Topic.Line"]])
new_advert[["City_encoded"]] <- encode_ordinal(advertising[["City"]])
new_advert[["Country_encoded"]] <- encode_ordinal(advertising[["Country"]])
new_advert[["Timestamp_encoded"]] <- encode_ordinal(advertising[["Timestamp"]])
head(new_advert)

# Drop the categorical columns
new_advert1 = subset(new_advert, select = -c(Ad.Topic.Line, City, Country, Timestamp) )
head(new_advert1)

# Confirm the datatypes of the encoded columns
sapply(new_advert1, class)

# To get the covariance between all the variables

cov(new_advert1, y=new_advert1, use="all.obs")

# To get the correlation matrix between the variables

install.packages("GGally")
library(ggplot2)
ggcorr(new_advert1, label = TRUE, label_alpha = 4)

# We then do a scatter plot to further show the relationship between the variables
plot(new_advert1)