# Basic data analysis

# load data
setwd("..")
train <- read.csv("data/train.csv", header = TRUE)

# get an overview
summary(train)
summary(train$Category)

tab = table(train$Category)
x <- as.numeric(train$Category)
summary(x)
summary(train$DayOfWeek)

# analyze crime categories
crimeCat <- table(train$Category)
sortedCat <- sort(crimeCat, decreasing = TRUE)

par(las=2) # make label text perpendicular to axis
par(mar=c(15,4,4,2)) # increase x-axis margin.
barplot(table(factor(train$Category, levels = names(sortedCat))))

# analyze crime dates
crimeDate <- as.Date(train$Dates)
crimeYears <- as.numeric(format(crimeDate, "%Y"))
crimeMonths <- as.numeric(format(crimeDate, "%m"))
crimeDays <- as.numeric(format(crimeDate, "%d"))
trainDates <- data.frame(crimeYears, crimeMonths, crimeDays)
          