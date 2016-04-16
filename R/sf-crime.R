library(ggplot2)
library(lattice)
library(scales)
library(data.table)
library(corrplot)

# LOAD DATA
setwd("~/kaggle/sf-crime/")
train <- fread("data/train.csv", header = TRUE)

# find duplicate entries
table(duplicated(train))
dup <- train[duplicated(train) | duplicated(train, fromLast = TRUE),]
sort(table(as.character(dup$Dates)))
train[1130:1135,] # show example

source("R/data-prep.R")

# remove incomplete year 2015
train <- subset(train, Year != "2015")
train$Year <- droplevels(train$Year)

# DATA CLEANING

# find invalid entries
sub <- subset(train, Y >= 40)
sub$Address <- droplevels(sub$Address)
sort(table(sub$Address)) # result is all junctions
train[train$Address == "LARKIN ST / AUSTIN ST",]

# remove observations with invalid coordinates
train <- subset(train, Y < 40)

# dates with too few counts
#dateCounts[dateCounts$Freq <100, ]

# empty dates
dateRange <- range(train$Date)
seq(dateRange[1], dateRange[2], by = "days")
length(unique(train$Date))

# process street addresses
train$Junction <- as.factor(regexpr(".* / .*", train$Address) == 1)
street <- function(s) { x <- unlist(strsplit(s, " of ")); if(length(x) == 2) x[2] else "JUNCTION"}
blocks <- function(s) { x <- unlist(strsplit(s, " Block ")); if(length(x) == 2) x[1] else "JUNCTION"}
train$Street <- factor(unlist(lapply(as.character(train$Address), street)))
train$Block <- factor(unlist(lapply(as.character(train$Address), blocks)))


# top crime and city hot spot
topCrime <- sort(table(train$Category), decreasing = TRUE)[1]
hotSpot <- names(sort(table(train$Address), decreasing = TRUE))[1]

for (i in c("Category", "DayOfWeek", "PdDistrict", "Address")) {
  train[[i]] <- as.factor(train[[i]])
}

# correlation
df <- subset(train, select = c(Category, DayOfWeek, PdDistrict, Address, Date, Year, Month, Hour, Minute, Junction, Street, Block))
df[] <- lapply(df,as.integer)
cor <- cor(df)
corrplot(cor)


# get an overview
dim(train)
summary(train)
summary(train$Category)

tab = table(train$Category)
summary(train$DayOfWeek)

# -----------------
# GRAPHS
# -----------------

# show coordinates with district highlighting
bayview <- subset(train, train$PdDistrict == "BAYVIEW")
with(bayview, plot(X, Y, col = rgb(0,0,0,0.1), pch=19))
qplot(X, Y, data = train, color = PdDistrict, size=1) + scale_size(range = c(0, 2)) + theme_light()
ggplot(train, aes(X, Y, color = PdDistrict)) + geom_point(size=1.5, alpha=0.1) + theme_light()
with(train, smoothScatter(X, Y))

xyplot(Y ~ X | PdDistrict, data = train, groups = PdDistrict) # plot each district separately colored by district
xyplot(Y ~ X | Category, data = train, groups = PdDistrict) # plot each category separately colored by district
histogram(~ Dates | Category, data = train) # each category in histogram

# analyze crime categories
crimeCat <- table(train$Category)
sortedCat <- sort(crimeCat, decreasing = TRUE)
par(las=2) # make label text perpendicular to axis
par(mar=c(15,4,4,2)) # increase x-axis margin.
barplot(table(factor(train$Category, levels = names(sortedCat))))

# show per date
ggplot(data = train, aes(X, Y, color = Category)) + geom_point(size = 1) + theme_light()

vtheft <- subset(train, train$Category == "VEHICLE THEFT")
qplot(vtheft$PdDistrict, vtheft$DayOfWeek)

qplot(train$Category) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + coord_cartesian(ylim = c(0,200000)) + coord_flip()
qplot(train$Category) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + scale_x_discrete(limits=rev(names(sortedCat))) + coord_flip()
qplot(train$Category, fill=train$Category) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + scale_x_discrete(limits=rev(names(sortedCat))) + coord_flip()
qplot(train$Category, fill=..count..) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + scale_x_discrete(limits=rev(names(sortedCat))) + coord_flip()

# analyse VEHICLE THEFT
barplot(table(as.Date(train$Dates[train$Category == "VEHICLE THEFT"])))
#barplot(table(train[train$Category == "VEHICLE THEFT",]$Year))
