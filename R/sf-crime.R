library(ggplot2)

# LOAD DATA
setwd("..")
train <- read.csv("data/train.csv", header = TRUE)

# PREPARE TIME DATA
train$Date   <- as.Date(train$Dates)
train$Dates  <- as.POSIXlt(train$Dates, "PST")
train$Year   <- as.factor(format(train$Dates, "%Y"))
train$Month  <- as.factor(format(train$Dates, "%m"))
train$Hour   <- as.factor(format(train$Dates, "%H"))
train$Minute <- as.factor(format(train$Dates, "%M"))

# remove incomplete year 2015
train <- subset(train, Year != "2015")
train$Year <- droplevels(train$Year)

# remove observations with invalid coordinates
train <- subset(train, Y < 40)

# process street addresses
train$Junction <- as.factor(regexpr(".* / .*", train$Address) == 1)
street <- function(s) { x <- unlist(strsplit(s, " of ")); if(length(x) == 2) x[2] else "JUNCTION"}
blocks <- function(s) { x <- unlist(strsplit(s, " Block ")); if(length(x) == 2) x[1] else "JUNCTION"}
train$Street <- factor(unlist(lapply(as.character(train$Address), street)))
train$Block <- factor(unlist(lapply(as.character(train$Address), blocks)))


# top crime and city hot spot
topCrime <- sort(table(train$Category), decreasing = TRUE)[1]
hotSpot <- names(sort(table(train$Address), decreasing = TRUE))[1]

# get an overview
dim(train)
summary(train)
summary(train$Category)

tab = table(train$Category)
x <- as.numeric(train$Category)
summary(x)
summary(train$DayOfWeek)

#length(object) # number of elements or components
#str(object)    # structure of an object
#class(object)  # class or type of an object
#names(object)  # names

# analyze crime categories
crimeCat <- table(train$Category)
sortedCat <- sort(crimeCat, decreasing = TRUE)
par(las=2) # make label text perpendicular to axis
par(mar=c(15,4,4,2)) # increase x-axis margin.
barplot(table(factor(train$Category, levels = names(sortedCat))))

qplot(train$Category) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + coord_cartesian(ylim = c(0,200000)) + coord_flip()
qplot(train$Category) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + scale_x_discrete(limits=rev(names(sortedCat))) + coord_flip()
qplot(train$Category, fill=train$Category) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + scale_x_discrete(limits=rev(names(sortedCat))) + coord_flip()
qplot(train$Category, fill=..count..) + theme(axis.ticks = element_blank(), panel.grid.major.y = element_blank()) + scale_x_discrete(limits=rev(names(sortedCat))) + coord_flip()

# analyse VEHICLE THEFT
barplot(table(as.Date(train$Dates[train$Category == "VEHICLE THEFT"])))
#barplot(table(train[train$Category == "VEHICLE THEFT",]$Year))

