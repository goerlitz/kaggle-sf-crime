# DATA PREPARATION

## Date preparation
# PREPARE TIME DATA
train$Date   <- as.Date(train$Dates)
train$Dates  <- as.POSIXct(train$Dates, "PST")
train$Year   <- as.factor(format(train$Dates, "%Y"))
train$Month  <- as.factor(format(train$Dates, "%m"))
train$Hour   <- as.factor(format(train$Dates, "%H"))
train$Minute <- as.factor(format(train$Dates, "%M"))
