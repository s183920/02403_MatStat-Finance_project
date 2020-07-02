# import packages
library("ggplot2")
library("dplyr")
library("lubridate")
library("stringr")

# set working directory and load data
setwd("D:/OneDrive/Dokumenter/DTU/Statistik/finans_project/R-scripts")

finans1 <- read.table("finans1_data.csv", header=TRUE, sep=",", as.is=TRUE)
data <- finans1
data$time <- ymd(data$time) # konvertere tiden fra character til Date objekt

# Basic info
names(data)
summary(data)
str(data)
dim(data)

max(data[, 2:ncol(data)])
min(data[, 2:ncol(data)])

# tidsinfo
min_date <- min(data$time)
max_date <- max(data$time)
interval_weeks <- difftime(max_date, min_date, units = "weeks")
interval_days <- difftime(max_date, min_date, units = "days")
as.data.frame(table(min_date, max_date, interval_days, interval_weeks))

#tjek for manglende eller fejl data
character <- 0
numeric <- 0
dates <- 0
na <- 0
empty <- 0
for (i in data){
  if (any(is.Date(i))){
    dates <- dates+1
  } else if (any(is.numeric(i))) {
    numeric <- numeric + 1
  } else if (any(is.character(i))) {
    character <- character + 1
  } else if (any(is.na(i))) {
    na <- na + 1
  } else if (any(as.character(data) == " ", as.character(data) == "")) {
    empty <- empty + 1
  }
}
print(c(dates, numeric, character, na, empty))


any(is.na(data)) # ingen NA's i finans1
sum(complete.cases(data)) == nrow(data)
any(as.character(data) == " ", as.character(data) == "") # ingen manglende værdier


# dage mellem datapunkter
day_diff <- c()
for (t in c(1:length(data$time)-1)) {
  day_diff[t] <- difftime(data$time[t+1], data$time[t], units = "days")
}
length(data$time) == length(day_diff)+1 # tjek at længden er formindsket med 1

# data frame with time variables
time_var <- data.frame(day_diff)

table(time_var) # tabel over antal dage mellem
 

