#Determine unique values
#we are dealing with 2315 publicly traded companies between April (1998-04-20)  and October(2007-10-30)
length(unique(x$symbol))

#write function to read these

library(ws.data)
library(lubridate)
library(dplyr)
library(tibble)
library(tidyr)
library(plyr)
library(ggplot2)

data(daily.1998)
data(daily.1999)
data(daily.2000)
data(daily.2001)
data(daily.2002)
data(daily.2003)
data(daily.2004)
data(daily.2005)
data(daily.2006)
data(daily.2007)

x<-rbind(daily.1998,daily.1999,daily.2000,daily.2001,daily.2002,daily.2003,daily.2004,daily.2005,daily.2006,daily.2007)
dim(x)

head(r,500)

eom <- function(d){

        ## Takes a vector of dates and figures out all of last days of the
        ## month correspoding to each date. Allows us to figure out all
        ## the dates that we need from an input dataframe instead of
        ## calculating them ourselves.

        stopifnot(is(d, "Date"))

        ## Trick is to take the date, change it to the first of that
        ## month, and then add 40 days. The resulting date is guaranteed
        ## to be in the next month. Then create the first day of that next
        ## month. Then subtract 1 day. That day is guaranteed to be the
        ## last day of the month you started in. Hacky but effective.

        next.month   <- as.Date(paste(format(d, "%Y-%m"), "01", sep =
                                              "-"))          + 40
        end.of.month <- as.Date(paste(format(next.month, "%Y-%m"), "01",
                                      sep = "-")) - 1

        invisible(end.of.month)
}

eom(x)


head
'''
install.packages("PerformanceAnalytics")
install.packages("xts")
install.packages("zoo")

require(PerformanceAnalytics)
require(zoo)
require(xts)
'''

x<-rbind()

