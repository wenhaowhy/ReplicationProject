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
data(yearly)
data(secref)

#Determine unique values
#we are dealing with 2315 publicly traded companies between April (1998-04-20)  and October(2007-10-30)
#

#Combine all the data for the years between 1998:2007
x<-rbind(daily.1998,daily.1999,daily.2000,daily.2001,daily.2002,daily.2003,daily.2004,daily.2005,daily.2006,daily.2007)
dim(x)

#There are 3083 unique public trading companies
length(unique(x$symbol))

#Limit number of days to after 1998-10-20 and before 2007-04-30 to allow for (6,6) portfolio
correct_days<-subset(x,v.date>="1998-10-20" & v.date<="2007-04-30")
View(correct_days)


#Merge daily with secref data by symbol to know which companies belong to which industries
#!!!!Question: but the number of unique symbols is different in each set (secref vs correct days)
y <- tbl_df(secref)
z <- merge(correct_days, y, by = "symbol")


#Limit universe to the top 1500 companies on Dec 31st of each year

y<-mutate(correct_days, year= years(v.date))

y %>%
        left_join(y, yearly, by = c("id")) %>%
        filter(top.1500) %>% tbl_df()


z<-tbl_df(correct_days)
View(z)

z %>%
        mutate(z, month = months(v.date)) %>%
        group_by(symbol, month) %>%
        summarize(ret = sum(tret))





head
'''
install.packages("PerformanceAnalytics")
install.packages("xts")
install.packages("zoo")

require(PerformanceAnalytics)
require(zoo)
require(xts)
'''


