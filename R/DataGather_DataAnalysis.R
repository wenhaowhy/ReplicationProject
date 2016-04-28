

library(ws.data)
library(lubridate)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(RcppRoll)

data(secref)
data(yearly)


#gather the data
#How can we organize this into one function along with adding secref and yearly
#Get rid of extremely high prices
gather.data <- function(symbols, years){

        require(ws.data)

        gathered <- data.frame()

        #open up all the daily data files for years
        for( i in years ){

                file.name <- paste("daily", i, sep = ".")
                data(list = file.name) #create a data list of daily files

                gathered <- rbind(gathered, subset(eval(parse(text=file.name)), symbol %in% symbols))
        }

        gathered <- rename(gathered, date = v.date)

        #join daily data with secref that contains industry returns
        gathered <- left_join(select(gathered,id,symbol,date,price,tret), select(secref, id, m.ind), by = "id")

        #1) create a new column for Mon-Year variable and 2)attach existing data to yearly
        gathered <- left_join(mutate(gathered, year = lubridate::year(date), month = paste(lubridate::month(date, TRUE, TRUE), year, sep = "-")),
                 select(yearly, id, year, top.1500),
                 by = c("year", "id"))

        #make gathered data a tbl_df for opening it
        gathered<-tbl_df(gathered)
        invisible(gathered)
}



#Open and save the data
x<-gather.data(symbols=secref$symbol,1998:2007)
View(head(x,100))
save(x, file="gather.Rdata")
f<-file.choose()

#Check whether data makes sense
#Notice: max(tret) is 499 (no sense), think how to deal with this
summary(x)
View(head(x))



#MG startegy
#Rank the industry returns using MG function
#Take care of NAs

MG.Data <- function(data.x) {
    data.x %>%
        group_by(m.ind) %>%
        arrange(m.ind, date) %>% #group and arrange by industry and date within each industry
        mutate(ind_ret = roll_mean(tret)) %>% #find industry returns by finding the rolling mean of all the stocks in each industry
        ungroup() %>%
        mutate(ind_rank = row_number(desc(ind_ret))) %>% #create the column called insutry return
        select(m.ind, ind_ret, ind_rank) %>% #select the columns to spit out for R
        arrange(ind_rank) #arrange by industry rank
}


View(MG.Data)

#Function for (6,6) strategy
#At the beginning of each month (t) stocks are ranked in ascending order according to their industry past performance
#winner:stocks in the top 30% of industries
#loosers:stocks in the bottom 30% of industries


data.6.0 <- function(data.x) {

        #filter by last day of the month and allow only unique dates
        anchors<-unique(data.x %>% group_by(month) %>%
                                filter(min_rank(desc(date)) == 1) %>%
                                filter(top.1500))

        anchors<-as.Date(data.x$date)

        #inititate the loop to calculate 125 days before for each date
        for (i in 1:length(anchors)){
                anchor<-anchors[i]

                date1<-anchor-125 #for each day substract 125 days (~6 months)
                date1<-as.Date(date1)

                date2<-anchor+0
                date2<-as.Date(date2)
        }

        data.x <- filter(data.x$date, date>= date1, date<= date2) #find the 6 months period

        invisible(data.x)
}


#Return for MG
#30% of top industries --> 21 industries
#30% of bottom industries --> 21 industries

MG.6.0.ret <- function(data.x) {

        data1 <- data.6.0(data.x)

        #rank the date according to the previous 6 months of performance
        rank <- MG.Data(data1)

        #filter out 21 best performing industry(30% out of 69 industries)
        winners <- filter(rank, ind_rank<=21)
        #filter out 21 worst performing industry
        losers <- filter(rank, ind_rank>=48)

        data1  %>%
                select(-top.1500, -id, -year) %>%
                filter(m.ind %in% winners) %>% #take out all the winner industries
                group_by(symbol) %>%
                arrange(symbol, date) %>% #group and arrange by symbol and date
                mutate(cum_ret = cumprod(1+tret) - 1) -> winner.ret #find cumilative return

        mean(winner.ret$cum_ret) -> winner_portfolio #find the mean of the returns for all the stocks in each winner industry

        #repeat for looser portfolio
        data1  %>%
                select(-top.1500, -id, -year) %>%
                filter(m.ind %in% losers) %>%
                group_by(symbol) %>%
                arrange(symbol, date) %>%
                mutate(cum_ret = cumprod(1+tret) - 1) -> looser.ret

        mean(looser.ret2$cum_ret) -> looser_portfolio

        #give out a table of winner and looser portfolios
        result <- c(winner_portfolio, looser_portfolio)
        invisible(result)
}




##This is a function that calculates the 52 week high prices and
##dates of every date in the dataset.
calc.52wkh<- function(x) {

        ##find list of unique end-of-month dates (jan-dec) from 1998 to 2007
        initial.date <- unique(monthly.sd$date)
} ##creates loop that works from the first row of the dates to the last of the dates (#length of vector)
##of the dataframe.
for(i in 1:length(initial.date)) {

        n <- initial.date[i]

        ##to create a 52 week period, an initial start date and end date is constructed.
        ##start date is 365 days from picked date, and the end date is the picked date.
        ##should I use end of month date or just regular date?
        start.date <- n-365
        end.date <- n

        ##subset dates from x and make sure that start date is always less than initial date
        ##and that initial date is the same or earlier than end date
        x.dates<-filter(x, start.date<x$date & end.date<= end.date)

        ##subset price from dataset.
        prices <- x["price"]

        ##combine price by symbol, order by max price per symbol.
        y<-tapply(price, x$symbol, max)

        ##organize dates by decreasing order.
        dates <- sort(x$date, decreasing = TRUE)

        ##allows loop to start at 1 and end at i, which equals the
        ##length of the total dates in the dataset.
        ##combines price and date.
        y$date<-dates[1]

        total <- leftjoin(prices, dates)

        invisible(total)
}
}

52wkratio <- function(x) {
        ##this function calculates the 52 week ratio of (Pi, t-1) / (high i, t-1)
        ##
        52wkhighs <- calc.52wkh(x)
        ##combine 52wk high with price symbol and date of normal dataset.
        combined = leftjoin(52wkhighs, x$price, x$symbol, x$date)

        combined$ratio = combined$price / combined$52wkhighs

}




##Create portfolio of 1/3 winners, 1/3 losers.
##We can label the winners and losers based on the 52 week ratio.
##develop an operation that ranks stocks based on highest to lowest 52 week ratio.
##Label the first 1/3 of stocks in the rankings as "winnder" and the bottom 1/3 of stocks
##as "losers".
##Then we can long the winners and short the losers for a period of 6 months.
##how to long winnders and short losers in this senario?


#Find 52 week high
#Arrange by symbol and date first
#How to arrange the code so that we have the last



x <- x %>% arrange(symbol, date)  %>% group_by(symbol) %>% mutate(price_52_wh = roll_max(price, 252, fill = NA, align = "right"))

#Check whether 52-week high prices make sense
#Get rid of
summary(x)

#Find the last trading day of the month
monthly <- x %>% group_by(month) %>%
        filter(min_rank(desc(date)) == 1)

View(monthly)

#Check the code
filter(x,symbol=="IBM",date>"2005-01-01" & date<"2007-12-01") %>% ggplot() + geom_point(aes(date,price)) + geom_point(aes(date, price_52_wh), color = "red")

d<-filter(x,symbol=="IBM", price==119.60, date>"2005-01-01" & date<"2007-12-01")
View(d)

#Compare 52 week high calculations to the Bloomberg data
stopifnot(
        round(subset(x, date == "2006-01-03" & symbol == "IBM", select = "price_52_wh"), 5) == ,
        round(subset(x, date == "2007-08-06" & symbol == "GOOG", select = "price_52_wh"), 5) ==
)


#FInd the ratio and arrange in descneding order
#Arrange the ratios in descenig order for each month
monthly %>% group_by(date) %>%
      mutate(ratio_52_wh=price/price_52_wh) %>%
      arrange(desc(ratio_52_wh)) ->m

#Check whether the size of the ratio makes sense (Max is 1 = good)
View(m)
summary(m)


#Form the portfolios


#Find monthly returns

#Error: expecting a single value (if use summarize)!
#How to arrange according to the total return?
#What to do with all the NAs!?


monthly.ret <- x %>% group_by(month, symbol) %>% mutate(ret.0.1.m = cumprod(1+tret)-1)
View(head(monthly.ret))


View(head(monthly.ret))
summary(monthly.ret)
"""

