#get rid of this!!!!
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
#Get rid of extremely high prices
gather.data <- function(symbols, years){

        require(ws.data)

        gathered <- data.frame()

        #open up all the daily data files for all years
        for( i in years ){

                file.name <- paste("daily", i, sep = ".")
                data(list = file.name) #create a data list of daily files

        #combine all the daily data in a data frame
                gathered <- rbind(gathered, subset(eval(parse(text=file.name)), symbol %in% symbols))
        }

        #rename v.date to date, makes more sense
        gathered <- rename(gathered, date = v.date)

        #join daily data with secref that contains industry returns
        gathered <- left_join(select(gathered,id,symbol,date,price,tret), select(secref, id, m.ind), by = "id")

        #1) create a new column for year and mon-year variable and 2)attach existing data to yearly
        gathered <- left_join(mutate(gathered, year = lubridate::year(date), month = paste(lubridate::month(date, TRUE, TRUE), year, sep = "-")),
                 select(yearly, id, year, top.1500),
                 by = c("year", "id"))

        #make gathered data a tbl_df
        gathered<-tbl_df(gathered)

        #clean data: get rid of stocks with high returns
        #decide how to deal with 1) high prices (Berkshire) and 2) high returns
        #gathered<-filter(gathered, ! symbol %in% c ())
        #get rid of CHTM - was around for 4 months

        #find past and forward 6 months returns to be used later in calculations of
        # MG and JT strategies
        gathered<-gathered %>% group_by(symbol) %>%
               mutate(ret.6.0.m=roll_prod(tret+1, 126,fill=NA,align="right")-1) %>%
               mutate(ret.0.6.m=roll_prod(lead(tret,n=1)+1,126,fill=NA, align="left")-1) %>%
               ungroup()

        invisible(gathered)
}



#Open and save the data
x<-gather.data(symbols=secref$symbol,1998:2007)
View(head(x,1000))
summary(x)

#Cleaning
filter(x,symbol=="3STTCE", date>"1998-12-12" & date<"2007-01-01") %>% ggplot() + geom_point(aes(date,tret))
m<-filter(x,symbol=="3STTCE")
View(m)
d<-filter(x,tret>15)
View(d)




############ JT strategy
#Find last day of the month returns
#create portfolio
#3 winners and 1 loosers

monthly_data<-function(x){

        monthly <-x %>% group_by(month) %>%
        filter(min_rank(desc(date)) == 1 & ! is.na(ret.6.0.m)) %>%
        filter(top.1500) %>%
        mutate(ret.class=ntile(ret.6.0.m,n=3)) %>%

        return(monthly)
}
View(monthly.data)


#filter dates to 6 months after the first dates became available, thus showing complete past 6-month returns.
#c<-filter(monthly.ret,date>"1998-06-01")
#View(c)

#create a portfolio
#winners' class is 3
#loosers' class is 1
winners<- filter(monthly.ret,ret.class==3)
loosers<-filter(monthly.ret,ret.class==1)

#Find future returns of the monthly portfolios
winners.0.6.m <- winners %>%
        arrange(date) %>%
        mutate(w.ret.0.6.m = roll_mean(ret.0.6.m, 6, align="right"))
losers.0.6.m <- loosers %>%
        arrange(date) %>%
        mutate(l.ret.0.6.m = roll_mean(ret.0.6.m, 6, align="right"))

#Bind the 2 tables together and find the difference between w.ret.0.6.m and l.ret.0.6.m, find this for each month
#Do this for every 6 months



###########MG returns

#Find industry returns and divide them into 3 classes depending on the industry returns

MG<-function(data){
        data %>%
        select(-top.1500, -year, -id, -symbol) %>%
        group_by(m.ind) %>%
        arrange(m.ind, date)  %>%
        mutate(ind_ret = (tret), na.rm=TRUE) %>%
        filter(row_number(date)==n()) %>%
        ungroup() %>%
        mutate(ind_rank = row_number(ind_ret)) %>%
        select(m.ind, ind_ret, ind_rank)
}

MG.ind.ret<-MG(monthly.ret)

#Find the top 30% and the bottom 30% of the industires
#Form a new portfolio every month, hold for 6 months

max(MG.ind.ret$ind_rank)
winners_MG<-filter(MG.ind.ret,ind_rank<=23)
losers_MG<-filter(MG.ind.ret,ind_rank>=46)

#Calculate stock returns for the forward six months for winner/loser industries





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

#################################################
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


#Find the ratio and arrange in descneding order
#Arrange the ratios in descenig order for each month
monthly %>% group_by(date) %>%
      mutate(ratio_52_wh=price/price_52_wh) %>%
      arrange(desc(ratio_52_wh)) %>%
      mutate(rank_52_wh = row_number(desc(ratio_52_wh))) %>%
      arrange(rank_52_wh)->m

#Alternative to ranking. What's better?
monthly %>% group_by(date) %>%
        mutate(ratio_52_wh=price/price_52_wh) %>%
        arrange(desc(ratio_52_wh)) %>%
        mutate(wh_class=ntile(ratio_52_wh, n=3))->instead
View(instead)


#Get rid of NAs first
#Remove all the rows where 52-week high can't be calculated (= NA)
m<-m[complete.cases(m),]

#Check whether the size of the ratio makes sense (Max is 1 = good)
View(m)
summary(m)

#Form the portfolios
#Notice: that the # of companies changes each month how to find the top 30% for EACH month. This function just calculates that
#top 30% is ranks below 711.
winners<-m %>%
        group_by(month) %>%
        filter(rank_52_wh<0.333*max(m$rank_52_wh))
View(winners)

loosers<-m %>%
        group_by(month) %>%
        filter(rank_52_wh>0.666*max(m$rank_52_wh))
View(loosers)

#Form the portfolios
#How to get head(30% and tail 30% of the rows in each month?




###########################################################
#Find monthly returns

#Error: expecting a single value (if use summarize)!
#How to arrange according to the total return?
#What to do with all the NAs!?


monthly.ret <- x %>% group_by(month, symbol) %>% mutate(ret.0.1.m = cumprod(1+tret)-1)
View(head(monthly.ret))


View(head(monthly.ret))
summary(monthly.ret)
"""

