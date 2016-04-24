
install.packages("RcppRoll")

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

gather.data <- function(symbols, years){

        require(ws.data)
        gathered <- data.frame()

        for( i in years ){

                file.name <- paste("daily", i, sep = ".")
                data(list = file.name)

                gathered <- rbind(gathered, subset(eval(parse(text=file.name)), symbol %in% symbols))
        }
        invisible(gathered)
}

"""Example of how gather.data works
A <- daily.1998
B <- daily.1999
C <- daily.2000
X <- c("A", "B", "C")
parse(text=X[1])
ex_daily1998<-eval(parse(text=X[1]))
head(ex_daily1998)
"""

#Use grabbed function to open data
x<-gather.data(symbols=secref$symbol,1998:2007)
save(x, file="gather.Rdata")
f<-file.choose()
load(f)

summary(x)
View(head(x))

#use dplyr package to make the data frame tbl_df to avoid printing a lot of data on the screen
x<-tbl_df(x)
y<-tbl_df(yearly)

#make 2 addisional columns: a year and a month-year column
x <- mutate(x, year = lubridate::year(v.date),
               month = paste(lubridate::month(v.date, TRUE, TRUE), year, sep = "-"))

#check whether the 2 columns are present
View(head(x))

#merge daily data with yearly by year and id
x <- left_join(select(x, v.date, month, year, symbol, id, tret, price, price.unadj,year),
               select(y, id, year, top.1500),
               by = c("year", "id")) %>%
               filter(top.1500) %>% tbl_df()

View(head(x))

#merge with secref to get industry data and arrange by date and symbol
x <- left_join(x, select(secref, id, name, m.sec, m.ind), by = "id") %>%
              arrange(v.date, symbol)
View(head(x,100))



#Find monthly returns
#Error: expecting a single value (if use summarize)!
#How to arrange according to the total return?
#What to do with all the NAs!?


monthly.ret <- x %>% group_by(month, symbol) %>% mutate(ret.0.1.m = cumprod(1+tret)-1)
View(head(monthly.ret))


View(head(monthly.ret))
summary(monthly.ret)


"""MG startegy"""

#Rank the industries using MG function
#Take care of NAs
MG.Data <- function(data.x) {
    data.x %>%
        group_by(m.ind) %>%
        arrange(m.ind, v.date) %>%
        mutate(ind_ret = roll_mean(tret)) %>%
        filter(row_number(v.date)==n()) %>%
        ungroup() %>%
        mutate(ind_rank = row_number(desc(ind_ret))) %>%
        select(m.ind, ind_ret, ind_rank) %>%
        arrange(ind_rank)
}

View(MG.Data)

#Function for (6,6) strategy

data.6.6 <- function(yyyy, mm, pro, data) {

        date1 <- ceiling_date((ymd(paste(yyyy,mm,days_in_month(mm),sep="-")) %m+%
                                       months(-pro)), "month") - duration(1, "days")
        date2 <- (ymd(paste(yyyy,mm,01,sep="-")) %m+% months(-pro-5)) #past 6 months

        date1 <- as.Date(date1)
        date2 <- as.Date(date2)

        data <- filter(data, v.date<= date1, v.date>= date2)

        invisible(data)
}

#Return for MG
#30% of top industries --> 21 industries


Top.1500 <- function(yyyy,mm,pro,data) {
        avail <- NULL
        yeartime <- year(ymd(paste(yyyy,mm,01,sep="-")) %m+% months(-pro))

        avail <- filter(yearly, year==yeartime, top.1500)

        stock_list <- unique(avail$id)

        data <- filter(data, id %in% stock_list, tret<2, tret>-2)

        data <- mutate(data, month = month(v.date), year = year(v.date))

        invisible(data)
}



MG.6.6.ret <- function(yyyy, mm, pro, data) {
        data1 <- Top.1500(yyyy, mm, pro, data)
        data2 <- data.6.6(yyyy, mm, pro, data1)
        data_now <- filter(data1,
                           month==mm,
                           year==yyyy)
        rank <- MG.Data(data2)

        winner_list <- filter(rank, ind_rank<=21)
        loser_list <- filter(rank, ind_rank>=48)
        data_now  %>%
                select(-top.1500, -id, -year) %>%
                filter(m.ind %in% winner_list) %>%
                group_by(symbol) %>%
                arrange(symbol, v.date) %>%
                mutate(cum_ret = cumprod(1+tret) - 1) %>%
                filter(row_number(v.date) == n()) -> return1
        mean(return1$cum_ret) -> result1
        data_now  %>%
                select(-top.1500, -id, -year) %>%
                filter(m.ind %in% loser_list) %>%
                group_by(symbol) %>%
                arrange(symbol, v.date) %>%
                mutate(cum_ret = cumprod(1+tret) - 1) %>%
                filter(row_number(v.date) == n()) -> return2
        mean(return2$cum_ret) -> result2
        result <- c(result1, result2)
        invisible(result)
}


MG.Year.ret <- function(yyyy) {
        data <- x
        month_tab <- NULL
        return_table <- NULL
        for (i in 1:12) {
                mm <- i
                for (pro in 1:6) {
                        a <- MG.6.6.ret(yyyy,mm,pro,data)
                        month_tab <- rbind(month_tab, a)
                }
                return_table <- rbind(return_table,
                                      c(mean(month_tab[,1]),mean(month_tab[,2])))
                month_tab <- NULL
        }
        return(return_table)
}

f<-MG.Year.ret(1999)
View(head(f,100))

Run <- function() {
        year_ret <- NULL
        total_ret <- NULL
        for (yy in 2000:2007) {
                year_ret <- MG.Year.ret(yy)
                total_ret <- rbind(total_ret, year_ret)
        }
        return(total_ret)
}

r<-Run()

monthly.ret.new <- monthly.ret %>% group_by(year, symbol) %>% summarize(ret.0.12.m = cumprod(ret.0.1.m+1)-1)

six_month_data <- mutate(x, six_month = paste(lubridate::month(v.date, TRUE, TRUE)))

d<-filter(monthly.ret,symbol=="MSFT")
View(d)
arrange(d, month)


#Find 6 months past return
x <- x %>% group_by(symbol) %>%
        mutate(six_month_ret = roll_mean(ret.0.1.m, 6, fill = NA))
View(tail(x,300))








"""
#Finding the 52 week high
x <- x %>% group_by(symbol) %>%
        mutate(yearly52high = max(price.unadj, 250, fill = NA))
View(head(x))

highest_price <- x %>% group_by(month) %>%
        filter(min_rank(desc(v.date)) == 1) %>%
        filter(top.1500) %>%
        mutate(sd.class = ntile(yearly52high, n = 5))

monthly.price <- mutate(highest_price,
                     month.plus.1 = paste(lubridate::month(v.date + 10, TRUE, TRUE),
                                          lubridate::year(v.date + 10), sep = "-"))
summary(monthly.price)

all <- left_join(select(monthly.price, - month), monthly.ret,
                 by = c("symbol", c("month.plus.1" = "month"))) %>% ungroup()

all <-  filter(all, ! is.na(yearly52high))  %>%
        group_by(month.plus.1) %>%
        select(v.date, month.plus.1, sd.class, ret.0.1.m, top.1500) %>%
        ungroup() %>% ## Needed to do the arrange correctly for entire data.
        arrange(v.date)
summary(all)
"""

