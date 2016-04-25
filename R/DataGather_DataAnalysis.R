
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
#How can we organize this into one function along with adding secref and yearly

gather.data <- function(symbols, years){

        require(ws.data)
        rename()
        gathered <- data.frame()

        for( i in years ){

                file.name <- paste("daily", i, sep = ".")
                data(list = file.name)

                gathered <- rbind(gathered, subset(eval(parse(text=file.name)), symbol %in% symbols))
        }

        gathered <- rename(gathered, date = v.date)

        gathered <- left_join(select(gathered,id,symbol,date,price,tret), select(secref, id, m.ind), by = "id")

        gathered <- left_join(mutate(gathered, year = lubridate::year(date), month = paste(lubridate::month(date, TRUE, TRUE), year, sep = "-")),
                 select(yearly, id, year, top.1500),
                 by = c("year", "id"))

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


"""MG startegy"""

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

"""
#Find monthly returns
#Error: expecting a single value (if use summarize)!
#How to arrange according to the total return?
#What to do with all the NAs!?


monthly.ret <- x %>% group_by(month, symbol) %>% mutate(ret.0.1.m = cumprod(1+tret)-1)
View(head(monthly.ret))


View(head(monthly.ret))
summary(monthly.ret)
"""

