#1. Gather the data
gather_data <- function(symbols, years){

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
        #gets rid of 982 lines of code where tret is less than 15
        #filter out only top 1500 companies
        gathered<-filter(gathered,tret<15)
        #gathered<-filter(gathered, top.1500==TRUE)

        #find past and forward 6 months returns to be used later in calculations of
        # MG and JT strategies
        gathered<-gathered %>% group_by(symbol) %>%
                mutate(ret.6.0.m=roll_prod(tret+1, 126,fill=NA,align="right")-1) %>%
                mutate(ret.0.6.m=roll_prod(lead(tret,n=1)+1,126,fill=NA, align="left")-1) %>%
                ungroup()


        # add a test case



        invisible(gathered)
}


#2. Rank the stocks by their 6 months past returns
#get rid of the stocks where returns are NA
gather_daily_JT <- function(){

        x <- gather_data(symbols=secref$symbol,1998:2007)

        ## Now get rid of the rows that we don't consider investible.

        x <- filter(x, ! is.na(ret.0.6.m) & ! is.na(ret.6.0.m))

        ## Create sd.class. Should probably do beta.class here as well.

        daily <- x %>% group_by(date) %>%
                mutate(ret.class = as.character(ntile(ret.6.0.m, n = 3))) %>%
                mutate(ret.class = ifelse(ret.class == "1", "Losers_JT", ret.class)) %>%
                mutate(ret.class = ifelse(ret.class == "3", "Winners_JT", ret.class)) %>%
                mutate(ret.class = factor(ret.class, levels = c("Losers_JT", "2", "Winners_JT"))) %>%
                ungroup()

        #Get rid of the 2 class, do not need it.
        daily<-filter(daily,! is.na(ret.class==2))

        ## There are some outliers, but I am not
        ## sure they matter especially since we only use sd.class in the analysis.

        ## ggplot(data = daily, aes(sd.class, log(sd.252.0.d))) + geom_violin() + facet_wrap(~ year)


        return(daily)
}

daily_returns<-gather_daily_JT()

#3. Gather daily returns into monthly, by selecting the last trading day of the month
gather_monthly <- function(x){
        ## Filter out the last trading day of the month
        monthly <- x %>% group_by(month) %>%
                filter(min_rank(desc(date)) == 1)
        return(monthly)
}

monthly_returns<-gather_monthly(daily_returns)
View(monthly_returns)

monthly_returns<-filter(monthly_returns,top.1500==TRUE)

#4. Use monthly data to find the difference between winners and losers
#Find the difference between the mean returns for Winners and Losers for each month

#Find mean returns for each month for winners and for losers
win_minus_los<-monthly_returns %>%
        group_by(month, ret.class) %>%
        summarize(mean_return=mean(ret.0.6.m))

win_minus_los_final<-win_minus_los %>%
        group_by(month) %>%
        summarize(fin_mean_ret=mean_return[ret.class=="Winners_JT"] - mean_return[ret.class=="Losers_JT"])

View(win_minus_los)
View(win_minus_los_final)

#on average, loser portfolio outperformed winner portfolio by 0.8%
mean(win_minus_los_final$fin_mean_ret)

#WINNERS
winners_mean_ret<-filter(win_minus_los, ret.class=="Winners_JT")
View(winners_mean_ret)
#The mean return of the winner portfolio is 10.9% if top.1500 not filtered out
#The return is 11.4% if top.1500 IS filtered
mean(winners_mean_ret$mean_return)

#Graph a bar plot
winners_mean_ret %>% ggplot(aes(x=month,mean_return)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

#LOSERS
losers_mean_ret<-filter(win_minus_los, ret.class=="Losers_JT")
View(losers_mean_ret)
#The average return on the loser portfolio is 9.7% without top.1500 filter
#The return with top.1500 filter is 12.3%
mean(losers_mean_ret$mean_return)

#Graph losers
losers_mean_ret %>% ggplot(aes(x=month,mean_return)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

#Graph the spread between winner portfolio and loser portfolio
win_minus_los_final %>% ggplot(aes(x=month,fin_mean_ret)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")
#top.1500 filtered: 0.8% return on the spread
mean(win_minus_los_final$fin_mean_ret)

#Make a table with winners and losers
JT.0.6.m.ret<-function(use_monthly_data) {

        r<-use_monthly_data %>%
                spread(key=ret.class,value=ret.0.6.m) %>%
                mutate(diff=Winners_JT-Losers_JT) %>%
                select(symbol, date, Winners_JT, Losers_JT, diff)

        return(r)
}

JT_portfolio<-JT.0.6.m.ret(monthly_returns)

top_1500_data<-function(data){

        filtered<- data %>% filter(data, top.1500==TRUE)
        return(filtered)

        }

monthly_returns<-top_1500_data(monthly_returns)
