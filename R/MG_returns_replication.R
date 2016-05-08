#MG returns replication

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

#2. Gather daily
#Rank the stocks by the past industry returns

gather_daily_MG<-<- function(){

        x <- gather_data()

        #Find industry returns by finding the mean of the returns of all the stocks in each industry
        x<-x %>% group_by(m.ind) %>%
                arrange(m.ind, date)  %>%
                mutate(ind_ret = mean(ret.6.0.m), na.rm=TRUE) %>%

                #Get rid of NAs values
                x <- filter(x, top.1500 & ! is.na(ind_ret))

        ## Create ind.class
        daily <- x %>% group_by(date) %>%
                mutate(ret.class = as.character(ntile(ind_ret, n = 3))) %>%
                mutate(ret.class = ifelse(ind.class == "1", "Losers_MG", ind.class)) %>%
                mutate(ret.class = ifelse(ind.class == "3", "Winners_MG", ind.class)) %>%
                mutate(ret.class = factor(ind.class, levels = c("Losers_MG", "2", "Winners_MG"))) %>%
                ungroup()

        ## ggplot(data = daily, aes(sd.class, log(sd.252.0.d))) + geom_violin() + facet_wrap(~ year)


        return(daily)
}

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
