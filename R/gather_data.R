
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
        gethered<-filter(gathered, top.1500==TRUE)

        #find past and forward 6 months returns to be used later in calculations of
        # MG and JT strategies
        gathered<-gathered %>% group_by(symbol) %>%
                mutate(ret.6.0.m=roll_prod(tret+1, 126,fill=NA,align="right")-1) %>%
                mutate(ret.0.6.m=roll_prod(lead(tret,n=1)+1,126,fill=NA, align="left")-1) %>%
                ungroup()


        # add a test case



        invisible(gathered)
}
