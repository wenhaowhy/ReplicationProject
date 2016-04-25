gather_data <- function(){

        ## Function for gathering data. First, data() the required inputs.

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

        ## Merge in a couple of steps for clarity.

        x <- bind_rows(daily.1998, daily.1999, daily.2000, daily.2001,
                       daily.2002, daily.2003, daily.2004, daily.2005,
                       daily.2006, daily.2007)


        x <- rename(x, date = v.date)

        ## Need year for the merge with yearly data frame and month (like Jan-2005) to
        ## calculate monthly returns.

        x <- mutate(x, year = lubridate::year(date),
                    month = paste(lubridate::month(date, TRUE, TRUE), year, sep = "-"))

        x <- left_join(x, select(yearly, -symbol),
                       by = c("year", "id"))

        x <- left_join(x, select(secref, -symbol), by = "id")

        x <- select(x, symbol, name, date, tret, top.1500, month) %>% arrange(date, symbol)

        return(x)
}


## Gathering data takes long enough to be annoying. Is there someway to just do
## this once?

x <- gather_data()

## Now that we have the key data, there are two main tasks. First, we need to
## know the trailing one year volatility for each stock on the last day of the
## month. Second, we need to know each stock's monthly return.

summary(x)

## Ought to do some error checking to see if this is really doing what I want.

x <- x %>% group_by(symbol) %>%
        mutate(sd.252 = roll_sdr(tret, 250, fill = NA))

View(tail(x,300))


## Maybe show some graphics?


## We really only need this data for the last day of the month, so, we can
## filter out the rest as here. Keep in mind that this is the last trade day of
## the month, not always the last day.

## We should probably take better care to do our universe trimming early in the
## process, probably in gather_data. It is hacky to have filters like top.1500
## floating around in this document.

monthly.sd <- x %>% group_by(month) %>%
        filter(min_rank(desc(date)) == 1) %>%
        filter(top.1500) %>%
        mutate(sd.class = ntile(sd.252, n = 5))

View(tail(monthly.sd,100))

## Need to examine this data. Deal with outliers and NA.

summary(monthly.sd)

monthly.ret <- x %>% group_by(month, symbol) %>% mutate(ret.0.1.m = cumprod(1+tret)-1)
View(head(monthly.ret))
