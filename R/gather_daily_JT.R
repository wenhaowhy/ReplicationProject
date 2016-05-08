#rank the stocks by their 6 months past returns
#get rid of the stocks where returns are NA
gather_daily_JT <- function(){

        x <- gather_data(symbols=secref$symbol,1998:2007)

        ## Now get rid of the rows that we don't consider investible.

        x <- filter(x, top.1500 & ! is.na(ret.0.6.m) & ! is.na(ret.6.0.m))

        ## Rank based on the past 6 month performance

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
