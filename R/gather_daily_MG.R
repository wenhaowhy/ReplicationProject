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
