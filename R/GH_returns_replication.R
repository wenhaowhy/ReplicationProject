#Find 52 week high ratios
#Arrange by symbol and date first
#How to arrange the code so that we have the last

#1. Find the 52 week high ratio
gather_daily_GH <- function(data) {
        data %>%
                group_by(symbol) %>%
                arrange(symbol, date) %>%
                mutate(highest = roll_max(price, 252, fill = NA, align = "right")) %>%
                ungroup() %>%
                mutate(wh_52_ratio = price/highest) %>%
                filter(!is.na(wh_52_ratio)) ->r

        invisible(r)
}


ratios_daily_GH<-gather_daily_GH(x)

#2. Rank the ratios.

GH_rank<- function(data) {

        data %>% group_by(date) %>%
        mutate(wh.52.class = as.character(ntile(wh_52_ratio, n = 3))) %>%
        mutate(wh.52.class= ifelse(wh.52.class == "1", "Losers_GH", wh.52.class)) %>%
        mutate(wh.52.class = ifelse(wh.52.class == "3", "Winners_GH", wh.52.class)) %>%
        mutate(wh.52.class = factor(wh.52.class, levels = c("Losers_GH", "2", "Winners_GH"))) %>%
        ungroup()->r

        invisible(r)
        }

daily_ranks_GH<-GH_rank(ratios_daily_GH)

#3. Gather daily returns into monthly, by selecting the last trading day of the month

gather_monthly <- function(x){
        ## Filter out the last trading day of the month
        monthly <- x %>% group_by(month) %>%
                filter(min_rank(desc(date)) == 1)
        return(monthly)
}

monthly_returns_GH<-gather_monthly(daily_ranks_GH)
View(monthly_returns_GH)
summary(monthly_returns)

#4 Form a portfolio of winners and losers.
#Calculate forward 6 months returns for each stock for the last tading day of each month

monthly_returns %>% group_by(symbol, )

GH.0.6.m.ret<- function(monthly_data){

     r<-monthly_data %>%
        spread(key=wh.52.class,value=ret.0.6.m) %>%
        mutate(diff=Winners_GH-Losers_GH) %>%
        select(date, Winners_GH, Losers_GH, diff)

     return(r)
}

GHreturns<-GH.0.6.m.ret(monthly_returns_GH)
View(GHreturns)


win_minus_los<-monthly_returns_GH %>%
        na.omit() %>%
        group_by(month,wh.52.class) %>%
        summarize(mean_return=mean(ret.0.6.m))
View(win_minus_los)

r<-win_minus_los %>%
        spread(key=wh.52.class,value=mean_return) %>%
        mutate(diff=Winners_GH-Losers_GH) %>%
        select(month, Winners_GH, Losers_GH, diff)

#Mean of Winners returns over the years
mean(r$Winners_GH)

#Mean of Losers returns over the years
mean(r$Losers_GH)

#Mean of the difference in returns over the years
mean(r$diff)

View(r)

#Graph the returns

#Graph the SPREAD
r %>% ggplot(aes(x=month,diff)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

#Graph the WINNERS
r %>% ggplot(aes(x=month,Winners_GH)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

#Graphs the LOSERS
r %>% ggplot(aes(x=month,Losers_GH)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

