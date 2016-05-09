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


ratio_daily_GH<-gather_daily_GH(x)

#2. Rank the ratios into 3 groups

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

#4. Form a portfolio of winners and losers.
#Calculate forward 6 months returns for each stock for the last trading day of each month

#Summarize the mean returns for each month for each wh.52.class group
win_minus_los<-monthly_returns_GH %>%
        na.omit() %>%
        group_by(month,wh.52.class) %>%
        summarize(mean_return=mean(ret.0.6.m))
View(win_minus_los)


portfolio_returns_GH<-win_minus_los %>%
        spread(key=wh.52.class,value=mean_return) %>%
        mutate(diff=Winners_GH-Losers_GH) %>%
        select(month, Winners_GH, Losers_GH, diff)
View(portfolio_returns_GH)

#Mean of Winners returns over the years
mean(portfolio_returns_GH$Winners_GH)

#Mean of Losers returns over the years
mean(portfolio_returns_GH$Losers_GH)

#Mean of the difference in returns over the years
mean(portfolio_returns_GH$diff)



#Graph the returns in a bar plot

#Graph the SPREAD
r %>% ggplot(aes(x=month,diff)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

#Graph the WINNERS
r %>% ggplot(aes(x=month,Winners_GH)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

#Graphs the LOSERS
r %>% ggplot(aes(x=month,Losers_GH)) + scale_y_continuous(limits = c(-1,1))+geom_bar(stat="identity")

############################
##Apply some filters#######

#Write filter top.1500 companies function

filter_top1500<-function(data) {

        data<-filter(data, top.1500==TRUE)

        return(data)
}

filtered<-filter_top1500(monthly_returns_GH)

#Comapre the dim before the filter and after the filter
#The number of rows decreased to about 150K from 225K
dim(monthly_returns_GH)
dim(filtered)

#Use filtered data to calculate the returns for the portfolio again

win_minus_los<-filtered %>%
        na.omit() %>%
        group_by(month,wh.52.class) %>%
        summarize(mean_return=mean(ret.0.6.m))

View(win_minus_los)


portfolio_returns_GH<-win_minus_los %>%
        spread(key=wh.52.class,value=mean_return) %>%
        mutate(diff=Winners_GH-Losers_GH) %>%
        select(month, Winners_GH, Losers_GH, diff)

View(portfolio_returns_GH)

#Mean of Winners returns over the years
#If we filter out only top 1500 companies, the mean return for
#the Winner portfolio decreases to 19.9%
mean(portfolio_returns_GH$Winners_GH)

#Mean of Losers returns over the years
#If we filter out only top 1500 companies, the mean return for
#the Loser portfolio increased to -2.3%
mean(portfolio_returns_GH$Losers_GH)

#Mean of the difference in returns over the years
#If we filter out only top 1500 companies, the mean return for
#the Loser portfolio increased to -2.3%
mean(portfolio_returns_GH$diff)
