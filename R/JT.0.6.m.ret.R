#Find the difference in future 6 months returns for the Winner and Loser portfolios
#Create a table with the Winner, Loser, diff columns
library(tidyr)


JT.0.6.m.ret<-function(monthly_data) {

     r<-monthly_data %>%
        spread(key=ret.class,value=ret.0.6.m) %>%
        mutate(diff=Winners_JT-Losers_JT) %>%
        select(symbol, date, Winners_JT, Losers_JT, diff)

     return(r)
     }
