#Find the difference in future 6 months returns for the Winner and Loser portfolios
#Create a table with the Winner, Loser, diff columns
library(tidyr)

#Use monthly data with rankings
MG.0.6.m.ret<-function(monthly_data) {

     r<-monthly_data %>%
        spread(key=ind.class,value=ret.0.6.m) %>%
        mutate(diff=Winners_MG-Losers_MG) %>%
        select(date, Winners_MG, Losers_MG, diff)

      return(r)
}
