#Find 52 week high
#Arrange by symbol and date first
#How to arrange the code so that we have the last

#1. Find ratios
GH_ratio <- function(){

        r <- gather_data(symbols=secref$symbol,1998:2007)

        ratio <- r %>% arrange(symbol, date)  %>%
                     group_by(symbol) %>%
                     mutate(price_52_wh = roll_max(price, 252, fill = NA, align = "right")) %>%
                     mutate(52_wh_ratio=price/price_52_wh)

        #Compare 52 week high to Bloomberg calculations. Use stopifnot

        #Filter out only top 1500 companies
        ratio<-filter(ratio, top.1500 & ! is.na(52_wh_ratio))

        return(ratio)
        }

#2. Rank.

GH_rank<- function() {
        r<-GH_ratio()

        r<- %>% group_by(date) %>%
        mutate(52wh.class = as.character(ntile(52_wh_ratio, n = 3))) %>%
        mutate(52wh.class = ifelse(ret.class == "1", "Losers_GH", 52wh.class)) %>%
        mutate(52wh.class = ifelse(ret.class == "3", "Winners_GH", 52wh.class)) %>%
        mutate(52wh.class = factor(ret.class, levels = c("Losers_GH", "2", "Winners_GH"))) %>%
        ungroup()

        return(r)
        invisible(r)
        }

#3. Then put this into monthly function
f<-GH_rank()
s<-gather_monthly(f)

#4 Form a portfolio

GH.0.6.m.ret<- function(monthly_data){

     r<-monthly_data %>%
        spread(key=52wh.class,value=ret.0.6.m) %>%
        mutate(diff=Winners_GH-Losers_GH) %>%
        select(date, Winners_GH, Losers_GH, diff)

     return(r)
}



