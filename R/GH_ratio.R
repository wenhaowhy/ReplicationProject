#Find 52 week high
#Arrange by symbol and date first
#How to arrange the code so that we have the last


#1. Find ratios
GH_ratio <- function(){

        r<-gather_data()

        r<- r %>% arrange(symbol, date)  %>%
                     group_by(symbol) %>%
                     mutate(price_52_wh = roll_max(price, 252, fill = NA, align = "right")) %>%
                     #look up what professr
                     mutate(52_wh_ratio=price/price_52_wh)

        #Compare 52 week high to Bloomberg calculations
        stopifnot(
                round(subset(x, date == "2006-01-03" & symbol == "IBM", select = "price_52_wh"), 5) == ,
                round(subset(x, date == "2007-08-06" & symbol == "GOOG", select = "price_52_wh"), 5) ==
        )

        #Filter out only top 1500 companies
        r<-filter(r,top.1500 & ! is.na(52_wh_ratio))

        return(r)

        invisible(r)
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

         monthly_data %>%
        spread(key=52wh.class,value=52_wh_ratio) %>%
        mutate(diff=Winners_GH-Losers_GH) %>%
        select(date, Winners_GH, Losers_GH, diff)
}

3.
#Check whether 52-week high prices make sense
#Get rid of
summary(x)

#Find the last trading day of the month
monthly <- x %>% group_by(month) %>%
        filter(min_rank(desc(date)) == 1)

View(monthly)


#Compare 52 week high calculations to the Bloomberg data
stopifnot(
        round(subset(x, date == "2006-01-03" & symbol == "IBM", select = "price_52_wh"), 5) == ,
        round(subset(x, date == "2007-08-06" & symbol == "GOOG", select = "price_52_wh"), 5) ==
)


#Find the ratio and arrange in descneding order
#Arrange the ratios in descenig order for each month
monthly %>% group_by(date) %>%
        mutate(ratio_52_wh=price/price_52_wh) %>%
        arrange(desc(ratio_52_wh)) %>%
        mutate(rank_52_wh = row_number(desc(ratio_52_wh))) %>%
        arrange(rank_52_wh)->m

#Alternative to ranking. What's better?
monthly %>% group_by(date) %>%
        mutate(ratio_52_wh=price/price_52_wh) %>%
        arrange(desc(ratio_52_wh)) %>%
        mutate(wh_class=ntile(ratio_52_wh, n=3))->instead
View(instead)
