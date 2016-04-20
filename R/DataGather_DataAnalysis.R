
library(ws.data)
library(lubridate)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(RcppRoll)

install.packages("RcppRoll")

data(secref)
data(yearly)
View(head(daily.1998))

#gather the data

grab.data <- function(symbols, years){

        require(ws.data)

        grabbed <- data.frame()

        for( i in years ){

                file.name <- paste("daily", i, sep = ".")
                data(list = file.name)

                grabbed <- rbind(grabbed, subset(eval(parse(text=file.name)), symbol %in% symbols))

        }

        invisible(grabbed)
}

x<-grab.data(symbols=secref$symbol,1998:2007)
View(head(x))

#use dplyr package to make the data frame tbl_df to avoid printing a lot of data on the screen
x<-tbl_df(x)
y<-tbl_df(yearly)

x <- mutate(x, year = lubridate::year(v.date),
            month = paste(lubridate::month(v.date, TRUE, TRUE), year, sep = "-"))

#merge with yearly by year and id
x <- left_join(select(x, v.date, month, year, symbol, id, tret, price, price.unadj,year),
               select(y, id, year, top.1500),
               by = c("year", "id")) %>%
        filter(top.1500) %>% tbl_df()

View(head(x))

#merge c with secref to get industry data and arrange by date and symbol
x <- left_join(x, select(secref, id, name, m.sec, m.ind), by = "id") %>%
        arrange(v.date, symbol)
View(head(x))

#find monthly returns

monthly.ret <- x %>% group_by(month, symbol) %>% summarize(ret.0.1.m = sum(tret))

summarize(monthly.ret)
View(head(monthly.ret))
?min_rank



