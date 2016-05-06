JT.6.6.Return <- function(yyyy, mm, pro, data) {
        data1 <- Top.1500(yyyy, mm, pro, data)
        data2 <- Data.6.6(yyyy, mm, pro, data1)
        data_now <- filter(data1,
                           month==mm,
                           year==yyyy)
        rank <- JT.Index(data2)
        winner_list <- filter(rank, cum_rank<=200)$symbol
        loser_list <- filter(rank, cum_rank>(n()-200))$symbol
        data_now  %>%
                select(-top.1500, -m.ind, -id, -year) %>%
                filter(symbol %in% winner_list) %>%
                group_by(symbol) %>%
                arrange(symbol, v.date) %>%
                mutate(cum_ret = cumprod(1+tret) - 1) %>%
                filter(row_number(v.date) == n()) -> return1
        mean(return1$cum_ret) -> result1
        data_now  %>%
                select(-top.1500, -m.ind, -id, -year) %>%
                filter(symbol %in% loser_list) %>%
                group_by(symbol) %>%
                arrange(symbol, v.date) %>%
                mutate(cum_ret = cumprod(1+tret) - 1) %>%
                filter(row_number(v.date) == n()) -> return2
        mean(return2$cum_ret) -> result2
        result <- c(result1, result2)
        invisible(result)
}
