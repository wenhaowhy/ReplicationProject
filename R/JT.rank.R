JT.Rank <- function(data){
        data %>%
                select(-top.1500, -m.ind, -id, -year) %>%
                group_by(symbol) %>%
                arrange(symbol, v.date)  %>%
                mutate(cum_ret = cumprod(1+tret) - 1) %>%
                filter(row_number(v.date) == n()) %>%
                ungroup() %>%
                mutate(cum_rank = row_number(cum_ret)) %>%
                select(symbol, cum_ret, cum_rank) -> z1
        invisible(z1)
}
