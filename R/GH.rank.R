GH.rank <- function(data) {
        data %>%
                select(-top.1500, -m.ind, -id, -year) %>%
                group_by(symbol) %>%
                arrange(symbol, v.date) %>%
                mutate(highest = max(price)) %>%
                filter(row_number(v.date)==n()) %>%
                ungroup() %>%
                mutate(index = price/highest, index_rank = row_number(index)) %>%
                select(symbol, index, index_rank) -> z3
        invisible(z3)
}
