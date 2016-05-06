MG.rank <- function (data) {
        data  %>%
                select(-top.1500, -year, -id, -symbol) %>%
                group_by(m.ind) %>%
                arrange(m.ind, v.date)  %>%
                mutate(ind_ret = mean(tret), na.rm=TRUE) %>%
                filter(row_number(v.date)==n()) %>%
                ungroup() %>%
                mutate(ind_rank = row_number(ind_ret)) %>%
                select(m.ind, ind_ret, ind_rank) -> z2
}
