gather_monthly <- function(x){

        ## Filter out the last trading day of the month

        monthly <- x %>% group_by(month) %>%
                filter(min_rank(desc(date)) == 1 & ! is.na(!!!))

        return(monthly)
}
