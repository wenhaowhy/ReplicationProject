Data.6.6 <- function(yyyy, mm, pro, data) {

        date1 <- ceiling_date((ymd(paste(yyyy,mm,days_in_month(mm),sep="-")) %m+%
                                       months(-pro)), "month") - duration(1, "days")
        date2 <- (ymd(paste(yyyy,mm,01,sep="-")) %m+% months(-pro-5))

        date1 <- as.Date(date1)
        date2 <- as.Date(date2)

        data <- filter(data, v.date<= date1, v.date>= date2)

        invisible(data)
}
