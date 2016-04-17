# Function to read all the daily.year data

all_data<-data.frame()
process_daily_file <- function(year){

        library(ws.data)
        file.name<- paste0("daily.", year)
        data(file.name)
}

datalist = list()

for (i in 1:year) {
        # ... make some data
        dat <- data.frame()
        daily.$i <- i  # maybe you want to keep track of which iteration produced it?
        datalist[[i]] <- data(file.name)
}

big_data = do.call(rbind, datalist)
