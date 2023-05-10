install.packages(c("jsonlite", "tidyr"))
library(jsonlite)
library(tidyr)

# For monthly US pop, (2000-2009)
url <- 'https://api.census.gov/data/2000/pep/int_natmonthly?get=POP,GEONAME,MONTHLY_DESC&for=us:1'
res <- GET(url)
monthly_US <- fromJSON(rawToChar(res$content))

monthly_US <- as.data.frame(monthly_US)
colnames(monthly_US) <- c('US_pop','GEONAME','MONTHLY_DESC','us')#monthly_US[1,]
monthly_US <- monthly_US[-1,]
monthly_US <- separate(monthly_US, 
                       MONTHLY_DESC, 
                       into = c("date", "string"), 
                       sep="\\s", extra='merge')
test <- separate(monthly_US, 
                 date, 
                 into=c("Month", "Day","Year"), 
                 sep="/")
test <- test %>% mutate_at(c('Month', 'Day', 'Year'), as.numeric)
monthly_2000 <- test[,c('US_pop','Month', 'Day', 'Year')]

final <- monthly_2000[monthly_2000$Year>=2004,] # add to df

m<-3
while (m<120){
  url_base <- 'https://api.census.gov/data/2019/pep/natmonthly?get=NAME,POP,MONTHLY_DESC&for=us:1&MONTHLY='
  url_test <- paste0(url_base, m)
  
  res<-GET(url_test)
  new_monthly<- fromJSON(rawToChar(res$content))
  new_pop <- as.numeric(new_monthly[2,2])
  new_date <- as.numeric(str_split_fixed(str_split_fixed(new_monthly[2,3]," ", n=2)[1], "/",3))
  
  new_entry <- c(new_pop, new_date[1], new_date[2], new_date[3])
  
  final <- rbind(final, new_entry)
  m <- m+1
}
