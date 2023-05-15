install.packages(c("jsonlite", "tidyr"))
library(jsonlite)
library(tidyr)

# Hub
# https://api.census.gov/data.html


# For monthly US pop, (2000-2009)
url = 'https://api.census.gov/data/2000/pep/int_natmonthly?get=POP,GEONAME,MONTHLY_DESC&for=us:1'
res=GET(url)
monthly_US= fromJSON(rawToChar(res$content))

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

# ------------------------------------------------------------------------------------------------------
# For yearly county
url <- 'https://api.census.gov/data/2000/pep/int_population?get=GEONAME,POP,DATE_DESC&for=county:001,037,073,065,059,071,067,085&in=state:06&DATE_=6,7,8,9,10,11,12,13'

res<-GET(url)
yearly_c <- fromJSON(rawToChar(res$content))
yearly_c <- as.data.frame(yearly_c)
colnames(yearly_c) <- c("GEONAME","POP","DATE_DESC","DATE_","state","county")
yearly_c <- yearly_c[-1,]

yearly_c <- separate(yearly_c,
                     DATE_DESC,
                     into = c('date', 'string'),
                     sep='\\s',
                     extra='merge') 

yearly_c <- separate(yearly_c, 
                     date, 
                     into=c("Month", "Day","Year"), 
                     sep="/")

yearly_c <- yearly_c %>% mutate_at(c('Month', 'Day', 'Year'), as.numeric)

yearly_c <- yearly_c[,c('county', 'Year', 'POP')]

# 2010

y_2010 <- data.frame(matrix(ncol = 3))
colnames(y_2010)<- c('county', 'Year','POP')
for (i in 2:6) {

  url <- 'https://api.census.gov/data/2015/pep/cty?get=STNAME,POP,CTYNAME&for=county:001,037,073,065,059,071,067,085&in=state:06&DATE_=2'
  res<-GET(url)
  new_yearly <- fromJSON(rawToChar(res$content))
  new_yearly <- as.data.frame(new_yearly)
  colnames(new_yearly) <- new_yearly[1,]
  new_yearly <- new_yearly[-1,]
  
  pop_yearly <- as.numeric(new_yearly$POP)
  curr_county <- new_yearly$county
  year_lst<- rep.int(2009+i, length(pop_yearly))
  quick <- data.frame(curr_county, year_lst, pop_yearly)
  colnames(quick)<- c('county', 'Year','POP')
  y_2010 <- rbind(y_2010, quick)
  
}

y_2016 <- data.frame(matrix(ncol = 3))
colnames(y_2016)<- c('county', 'Year','POP')
for (i in 2:5) {
  
  url <- 'https://api.census.gov/data/2014/pep/cty?get=STNAME,POP,CTYNAME&for=county:001,037,073,065,059,071,067,085&in=state:06&DATE_=2'
  res<-GET(url)
  new_yearly <- fromJSON(rawToChar(res$content))
  new_yearly <- as.data.frame(new_yearly)
  colnames(new_yearly) <- new_yearly[1,]
  new_yearly <- new_yearly[-1,]
  
  pop_yearly <- as.numeric(new_yearly$POP)
  curr_county <- new_yearly$county
  year_lst<- rep.int(2014+i, length(pop_yearly))
  quick <- data.frame(curr_county, year_lst, pop_yearly)
  colnames(quick)<- c('county', 'Year','POP')
  y_2016 <- rbind(y_2016, quick)
  
}

yearly_c <- rbind(yearly_c, y_2010)
yearly_c <- rbind(yearly_c, y_2016)
yearly_c <- na.omit(yearly_c)
yearly_c <- yearly_c[order(yearly_c$Year),]

# ----------------------------------------------------------------------------------------
# use yearly_c and final datasets 

mean_US <- aggregate(as.numeric(US_pop) ~ Year, final, mean)
colnames(mean_US) <- c('Year', 'US_mean')
# yearly_c <- subset(yearly_c, select=c(Year, county, POP))
yearly_c <- merge(x=yearly_c, y=mean_US, by="Year")
yearly_c$portion <- as.numeric(yearly_c$POP)/yearly_c$US_mean


all <- merge(x=yearly_c, y=final, by="Year", all.y=TRUE)
all <- all[order(all$Year, all$county, all$Month),]
all$urban_pop <- as.integer(all$portion*as.numeric(all$US_pop))

urban <- subset(all, select=c(Year, county, Month, urban_pop))
