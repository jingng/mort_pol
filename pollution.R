install.packages(c("httr", "jsonlite"))
install.packages("stringr")
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)

#loop through years
#PM10:81102,CO:42101, NO2:42602
#"NO2 1-hour 2010"

#Los Angeles, San Diego, Riverside, Orange, San Bernardino, Sacramento, Alameda, Santa Clara
county_lst<-c("037","073","065","059","071","067","001","085")

base = "https://aqs.epa.gov/data/api/dailyData/byCounty?email=test@aqs.api&key=test&param=81102,42101,42602&bdate="

start_year <- seq.Date(as.Date("2004-01-01"), as.Date("2019-01-01"), "years")
start_year <- as.numeric(str_replace_all(start_year, "[^[:alnum:]]", "") )

end_year <- seq.Date(as.Date("2004-12-31"), as.Date("2019-12-31"), "years")
end_year <- as.numeric(str_replace_all(end_year, "[^[:alnum:]]", "") )

# startdate=20040101
# enddate=20041231
county="001"

for (i in 1:length(start_year)) {
  url = paste0(base, start_year[i],"&edate=",end_year[i], "&state=06&county=", county)
  # res = GET("https://aqs.epa.gov/data/api/dailyData/byCounty?email=test@aqs.api&key=test&param=81102,42101,42602&bdate=20040101&edate=20041229&state=06&county=001")
  res=GET(url)
  
  data = fromJSON(rawToChar(res$content))
  data=data$Data
  ps=data$pollutant_standard
  f_data = data[ps=="NO2 1-hour 2010"| ps=="CO 1-hour 1971" | ps=="PM10 24-hour 2006",]
  f_data$year <- format(as.Date(f_data$date_local), "%Y")
  f_data$month <- format(as.Date(f_data$date_local), "%m")
  
  p <- f_data[f_data$parameter=="Nitrogen dioxide (NO2)",]
  p <- data.frame(
    Year = p$year,
    Month = p$month,
    County = p$county,
    City = p$city,
    Pollutant = p$parameter,
    P_Mean = p$arithmetic_mean,
    Unit = p$units_of_measure,
    AQI = p$aqi
    
  )
  
  month_sum <- aggregate( cbind(P_Mean, AQI) ~ Year + Month + County, p, mean)
}

m_s <- function(start_year, end_year, county) {
  
  for (i in 1:length(start_year)) {
    url = paste0(base, start_year[i],"&edate=",end_year[i], "&state=06&county=", county)
    # res = GET("https://aqs.epa.gov/data/api/dailyData/byCounty?email=test@aqs.api&key=test&param=81102,42101,42602&bdate=20040101&edate=20041229&state=06&county=001")
    res=GET(url)
    
    data = fromJSON(rawToChar(res$content))
    data=data$Data
    ps=data$pollutant_standard
    f_data = data[ps=="NO2 1-hour 2010"| ps=="CO 1-hour 1971" | ps=="PM10 24-hour 2006",]
    f_data$year <- format(as.Date(f_data$date_local), "%Y")
    f_data$month <- format(as.Date(f_data$date_local), "%m")
    
    p <- f_data[f_data$parameter=="Nitrogen dioxide (NO2)",]
    p <- data.frame(
      Year = p$year,
      Month = p$month,
      County = p$county,
      City = p$city,
      Pollutant = p$parameter,
      P_Mean = p$arithmetic_mean,
      Unit = p$units_of_measure,
      AQI = p$aqi
      
    )
    
    month_sum <- aggregate( cbind(P_Mean, AQI) ~ Year + Month + County, p, mean)
  }
  
}

test <-m_s(start_year[1], end_year[1], county)

df <- data.frame(matrix(ncol = 5))
colnames(df) <- c('Year', 'Month', 'County', 'P_Mean', 'AQI')
for (j in 1:length(start_year)) {
  
  df_new <- m_s(start_year[j], end_year[j], county)
  df <- rbind(df, df_new)
}


#Works 
startdate=20040101
enddate=20041231
county="001"

url = paste0(base, startdate,"&edate=",enddate, "&state=06&county=", county)
# res = GET("https://aqs.epa.gov/data/api/dailyData/byCounty?email=test@aqs.api&key=test&param=81102,42101,42602&bdate=20040101&edate=20041229&state=06&county=001")
res=GET(url)

data = fromJSON(rawToChar(res$content))
data=data$Data
ps=data$pollutant_standard
f_data = data[ps=="NO2 1-hour 2010"| ps=="CO 1-hour 1971" | ps=="PM10 24-hour 2006",]
f_data$year <- format(as.Date(f_data$date_local), "%Y")
f_data$month <- format(as.Date(f_data$date_local), "%m")

# Filter relevant data and separate by pollutants
#Make into a function
#"Nitrogen dioxide (NO2)"
#"Carbon Monoxide"
#"PM10 Total 0-10um STP"

p <- f_data[f_data$parameter=="Nitrogen dioxide (NO2)",]
p <- data.frame(
  Year = p$year,
  Month = p$month,
  County = p$county,
  City = p$city,
  Pollutant = p$parameter,
  P_Mean = p$arithmetic_mean,
  Unit = p$units_of_measure,
  AQI = p$aqi
  
)

month_sum <- aggregate( cbind(P_Mean, AQI) ~ Year + Month + County, p, mean)

