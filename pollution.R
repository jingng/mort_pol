install.packages(c("httr", "jsonlite", "stringr", "dplyr"))
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)

get_pol <- function(d, pollutant_name) {
  p <- d[d$parameter==pollutant_name,]
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
}

county_lst<-c("037","073","065","059","071","067","001","085")

base = "https://aqs.epa.gov/data/api/dailyData/byCounty?email=test@aqs.api&key=test&param=81102,42101,42602&bdate="

start_year <- seq.Date(as.Date("2004-01-01"), as.Date("2019-01-01"), "years")
start_year <- as.numeric(str_replace_all(start_year, "[^[:alnum:]]", "") )

end_year <- seq.Date(as.Date("2004-12-31"), as.Date("2019-12-31"), "years")
end_year <- as.numeric(str_replace_all(end_year, "[^[:alnum:]]", "") )

county_sum <- function(start_year, end_year, county) {
  
  nit_df <- data.frame(matrix(ncol = 5))
  colnames(nit_df)<- c('Year', 'Month', 'County', 'Pollutant', 'P_Mean')
  carbon_df <- data.frame(matrix(ncol = 5))
  colnames(carbon_df)<- c('Year', 'Month', 'County', 'Pollutant', 'P_Mean')
  pm_df <- data.frame(matrix(ncol = 5))
  colnames(pm_df)<- c('Year', 'Month', 'County', 'Pollutant', 'P_Mean')
  
  for (i in 1:length(start_year)) {
    url = paste0(base, start_year[i],"&edate=",end_year[i], "&state=06&county=", county)
    res=GET(url)
    
    data = fromJSON(rawToChar(res$content))
    data=data$Data
    ps=data$pollutant_standard
    f_data = data[ps=="NO2 1-hour 2010"| ps=="CO 1-hour 1971" | ps=="PM10 24-hour 2006",]
    f_data$year <- format(as.Date(f_data$date_local), "%Y")
    f_data$month <- format(as.Date(f_data$date_local), "%-m")
    
    
    # Nitrogen Dioxide
    
    p <- get_pol(f_data, "Nitrogen dioxide (NO2)")
    nit_sum <- aggregate(P_Mean ~ Year + Month + County + Pollutant, p, mean)
    nit_sum <- nit_sum[order(as.numeric(as.character(nit_sum$Month))), ]
    nit_df <- rbind(nit_df, nit_sum)
    
    # Carbon Monoxide
    
    c_data <- get_pol(f_data, "Carbon monoxide")
    car_sum <- aggregate(P_Mean ~ Year + Month + County + Pollutant, c_data, mean)
    car_sum <- car_sum[order(as.numeric(as.character(car_sum$Month))), ]
    carbon_df <- rbind(carbon_df, car_sum)
    
    #"PM10 Total 0-10um STP"
    
    pm_data <- get_pol(f_data, "PM10 Total 0-10um STP")
    if (nrow(pm_data!=0)) {
      pm_sum <- aggregate(P_Mean ~ Year + Month + County + Pollutant, pm_data, mean)
      pm_sum <- pm_sum[order(as.numeric(as.character(pm_sum$Month))), ]
      pm_df <- rbind(pm_df, pm_sum)
      
    }
    
  }
  
  return (list(nit_df[-1,], carbon_df[-1,], pm_df[-1,]))
  
}

nit_df <- data.frame(matrix(ncol = 4))
colnames(nit_df)<- c('Year', 'Month', 'County', 'P_Mean')
carbon_df <- data.frame(matrix(ncol = 4))
colnames(carbon_df)<- c('Year', 'Month', 'County', 'P_Mean')
pm_df <- data.frame(matrix(ncol = 4))
colnames(pm_df)<- c('Year', 'Month', 'County', 'P_Mean')

pol_merge <- function(lst, lst_no) {
  county_name = lst[[lst_no]]$County[1]
  county_toll <- df_fil[df_fil$County==county_name,]
  merged <- merge(lst[lst_no], county_toll, by.x = c('Year','Month','County'), by.y = c('Year','Month','County'))
  
  return (merged)
}

order_per_m <- function(dataframe) {
  ordered <- dataframe[order(c(as.numeric(as.character(dataframe$Year))), as.numeric(as.character(dataframe$Month)), dataframe$County), ]
  return (ordered)
}

nit_merged_df <- data.frame(matrix(ncol = 6))
colnames(nit_merged_df)<- c('Year', 'Month', 'County', 'Pollutant','P_Mean', 'Count')
co_merged_df <- data.frame(matrix(ncol = 6))
colnames(co_merged_df)<- c('Year', 'Month', 'County', 'Pollutant', 'P_Mean', 'Count')
pm_merged_df <- data.frame(matrix(ncol = 6))
colnames(pm_merged_df)<- c('Year', 'Month', 'County', 'Pollutant', 'P_Mean', 'Count')

for (j in county_lst){
  poll_lst <- county_sum(start_year, end_year, j)
  
  nit_merged <- pol_merge(poll_lst, 1)
  co_merged <- pol_merge(poll_lst, 2)
  pm_merged <- pol_merge(poll_lst, 3)
  
  nit_merged_df <- rbind(nit_merged_df, nit_merged)
  co_merged_df <- rbind(co_merged_df, co_merged)
  pm_merged_df <- rbind(pm_merged_df, pm_merged)
}

nit_merged_df <- order_per_m(nit_merged_df)
co_merged_df <- order_per_m(co_merged_df)
pm_merged_df <- order_per_m(pm_merged_df)

#writing
pm_merged_df <- pm_merged_df[-1,]
write.table(pm_merged_df, file="pm.txt", col.names = TRUE, row.names= FALSE)

# for all of cali

#nit 
n_pc <- aggregate(cbind(P_Mean, Count) ~ Year + Month, nit_merged_df, sum)
n_x <- n_pc$P_Mean
n_y <- n_pc$Count

#co
co_pc <- aggregate(cbind(P_Mean, Count) ~ Year + Month, co_merged_df, sum)
co_x <- co_pc$P_Mean
co_y <- co_pc$Count

#pm 
pm_pc <- aggregate(cbind(P_Mean, Count) ~ Year + Month, pm_merged_df, sum)
pm_x <- pm_pc$P_Mean
pm_y <- pm_pc$Count

# sequentially
time <- n_pc[order(c(as.numeric(as.character(n_pc$Year))), as.numeric(as.character(n_pc$Month))), ]


