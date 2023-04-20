# success <- 50:200
# plot(success, dpois(success, lambda=75), type='l')

# install.packages("dplyr")
# install.packages("plyr")


library(dplyr)
library(plyr)

url_2004 <- "https://data.chhs.ca.gov/dataset/58619b69-b3cb-41a7-8bfc-fc3a524a9dd4/resource/3f34d5dd-9b30-456b-9258-4d251b550035/download/2021-05-14_deaths_final_2004-2008_occurrence_county_month_sup.csv"
url_2009 <- "https://data.chhs.ca.gov/dataset/58619b69-b3cb-41a7-8bfc-fc3a524a9dd4/resource/489b2dc5-bac0-4ae3-b326-768bd3d907b5/download/2021-05-14_deaths_final_2009-2013_occurrence_county_month_sup.csv"
url_2014 <- "https://data.chhs.ca.gov/dataset/58619b69-b3cb-41a7-8bfc-fc3a524a9dd4/resource/1a69fe2d-9796-49de-be51-1949d09b242a/download/20221215_deaths_final_2014-2018_occurrence_county_month_sup.csv"
url_lst <- c(url_2004, url_2009, url_2014)


func <- function(url) {
  m_data <- read.csv(url)
  m_fil <- m_data[m_data$Cause == "CLD" & m_data$Strata_Name=="Total Population",]
  df <- data.frame(
    Year = m_fil$Year,
    Month = m_fil$Month,
    County = m_fil$County,
    Count = m_fil$Count
  )
  return(df)
  
}
df <- data.frame(matrix(ncol = 4))
colnames(df) <- c('Year', 'Month', 'County', 'Count')
for (x in url_lst) {
  
  df_new <- func(x)
  df <- rbind(df, df_new)
}


# Which counties should I look at? Top 8

total_c <- setNames(na.omit(aggregate(df$Count, by=list(df$County), FUN=sum)),
                    c("County", "Count"))

total_c <- total_c[order(total_c$Count, decreasing = TRUE),]
top <- head(total_c, 8)
county_lst <- top$County

df_fil<-df[df$County %in% top$County, ]
df_fil <- df_fil[order(df_fil$Year, df_fil$County, df_fil$Month),]

par(mfrow=c(2,4))
for (i in county_lst) {
  county <- df_fil[df_fil$County==i,]
  x <- min(county$Count):max(county$Count)
  fit <- dpois(x, lambda=mean(county$Count))
  
  hist(county$Count, 
       freq=FALSE,
       xlim=c(round_any(min(county$Count),10, f=floor), 
              round_any(max(county$Count),10, f=ceiling)),
       ylim=c(0, max(fit)),
       breaks=round_any(max(county$Count),100, f=ceiling)/10,
       main = paste(i),
       xlab="Monthly Death Count",
       ylab="Probability")
  lines(x, fit, 
        col='red',
        lwd=2,
        type='l')
  
}
