## packages I want loaded for all pages of my site
suppressPackageStartupMessages({
  #library(tidyverse)
  #library(stringr)
  
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  library(reshape2)
  library(scales)
  
  library(forecast)
})

## variables I need for my site 
df.cav <- read.table("../data/cavalier.csv",sep=",",header=TRUE,fill=TRUE)
df.cor <- read.table("../data/corolla.csv",sep=",",header=TRUE,fill=TRUE)
df.rav <- read.table("../data/rav4.csv",sep=",",header=TRUE,fill=TRUE)

df.cav$car <- "cav"
df.cor$car <- "cor"
df.rav$car <- "rav"

df.cars <- rbind(df.cav, df.cor, df.rav)

# Function to fill in monthly data where no monthly data exist
get_missingmonths <- function(df) {
  dfx <- data.frame(date = seq.Date(from = min(df$date, na.rm=TRUE), 
                                    to = max(df$date, na.rm=TRUE), 
                                    by="month")) %>%
    left_join(df, by="date")
  return(dfx)
}


## knitr options I want set as default for all ('global') code chunks
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

#
#  For total miles
#
# aggregate data by month
df.bymonth <- df.cars %>% 
  group_by(date = floor_date(ymd(Date), unit="month")) %>%
  summarise(miles = sum(miles), cost=sum(cost), rate = mean(rate), gal=sum(gal)) 
# Fill in gaps
df.miles <- get_missingmonths(df.bymonth)
# interval, for use in text
t <- interval(min(df.bymonth$date, na.rm=TRUE), max(df.bymonth$date, na.rm=TRUE))
tlen <- time_length(t, unit="year")
avg_miles <- sum(df.bymonth$miles)/tlen
avg_cost <- sum(df.bymonth$cost)/tlen
tot_miles <- sum(df.bymonth$miles)
tot_cost <- sum(df.bymonth$cost)




