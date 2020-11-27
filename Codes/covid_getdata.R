#######################
## Analysis of       ##
##  Covid-19 cases   ##
##    and            ##
##  Death due to     ## 
##  Covid-19 on a    ## 
##  given day        ##
##                   ##
##      NO. 1        ##
##                   ##
##  Getting the data ##
##                   ##
#######################



# Clear memory
rm(list=ls())

# Call packages
install.packages('WDI')
library(WDI)
library(tidyverse)

# Downloading covid data for 10.08.2020
covid_data_raw <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/08-10-2020.csv"))

# Saving raw covid data to csv file
write.csv(covid_data_raw, "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1//covid_data_raw.csv", row.names = FALSE)


# Get all the data - 2018 is the latest available data for life expectancy
pop_data_raw <- WDI(indicator=c('SP.POP.TOTL'), 
                country="all", start=2019, end=2019)

# Saving raw population data to csv file
write.csv(pop_data_raw, "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1//pop_data_raw.csv", row.names = FALSE)
