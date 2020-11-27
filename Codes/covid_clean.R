#######################
## Analysis of       ##
##  Covid-19 cases   ##
##    and            ##
##  Death due to     ## 
##  Covid-19 on a    ## 
##  given day        ##
##                   ##
##      NO. 2        ##
##                   ##
##Cleaning the data  ##
##                   ##
#######################

#### Removing groupings and redundant observations from population data 

# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data from github
pop_data <- "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1/pop_data_raw.csv"
df_pop <- read_csv( pop_data )

## Check the observations:
#   Lot of grouping observations
#     usually contains a number
d1_pop <- df_pop %>% filter(grepl("[[:digit:]]", df_pop$iso2c))
d1_pop

# Filter grouping observations
df_pop <- df_pop %>% filter( !grepl("[[:digit:]]", df_pop$iso2c) )

# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# 1st drop specific values
drop_id <- c("EU","HK","OE")

# Check for filtering
df_pop %>% filter( grepl( paste( drop_id , collapse="|"), df_pop$iso2c ) )

# Save the opposite
df_pop <- df_pop %>% filter( !grepl( paste( drop_id , collapse="|"), df_pop$iso2c ) ) 

# 2nd drop values that starts with ceartain char

# Get the first letter from iso2c
fl_iso2c <- substr(df_pop$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW")

# Check
d1_pop <- df_pop %>% filter( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                       !grepl( paste( retain_id , collapse="|"), df_pop$iso2c ) ) 

# Save observations which are the opposite (use of !)
df_pop <- df_pop %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                          !grepl( paste( retain_id , collapse="|"), df_pop$iso2c ) ) ) 

# Clear non-needed variables
rm( d1_pop , drop_id, fl_iso2c , retain_id )

#### Removing missing values 

# Check for missing observations
m <- df_pop %>% filter( !complete.cases( df_pop ) )
# There are only two observation wit hmissing variables. However, it looks like the iso2c code for Namibia is 'NA' thus R thinks its missing. I would not drop Namibia in order to preserve coverage. 

# Drop if life-expectancy, gdp or total population missing -> if not complete case except iso2c
df_pop <- df_pop %>% filter( complete.cases( df_pop ) | is.na( df_pop$iso2c ) )


###
# CLEAN VARIABLES for population
#
# Recreate table:
#   Rename variables and scale them
#   Drop all the others !! in this case write into readme it is referring to year 2018!!
df_pop <-df_pop %>% transmute( country = country,
                       population=SP.POP.TOTL) #/1000000, SP.POP.TOTL

- ###
  # Check for extreme values
  # all HISTOGRAMS
  df_pop %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()


# It seems we have a large value(s) for population:
df_pop %>% filter( population > 500 )
# These are India and China... not an extreme value

# Check for summary as well
summary( df_pop)

# Save the clean data file
# Saving raw population data to csv file

# write.csv(df_pop, "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1//pop_data_clean.csv", row.names = FALSE)


covid_data <- "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1/covid_data_raw.csv"
df_covid <- read_csv( covid_data )

# Dropping redundant variables (‘FIPS, Admin2, Last Update, Lat, Long, Combined Key, Incidence Rate, Case.Fatality Ratio’)

df_covid <- df_covid[,-c(1:2,5:7,12:14)]

# Combining rows for covid data 

library(dplyr)

df_covid_1 <- df_covid %>% group_by(Country_Region) %>% summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered), Active = sum(Active))

# Saving clean covid data 

# write.csv(df_covid_1, "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1//covid_data_clean.csv", row.names = FALSE)

# Joining covid data with population 

full_data <- merge(x=df_covid_1, y=df_pop, by.x=c("Country_Region"), 
            by.y=c("country"), all = TRUE)


# Changing some of the country names to match

full_data["15", "Country_Region"] <- "Bahamas"
full_data["31", "Country_Region"] <- "Brunei"
full_data["48", "Country_Region"] <- "Congo"
full_data["49", "Country_Region"] <- "Congo"
full_data["50", "Country_Region"] <- "Congo"
full_data["51", "Country_Region"] <- "Congo"
full_data["59", "Country_Region"] <- "Czech Republic"
full_data["67", "Country_Region"] <- "Egypt"
full_data["81", "Country_Region"] <- "Gambia"
full_data["102", "Country_Region"] <- "Iran"
full_data["116", "Country_Region"] <- "Korea, Rep."
full_data["119", "Country_Region"] <- "Kyrgyzstan"
full_data["121", "Country_Region"] <- "Laos"		
full_data["179", "Country_Region"] <- "St. Kitts and Nevis"	
full_data["180", "Country_Region"] <- "St. Lucia"
full_data["181", "Country_Region"] <- "St. Vincent and the Grenadines"
full_data["177", "Country_Region"] <- "Russia"
full_data["192", "Country_Region"] <- "Slovakia"
full_data["210", "Country_Region"] <- "Syria"
full_data["234", "Country_Region"] <- "Venezuela"
full_data["230", "Country_Region"] <- "United States"
full_data["240", "Country_Region"] <- "Yemen"

# Merging rows 

full_data <- full_data %>% group_by(Country_Region) %>% summarize(Confirmed = sum(Confirmed, na.rm=T), Deaths = sum(Deaths, na.rm=T), Recovered = sum(Recovered, na.rm=T), Active = sum(Active, na.rm=T), poulation = sum(population, na.rm=T))


# Renaming variables and dividing by population to establish 'per capita' metrics. Scaling up to reflect metrics per 1M of population

options(scipen = 999)

full_data <- full_data %>% transmute( Country = Country_Region,
                                      CasePerCap = (Confirmed/full_data$poulation) * 100000,
                                      DeathsPerCap = (Deaths/full_data$poulation) * 100000,
                                      Population = full_data$poulation)


#filtering out 0 observations 

library(dplyr)
full_data_clean <- filter(full_data, CasePerCap > 0, DeathsPerCap > 0, Population > 0)




write.csv(full_data_clean, "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1//covid_data_clean.csv", row.names = FALSE)
