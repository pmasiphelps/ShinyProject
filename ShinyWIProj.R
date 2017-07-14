#RShiny Project
#Connect to RShiny hosting account - https://www.shinyapps.io/admin/#/dashboard
rsconnect::setAccountInfo(name='pmasiphelps', token='97B09C90186FFB5822F2B3EFE953E00A', secret='h8zXl01tUOwxGmYA5bVBRCU1RSM/d0WlgvjGAolJ')

setwd("/Users/Patrick/Documents/ShinyProject")

#packages
library(dplyr)
library(tidyverse)
library(lubridate)

#Data Cleaning
WIstops <- read.csv("WI-clean.csv")

WIstops$stop_date <- format(as.Date(WIstops$stop_date), "%Y/%m/%d")  #change the stop date to type date

WIstops2 <- WIstops %>% filter(stop_date > "2010/12/31" & stop_date < "2016/01/01")  #cut down data to 2011-2016

WIstops3 <- WIstops2 %>% mutate(year = as.factor(substr(stop_date, 1, 4)), month = as.factor(substr(stop_date, 6, 7)), day = as.factor(substr(stop_date, 9, 10)))  #add year/month/day variables as factors

WIstops3$driver_race <- as.character(WIstops3$driver_race)

WIstops3$driver_race[WIstops3$driver_race == ""] <- "None Provided"

#missing data
sapply(WIstops3, function(x) sum(is.na(x)))

#some basic analysis

#racial stuff
WIstops6 <- WIstops3 %>% group_by(year, driver_race) %>% summarise(total_stops = n())

WIstops6$driver_race <- as.character(WIstops6$driver_race)

WIstops6$driver_race[WIstops6$driver_race == ""] <- "None Provided"

WIraceyearbar <- ggplot(data = WIstops6, aes(x = year, y = total_stops, fill = driver_race)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Race/Year") + 
  xlab("Year") + 
  ylab("Number of Stops")
  
#total stops by year
WIstops4 <- WIstops3 %>% group_by(year, month) %>% summarize(n = n())

#stops by county/race
WIstops3$officer_id <- as.factor(WIstops3$officer_id)
  
WIstops5 <- WIstops3 %>% group_by(county_name, year, driver_race) %>% summarize(total_stops = n()) %>% arrange(county_name)

WIstops5$driver_race <- as.character(WIstops5$driver_race)

WIstops5$driver_race[WIstops5$driver_race == ""] <- "None Provided"

WIcountyraceyearbar <- ggplot(data = WIstops5, aes(x = year, y = total_stops, fill = driver_race)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Race/Year") + 
  xlab("Year") + 
  ylab("Number of Stops") +
  facet_wrap( ~ county_name)

#time and location stuff

WIstops7 <- WIstops3 %>% group_by(year, month) %>% summarize(percent_missing_location = 100*sum(is.na(lat))/n())
  
#Shows how individual officers recorded race data over time
WIstops8 <- WIstops3 %>% group_by(officer_id, year, month) %>% 
  summarize(num_stops = n(), percent_missing_race = 100*sum(driver_race == "None Provided")/n())

WIstops8$yrmth <- paste(WIstops8$year, WIstops8$month, sep = "/")

#line graph of officers recording race variable over time
WIofficerracemonthline <- ggplot(data = WIstops8, aes(x = yrmth, 
                                                      y = percent_missing_race, 
                                                      group = officer_id)) + 
  geom_line(alpha = .1) +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "none")

#heat map of officers recording race variable over time
WIofficerracemonthscatter <- ggplot(data = WIstops8, aes(x = yrmth, 
                                                      y = percent_missing_race)) + 
  geom_point(alpha = .1) +
  geom_bin2d() +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#SHows how officers(by police department) 
WIstops9 <- WIstops3 %>% group_by(police_department, year, month) %>% 
  summarize(num_stops = n(), percent_missing_race = sum(driver_race == "None Provided")/n())

WIstops10 <- WIstops3 %>% group_by(police_department, year, driver_race) %>% summarize(total_stops = n())

#bar graph showing county change in unrecorded race data each year
WIdeptraceyearbar <- ggplot(data = WIstops10, aes(x = year, y = total_stops, fill = driver_race)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Race/Year") + 
  xlab("Year") + 
  ylab("Number of Stops") +
  facet_wrap( ~ police_department)

WIdeptraceyearbar <- ggplot(data = WIstops3, aes(x = year, y = total_stops, fill = driver_race)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Race/Year") + 
  xlab("Year") + 
  ylab("Number of Stops") +
  facet_wrap( ~ police_department)

#seasonality 
#stops per month/year
WIstops3$yrmth <- paste(WIstops3$year, WIstops3$month, sep = "/")
WIstops4 <- WIstops3 %>% group_by(year, month) %>% summarize(total_stops = n())
WImonthyearbar <- ggplot(data = WIstops4, aes(x = year, y = total_stops, fill = month)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Month/Year") + 
  xlab("Month/Year") + 
  ylab("Number of Stops")

#stops by county per month/year
WIstops11 <- WIstops3 %>% group_by(county_name, year, month) %>% summarize(total_stops = n())
WIcountymonthyearbar <- ggplot(data = WIstops11, aes(x = year, y = total_stops, fill = month)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Month/Year") + 
  xlab("Month/Year") + 
  ylab("Number of Stops") +
  facet_wrap( ~ county_name)

#average number of stops by county by month (so averaging by month across 2011-2015)
WIstops12 <- WIstops3 %>% group_by(county_name, month) %>% summarize(total_stops = n())
WIcountymonthbar <- ggplot(data = WIstops12, aes(x = month, y = total_stops, fill = month)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Month/Year") + 
  xlab("Month/Year") + 
  ylab("Number of Stops") +
  facet_wrap( ~ county_name)

#get average num of stops by county across 2011-2015
WIstops13 <- WIstops3 %>% group_by(county_name) %>% summarize(stops_per_month = n()/60)
WIstops14 <- merge(WIstops3, WIstops13, by = "county_name")

WIstops15 <- WIstops3 %>% group_by(month) %>% summarize(n())

#get the percent difference between each county's stops in a specific month (across 2011-2015) compared to an average month
WIstops16 <- WIstops14 %>% group_by(county_name, stops_per_month, month) %>% 
  summarize(total_stops = n()) %>% 
  mutate(avg_stops = total_stops/5, 
         percent_diff_stops_than_avg = (avg_stops-stops_per_month)/stops_per_month)

#bar chart of the above
WIcountymonthpercentdiffbar <- ggplot(data = WIstops16, aes(x = month, y = percent_diff_stops_than_avg, fill = month)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops: Percent Difference from Avg Month") + 
  xlab("Month") + 
  ylab("Percent Difference from Average Month") +
  facet_wrap( ~ county_name)

