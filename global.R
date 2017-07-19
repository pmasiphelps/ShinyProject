## global.R ##
##Patrick Masi-Phelps##

library(dplyr)
#this file reads in all of the csv files that this app needs to run.

#csv with monthly stop data by county - used in the red/blue map of counties 
#showing the percent difference in stops compared to the average month
WIcountymonthpercentdiff <- read.csv("WImonthlystops.csv")

#csv with state-wide stop data by month - used in the bar graph of total stops per month
WIstatemonthyrstops <- read.csv("WIstopspermonthyr.csv")

#csv with monthly stop data by race - used in the dygraph time series
WIracemonthstops <- read.csv("WIracemonthlystops.csv")

#csv with the percentage of stops with missing race observations, by officer, by month - 
#used in the heat map/density plot
WIofficerracepercent2 <- read.csv("WIofficerracepercent2.csv")

#set up the hover stuff for monthly stops map
WIcountymonthpercentdiff$hover <- with(WIcountymonthpercentdiff, paste(County, '<br>', 
                                                                       "Stops this Month", total_stops, 
                                                                       "Percent Difference from Average Month", percent_diff_stops_than_avg, "<br>"))

#creates a variable that will serve as the user's choice for the month slider in the county map
month_choice <- (WIcountymonthpercentdiff %>% distinct(month))[,1]

