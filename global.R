## global.R ##

# use the csv with monthly stop data by county
WIcountymonthpercentdiff <- read.csv("WImonthlystops.csv")

# use this for the state-wide month/year total stop data
WIstatemonthyrstops <- read.csv("WIstopspermonthyr.csv")

# use the csv with monthly stop data by race. this also has total stops per month for the state.
WIracemonthstops <- read.csv("WIracemonthlystops.csv")

#WIracemonthstops <- read.table(file = "WIracemonthlystops.csv", sep = ",", row.names = 1, header = TRUE)

# use the csv with the percent of recorded race variables, by officer, by month
WIofficerracepercent <- read.csv("WIofficerracepercent.csv")

#set up the hover stuff for monthly stops map
WIcountymonthpercentdiff$hover <- with(WIcountymonthpercentdiff, paste(County, '<br>', 
                                                                       "Stops this Month", total_stops, 
                                                                       "Percent Difference from Average Month", percent_diff_stops_than_avg, "<br>"))
# create variable with colnames as choice
month_choice <- (WIcountymonthpercentdiff %>% distinct(month))[,1]

