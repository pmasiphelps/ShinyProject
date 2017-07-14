## global.R ##

# convert matrix to dataframe
WIcountymonthpercentdiff <- read.csv("WImonthlystops.csv")

#set up the hover stuff
WIcountymonthpercentdiff$hover <- with(WIcountymonthpercentdiff %>% filter(month == input$selected), paste(County, '<br>', 
                                                                       "Stops this MOnth", total_stops, 
                                                                       "Percent Difference from Average Month", percent_diff_stops_than_avg, "<br>"))

# create variable with colnames as choice
month_choice <- (WIcountymonthpercentdiff %>% distinct(month))[,1]

