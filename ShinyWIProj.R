#RShiny Project
#Connect to RShiny hosting account - https://www.shinyapps.io/admin/#/dashboard
rsconnect::setAccountInfo(name='pmasiphelps', token='97B09C90186FFB5822F2B3EFE953E00A', secret='h8zXl01tUOwxGmYA5bVBRCU1RSM/d0WlgvjGAolJ')


#packages
library(dplyr)
library(tidyverse)
library(lubridate)

#Data Cleaning
WIstops <- read.csv("/Users/Patrick/Documents/ShinyProject/WI-clean.csv")

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

#actual race data stuff
WIstops3$yrmnth <- paste(WIstops3$year, WIstops3$month)

WIstops3$yearmonth <- lapply(WIstops3$yrmnth, function(x) as.yearmon(as.numeric(x)))
                             
WIstops20 <- WIstops3 %>% group_by(yrmnth, driver_race) %>% summarise(total_stops = n())

#USE THIS RACE BAR GRAP
WIraceyearbar1 <- ggplot(data = WIracemonthstops, aes(x = yrmnth, y = total_stops, fill = driver_race)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops by Race/Year") + 
  scale_x_discrete(breaks = c("2011 01", "2011 04", "2011 07", "2011 10", 
                              "2012 01", "2012 04", "2012 07", "2012 10", 
                              "2013 01", "2013 04", "2013 07", "2013 10", 
                              "2014 01", "2014 04", "2014 07", "2014 10", 
                              "2015 01", "2015 04", "2015 07", "2015 10"),
                   labels = c("Jan - 2011", "Apr - 2011", "Jul - 2011", "Oct - 2011", 
                              "Jan - 2012", "Apr - 2012", "Jul - 2012", "Oct - 2012",
                              "Jan - 2013", "Apr - 2013", "Jul - 2013", "Oct - 2013",
                              "Jan - 2014", "Apr - 2014", "Jul - 2014", "Oct - 2014",
                              "Jan - 2015", "Apr - 2015", "Jul - 2015", "Oct - 2015")) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  xlab("Year") + 
  ylab("Number of Stops") +
  facet_wrap( ~ driver_race)

#ALT RACE LINE GRAPH
WIraceyearline1 <- plot_ly(WIracemonthstops, x = ~yrmnth, 
                           y = ~total_stops, 
                           color = ~driver_race,
                           type = 'scatter', 
                           mode = 'lines')
#RaceLInegraph in DYgraph
WIracemonthstops2 <- WIracemonthstops
#turn the date column into date type
WIracemonthstops2$yrmnthd <- as.Date(paste(WIracemonthstops2$yrmnth, "01", sep = ""), "%Y %m %d")
WIracemonthstops2 <- spread(WIracemonthstops2, key = driver_race, value = total_stops)

WIracemonthstops3 <- xts(WIracemonthstops2[,2:7], order.by = WIracemonthstops2$yrmnth)
write.csv(WIracemonthstops3, row.names=TRUE, file = "WIracemonthlystops.csv")

#THIS IS THE ONE the graph......
WIraceyearline2 <- dygraph(data = WIracemonthstops3, main = "Stops by Race") %>%
  dyRangeSelector()

#USE THIS PIE CHART
#add column showing percent of stops in month for each race
WIstops21 <- WIstops20 %>% group_by(yrmnth) %>% 
  summarize(total_stops_all_races = sum(total_stops))

WIstops22 <- merge(WIstops20, WIstops21, by = "yrmnth")
WIstops22 <- WIstops22 %>% mutate(percent_of_stops = total_stops/total_stops_all_races)

write.csv(WIstops22, file = "WIracemonthlystops.csv", row.names=TRUE)
WIracemonthstops <- WIstops22

#OK HERE"S THE PIE CHART
WIracemonthpie <- ggplot(WIracemonthstops, aes(x = "", 
                                        y = percent_of_stops, 
                                        fill = driver_race)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)



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

WIstops8$yrmth <- as.Date(paste(WIstops8$yrmth, "01", sep = "/"), "%Y/%m/%d")

WIstops8 <- spread(WIstops8, key = officer_id, value = percent_missing_race)

WIstops9 <- WIstops8
WIstops9[is.na(WIstops9)] <- 0

WIofficerracepercent <- xts(WIstops9[,2:545], order.by = WIstops9$yrmth)

#dygraph of officer stuff
officer_dygraph <- dygraph(data = WIofficerracepercent, main = "Missing Race Observations by Officer") %>%
  dyRangeSelector() %>% 
  dyHighlight(highlightCircleSize = .2,
              highlightSeriesBackgroundAlpha = 0.3,
              hideOnMouseOut = FALSE) %>% 
  dyLegend(show = "never") %>% 
  dyAxis("y", label = "Percentage of Stops with Missing Race Observation/Month")

write.csv(WIofficerracepercent, file = "WIofficerracepercent.csv", row.names=TRUE)


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


#seasonality 
#stops per month/year
WIstops3$yrmth <- paste(WIstops3$year, WIstops3$month, sep = "/")
WIstops4 <- WIstops3 %>% group_by(year, month) %>% summarize(total_stops = n())

write.csv(WIstops4, file = "WIstopspermonthyr.csv")


WImonthyearbar <- ggplot(data = WIstatemonthyrstops, aes(x = year, y = total_stops, fill = month)) + 
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
WIstops13 <- WIstops3 %>% group_by(location_raw) %>% summarize(stops_per_month = n()/60)
WIstops14 <- merge(WIstops3, WIstops13, by = "location_raw")

WIstops15 <- WIstops3 %>% group_by(month) %>% summarize(n())

#get the percent difference between each county's stops in a specific month (across 2011-2015) compared to an average month
WIstops16 <- WIstops14 %>% group_by(location_raw, stops_per_month, month) %>% 
  summarize(total_stops = n()) %>% 
  mutate(avg_stops = total_stops/5, 
         percent_diff_stops_than_avg = (avg_stops-stops_per_month)/stops_per_month)

#bar chart of the above
WIcountymonthpercentdiffbar <- ggplot(data = WIstops16, aes(x = month, y = percent_diff_stops_than_avg, fill = month)) + 
  geom_col(position = "dodge") +
  ggtitle("Wisconsin Police Stops: Percent Difference from Avg Month") + 
  xlab("Month") + 
  ylab("Percent Difference from Average Month") +
  facet_wrap( ~ location_raw)

#getting county data
counties = map_data("county")
WIcounties <- counties %>% filter(region == "wisconsin")
ggplot(data = WIcounties, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = group))

WIcounties$subregion <- toupper(WIcounties$subregion)
WIcounties$subregion[WIcounties$subregion == "ST CROIX"] <- "ST. CROIX"

colnames(WIstops16)[1] <- "County"
colnames(WIcounties)[6] <- "County"

#merging
WIcountiesmonthsstopspercentdiff <- merge(WIstops16, WIcounties, by = "County")

WIcountiesmonthsstopspercentdiff <- WIcountiesmonthsstopspercentdiff %>% arrange(group, order, month)
#graphing monthly diffs by county - map
#test map for may (month = 5)
WIcountiesmonthsstopspercentdiff_may <- WIcountiesmonthsstopspercentdiff %>% filter(month == "05")

#test map for december (month = 12)
WIcountiesmonthsstopspercentdiff_dec <- WIcountiesmonthsstopspercentdiff %>% filter(month == "12")

WIcountiesmonthsstopspercentdiff2 <- WIcountiesmonthsstopspercentdiff

WIcountiesmonthsstopspercentdiff2$month <- lapply(WIcountiesmonthsstopspercentdiff2$month, function(x) as.month(as.numeric(x)))


WIstops$stop_date <- format(as.Date(WIstops$stop_date), "%Y/%m/%d") 
WIstops3$yearmonth <- lapply(WIstops3$yrmnth, function(x) as.yearmon(as.numeric(x)))


wistopsmonthsmap <- ggplot(WIcountiesmonthsstopspercentdiff_dec, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = percent_diff_stops_than_avg))

WIcountiesmonthsstopspercentdiff <- WIcountiesmonthsstopspercentdiff %>% filter(County != "MENOMINEE")

write.csv(WIcountiesmonthsstopspercentdiff, file = "WImonthlystops.csv", row.names=FALSE)

