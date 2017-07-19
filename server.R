## server.R ##
##Patrick Masi-Phelps##

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
library(dygraphs)

shinyServer(function(input, output){
  
  #geom_polygon map plot of the average amount of stops, grouped by month, 
  #compared to an average month, as a percent difference. Months with more stops
  #than an average month will be redder - months with fewer stops than average 
  #will be bluer. User can toggle the month using a slider.
  
  output$map <- renderPlotly({
    ggplotly(  
      ggplot(WIcountymonthpercentdiff %>% filter(month == input$mnth_selected), 
             aes(x = long, 
                 y = lat, 
                 text = paste("County: ", County, "\n", "Percent Difference this Month: ", percent_diff_stops_than_avg, "%"))) +
      geom_polygon(aes(group = group, 
                       fill = percent_diff_stops_than_avg),
                   color = "black",
                   size = .1) +
      scale_fill_gradient2(name = "Percent Difference",
                           limits=c(-90, 210), high = "red", mid = "white",
                           low = "blue", na.value = "grey50") +
      theme_void(),
      tooltip = c("text"))
  })
  
  #bar graph showing the total number of stops each month, from 2011 - 2015, statewide
  
  output$statemonthbar <- renderPlotly({
    ggplotly(
      ggplot(data = WIstatemonthyrstops, aes(x = year, 
                                             y = total_stops, 
                                             group = month,
                                             fill = month,
                                             text = paste("Month: ", month, "\n", "Total Stops: ", total_stops))) + 
        geom_col(aes(fill = month), position = "dodge") +
        xlab("Month/Year") + 
        ylab("Number of Stops") +
        theme(text = element_text(size=10)),
      tooltip = c("text")
    )
  })
  
  #info box showing the county with the highest percent difference each month - used with the map
  output$maxBox <- renderInfoBox({
    max_value <- max(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$mnth_selected])
    max_county <- 
      WIcountymonthpercentdiff$County[WIcountymonthpercentdiff$percent_diff_stops_than_avg == max_value]
    infoBox(max_county, max_value, icon = icon("hand-o-up"))
  })
  
  #info box showing the county with the lowest percent difference each month - used with the map
  output$minBox <- renderInfoBox({
    min_value <- min(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$mnth_selected])
    min_state <-
      WIcountymonthpercentdiff$County[WIcountymonthpercentdiff$percent_diff_stops_than_avg == min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  
  #info box showing the statewide percent difference compared to an average month 
  
  output$avgBox <- renderInfoBox({
    infoBox(paste("State-wide Percent Difference"),
            round(mean(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$mnth_selected]), 3), 
            fill = TRUE)
  })
  
  #time series of number of stops per month by race of the driver using dygraphs.

  output$raceline <- renderDygraph({
      dygraph(data = WIracemonthstops) %>%
      dyRangeSelector() %>% 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = FALSE) %>% 
      dyAxis("x", label = "Time (Range = January 2011 - December 2015)") %>%
      dyAxis("y", label = "Number of Stops/Month") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))
  })
  
  #heatmap/density plot of number of officers who recorded a certain percentage of race observations each month.
  
  output$officer_raceheatmap <- renderPlot({
      ggplot(data = WIofficerracepercent2, aes(x = yrmth, 
                                               y = percent_missing_race)) + 
        geom_point(alpha = .1) +
        geom_bin2d() +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        xlab("Month/Year") + 
        ylab("Percentage of Stops with Missing Race Observation/Month")
  })
  
})

