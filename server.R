## server.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(maps)
library(dygraphs)

shinyServer(function(input, output){
  
  #Monthly seasonal stuff
  output$map <- renderPlotly({
    ggplot(WIcountymonthpercentdiff %>% filter(month == input$mnth_selected), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = percent_diff_stops_than_avg)) +
    scale_fill_gradient2(limits=c(-.9, 2.1), high = "red", mid = "white",
                         low = "blue", na.value = "grey50")
  })
  
  output$statemonthbar <- renderPlot({
    ggplot(data = WIstatemonthyrstops, aes(x = year, y = total_stops, fill = month)) + 
      geom_col(position = "dodge") +
      ggtitle("Wisconsin Police Stops by Month/Year") + 
      xlab("Month/Year") + 
      ylab("Number of Stops")
  })
  
  output$maxBox <- renderInfoBox({
    max_value <- max(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$mnth_selected])
    max_county <- 
      WIcountymonthpercentdiff$County[WIcountymonthpercentdiff$percent_diff_stops_than_avg == max_value]
    infoBox(max_county, max_value, icon = icon("hand-o-up"))
  })
  
  output$minBox <- renderInfoBox({
    min_value <- min(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$mnth_selected])
    min_state <-
      WIcountymonthpercentdiff$County[WIcountymonthpercentdiff$percent_diff_stops_than_avg == min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  
  output$avgBox <- renderInfoBox({
    infoBox(paste("AVG. Percent Difference than the Typical Month, Across Counties"),
            mean(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$mnth_selected]),
            icon = icon("calculator"), fill = TRUE)
  })
  
  #Driver Race stuff

  output$raceline <- renderDygraph({
      dygraph(data = WIracemonthstops3, main = "Stops by Race") %>%
      dyRangeSelector() %>% 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = FALSE) %>% 
      dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))
  })
  
  output$officer_raceline <- renderDygraph({
      dygraph(data = WIofficerracepercent, main = "Missing Race Observations by Officer") %>%
        dyRangeSelector() %>% 
        dyHighlight(highlightCircleSize = .2,
                    highlightSeriesBackgroundAlpha = 0.3,
                    hideOnMouseOut = FALSE) %>% 
        dyLegend(show = "never") %>% 
        dyAxis("y", label = "Percentage of Stops with Missing Race Observation/Month")
  })
  
})

