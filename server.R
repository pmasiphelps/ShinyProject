## server.R ##
library(shiny)
library(plotly)
library(dplyr)
library(maps)

shinyServer(function(input, output){
  
  
  output$map <- renderPlotly({
    ggplot(WIcountymonthpercentdiff %>% filter(month == input$selected), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = percent_diff_stops_than_avg)) +
    scale_fill_gradient2(limits=c(-.9, 2.1), high = "red", mid = "white",
                         low = "blue", na.value = "grey50")
  })
  
  output$maxBox <- renderInfoBox({
    max_value <- max(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$selected])
    max_county <- 
      WIcountymonthpercentdiff$County[WIcountymonthpercentdiff$percent_diff_stops_than_avg == max_value]
    infoBox(max_county, max_value, icon = icon("hand-o-up"))
  })
  
  output$minBox <- renderInfoBox({
    min_value <- min(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$selected])
    min_state <-
      WIcountymonthpercentdiff$County[WIcountymonthpercentdiff$percent_diff_stops_than_avg == min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG. Percent Difference than the Typical Month, Across Counties"),
            mean(WIcountymonthpercentdiff$percent_diff_stops_than_avg[WIcountymonthpercentdiff$month == input$selected]),
            icon = icon("calculator"), fill = TRUE))
})

