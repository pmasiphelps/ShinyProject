##ui.R
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(maps)

shinyUI(dashboardPage(
  dashboardHeader(title = "Wisconsin Police Stops"),
  dashboardSidebar(
    sidebarUserPanel("Patrick Masi-Phelps",
                      image = "https://nationalzoo.si.edu/sites/default/files/animals/africanlion-005_0.jpg"),
    sidebarMenu(
      # menuItem("Race Data", tabName = "race", icon = icon("map")), #look at icon help page, click on Font Awesome for cool icons
      menuItem("Seasonal Changes", 
               tabName = "seasons", 
               icon = icon("database"),
               menuSubItem("Stops by County Map", tabName = "seasons_counties"),
               menuSubItem("Stops by Month: Statewide", tabName = "seasons_state")
               ),
      menuItem("Driver Racial Data", 
               tabName = "race", 
               icon = icon("map"),
               menuSubItem("Stops by Driver Race", tabName = "race_timeseries"),
               menuSubItem("Missing Race Observations by Officer", tabName = "officer_race")
               )
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "seasons_counties",
              #info boxes
              fluidRow(sliderInput("mnth_selected",    #this is the widget you can insert. insert it elsewhere if u want
                          "Select Month to Display",
                          month_choice, 
                          value = 1, max = 12, min = 1, 
                          step = 1, ticks = FALSE, animate = TRUE)),
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
              # gvisGeoChart
              fluidPage(plotlyOutput("map")
              )
      ),
      tabItem(tabName = "seasons_state",
              h2("Stops by Month"),
              plotOutput("statemonthbar")
      ),
      tabItem(tabName = "race_timeseries",
              h2("Time Series: Stops by Race"),
              fluidRow(
                box(width = 12,
                    dygraphOutput("raceline"))
              )
      ),
      tabItem(tabName = "officer_race",
              h2("Time Series: Missing Race Observations by Officer"),
              fluidRow(
                box(width = 12,
                    dygraphOutput("officer_raceline"))
              )
    )
  )
)))
