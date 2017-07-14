##ui.R
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(maps)

shinyUI(dashboardPage(
  dashboardHeader(title = "Wisconsin Police Stops: A Dashboard for the People, by the People"),
  dashboardSidebar(
    sidebarUserPanel("Patrick Masi-Phelps",
                      image = "https://nationalzoo.si.edu/sites/default/files/animals/africanlion-005_0.jpg"),
    sidebarMenu(
      # menuItem("Race Data", tabName = "race", icon = icon("map")), #look at icon help page, click on Font Awesome for cool icons
      menuItem("Seasonal Changes", 
               tabName = "seasons", 
               icon = icon("database"))),
    selectizeInput("selected",    #this is the widget you can insert. insert it elsewhere if u want
                   "Select Item to Display",
                   month_choice)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "seasons",
              #info boxes
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
              # gvisGeoChart
              fluidPage(plotlyOutput("map")
              )
      )
    )
  )
))
