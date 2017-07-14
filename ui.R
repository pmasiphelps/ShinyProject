##ui.R
library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarUserPanel("Patrick Masi-Phelps",
                      image = "https://nationalzoo.si.edu/sites/default/files/animals/africanlion-005_0.jpg"),
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")), #look at icon help page, click on Font Awesome for cool icons
      menuItem("Data", tabName = "data", icon = icon("database"))),
    selectizeInput("selected",    #this is the widget you can insert. insert it elsewhere if u want
                   "Select Item to Display",
                   choice) #we made the "choice" variable equal to the columns of the dataset (see global.r), so user can choose which variables to look at
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              #info boxes
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
              # gvisGeoChart
              fluidRow(box(htmlOutput("map"),
                           height = 300),
                       # gvisHistoGram
                       box(htmlOutput("hist"),
                           height = 300))
              ),
      tabItem(tabName = "data",
              # datatable
              fluidRow(box(DT::dataTableOutput("table"),
                           width = 12)))
              )
  )
))
