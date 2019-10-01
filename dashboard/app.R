#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)

## ui.R ##
ui <- dashboardPage(
    dashboardHeader(title = "LUCAS-CA"),
    dashboardSidebar(
        menuItem("About", tabName = "about", icon = icon("dashboard")),
        menuItem("Climate Scenarios", tabName = "climate_scenarios", icon = icon("dashboard")),
        menuItem("Land Use Scenarios", tabName = "landuse_scenarios", icon = icon("dashboard")),
        menuItem("LUCAS Model", tabName = "lucas", icon = icon("dashboard"))
        
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "about",
                    fluidRow(
                        box(h1("This is a title"), p("This is some regular text describing the model."))
                    ))
        )
    )
)

server <- function(input, output) { }

shinyApp(ui, server)
