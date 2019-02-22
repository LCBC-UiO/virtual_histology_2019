#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
height <- 900

# Prep the files containing the tab code ----
traj <- shinyAppFile(appFile = "../R/Shiny_cortical_trajectories.R",
                     options = list(
                       width = "100%", height = height
                     ))

gamms <- shinyAppFile(appFile = "../R/Shiny_GammStats.R",
                      options = list(
                        width = "100%", height = height
                      ))

genes <- shinyAppFile(appFile = "../R/Shiny_genes.R",
                      options = list(
                        width = "100%", height = height
                      ))

go <- shinyAppFile(appFile = "../R/Shiny_GO.R",
                   options = list(
                     width = "100%", height = height
                   ))

vh <- shinyAppFile(appFile = "../R/Shiny_VirtualHistology.R",
                   options = list(
                     width = "100%", height = height
                   ))

# Define UI for application that draws a histogram ----
ui <- fluidPage(
  
  dashboardPage(
    title = "Virtual histology - Supplemental information",
    dashboardHeader(title = "Virtual histology"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Virtual histology", tabName = "vh", icon = icon("dashboard")),
        menuItem("Trajectories", tabName = "traj", icon = icon("chart-line")),
        menuItem("GAMMS", tabName = "gamms", icon = icon("chart-line")),
        menuItem("Genes", tabName = "genes", icon = icon("table")),
        menuItem("Go", tabName = "go", icon = icon("table"))
      ),
      helpText("Created by Didac Vidal Piñeiro")
    ),      
    dashboardBody(
      h2("some text"),
      tabItems(
        tabItem(tabName = "vh", vh),
        tabItem(tabName = "traj", traj),
        tabItem(tabName = "gamms", gamms),
        tabItem(tabName = "genes", genes),
        tabItem(tabName = "go", go)
      ),
      p("some more text")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)

