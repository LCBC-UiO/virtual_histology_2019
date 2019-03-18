
library(shiny); library(shinydashboard);

# Define UI for application that draws a histogram
shinyUI(
  
  # Define UI for application that draws a histogram ----
  fluidPage(
    
    dashboardPage(
      title = "Virtual histology -Supporting materials",
      dashboardHeader(title = "Virtual histology"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Virtual histology", tabName = "vh", icon = icon("dashboard")),
          menuItem("Trajectories", tabName = "traj", icon = icon("chart-line")),
          menuItem("GAMMS", tabName = "gamms", icon = icon("chart-line")),
          menuItem("Genes", tabName = "genes", icon = icon("table")),
          menuItem("Go", tabName = "go", icon = icon("table"))
        ),
        helpText("Created by Didac Vidal Piñeiro"),
        tags$a(href='https://www.oslobrains.no/',
               tags$img(src='https://www.oslobrains.no/wp-content/uploads/2017/09/LCBC_wide_compact_full.png',width='100%')
        )
      ),      
      dashboardBody(
        h1("Cellular correlates of cortical thinning throughout the lifespan - Supporting materials"),
        tabItems(
          tabItem(tabName = "vh", 
                  fluidPage(
                    fluidRow(
                      titlePanel("Cortical thickening/thinning at a given age"),
                      column(6,
                             selectInput("vh_variable", "Age:", AgeU, multiple = FALSE),
                             textOutput("Age_at"))),
                    fluidRow(plotOutput("plot_vh_geo")),
                    fluidRow(plotOutput("plot_vh_bars")),
                    fluidRow(plotOutput("plot_vh_VH")),
                    DTOutput('vh_tbl')
                  )),
          
          tabItem(tabName = "traj", 
                  fluidPage(
                    fluidRow(titlePanel("Cortical thickness and thickening/thinning trajectories across the lifespan"),
                             selectInput("traj_variable", "Cortical Regions (select one or more):", 
                                         region, multiple = TRUE, selected = "entorhinal")),
                    fluidRow(h5("Error bars represent 95% CI of the trajectories", style="color:red")),
                    fluidRow(plotOutput("plot_traj_cth")),
                    fluidRow(plotOutput("plot_traj_ders"))
                  )),
          
          tabItem(tabName = "gamms", 
                  fluidPage(
                    fluidRow(titlePanel("GAMM model statistics"),
                             column(6,
                                    selectInput("gamms_variable", "Select statistic of interest:", stat, multiple = FALSE))),
                    fluidRow(plotOutput("plot_gamms_geo")),
                    fluidRow(plotOutput("plot_gamms_bars"))
                  )),
          
          tabItem(tabName = "genes", 
                  fluidPage(
                    fluidRow(titlePanel("GO enrichment analyses")),
                    fluidRow(h5("This table is inculded for sake of completeness.", style="color:red")),
                    fluidRow(h5("See Methods and materials to consult the specific analysis, including critical values for cth and age regulation analyses", style="color:red")),
                    
                    DTOutput('genes_tbl')
                  )),
          tabItem(tabName = "go", 
                  fluidPage(
                    fluidRow(titlePanel("GO enrichment analyses")),
                    fluidRow(h5("This table is inculded for sake of completeness. See Methods and materials to consult the specific analysis", style="color:red")),
                    DTOutput('go_tbl')
                  ))
        ),
        p("See Methods and Materials for more information.")
      )
    )
  )
  
)
