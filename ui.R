library(shiny); library(shinydashboard);

# Define UI for application that draws a histogram
shinyUI(
  
  # Define UI for application that draws a histogram ----
  fluidPage(
    
    dashboardPage(
      title = "Virtual histology - Supporting materials",
      dashboardHeader(title = "Virtual histology"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Virtual histology", tabName = "vh", icon = icon("dashboard")),
          menuItem("Trajectories", tabName = "traj", icon = icon("chart-line")),
          menuItem("GAMMS", tabName = "gamms", icon = icon("chart-line")),
          menuItem("Gene trajectories", tabName = "genetraj", icon = icon("chart-line")),
          menuItem("Genes", tabName = "genes", icon = icon("table"))#,
       #   menuItem("Go", tabName = "go", icon = icon("table"))
        ),
        helpText("Created by Didac Vidal Piñeiro"),
        tags$a(href='https://www.oslobrains.no/',
               tags$img(src='https://www.oslobrains.no/wp-content/uploads/2017/09/LCBC_wide_compact_full.png',width='100%')
        )
      ),      
      dashboardBody(
        h1("Cellular correlates of cortical thinning throughout the lifespan - Supporting materials"),
        tabItems(
          
          # VH ----
          tabItem(tabName = "vh", 
                  fluidPage(
                    fluidRow(
                      titlePanel("Cortical thickening/thinning at a given age"),
                      column(6,
                             selectInput("vh_variable", "Age:", AgeU, multiple = FALSE),
                             h4(textOutput("Age_at")))),
                    fluidRow(plotOutput("plot_vh_geo")),
                    fluidRow(plotOutput("plot_vh_bars")),
                    fluidRow(plotOutput("plot_vh_VH")),
                    DTOutput('vh_tbl')
                  )),
          
          # Traj ----
          tabItem(tabName = "traj", 
                  fluidPage(
                    fluidRow(titlePanel("Cortical thickness and thickening/thinning trajectories across the lifespan")),
                    fluidRow(column(6,
                                    selectInput("traj_variable", "Cortical Regions (select one or more):", 
                                                region, multiple = TRUE, selected = "entorhinal")),
                             column(6, plotOutput("plot_traj_brain", height = "200px"))),
                    fluidRow(h5("Error bars represent 95% CI of the trajectories", style="font-weight: bold; color:grey")),
                    fluidRow(plotOutput("plot_traj_cth")),
                    fluidRow(plotOutput("plot_traj_ders"))
                  )),
          
          # Gamms ----
          tabItem(tabName = "gamms", 
                  fluidPage(
                    fluidRow(titlePanel("GAMM model statistics"),
                             column(6, selectInput("gamms_variable", "Select statistic of interest:", 
                                                stat, multiple = FALSE))),
                    fluidRow(plotOutput("plot_gamms_geo")),
                    fluidRow(plotOutput("plot_gamms_bars"))
                  )),
          
          # Gene traj ----
          tabItem(tabName = "genetraj", 
                  fluidPage(
                    fluidRow(titlePanel("Gene Expression trajectories across the lifespan"),
                             column(4,selectInput("Filter", "Filter (expression change age group)", 
                                                  c("None", "Young", "Old"), selected = "None"), offsett = 5),
                             column(4,selectInput("Filter2", "Filter (interregional consistent)", 
                                                  c("None", "Consistent"), selected = "None"), offsett = 9),
                             column(4,selectInput("variable", "Gene (select one or more):", 
                                                  gene_trajs, multiple = TRUE, selected = "A1BG"), offset = 0)),                    fluidRow(h5("Error bars represent 95% CI of the trajectories", style="font-weight: bold; color:grey")),
                    fluidRow(plotOutput("gene_traj_gene")),
                    fluidRow(plotOutput("gene_traj_ders")),
                    fluidRow(h5("main stats from GAMM fittings", style="font-weight: bold; color:grey")),
                    DTOutput('gene_traj_tbl')
                  )),
          
          # Genes ----
          tabItem(tabName = "genes", 
                  fluidPage(
                    fluidRow(titlePanel("GO enrichment analyses")),
                    fluidRow(h5("This table is inculded for sake of completeness.", style="font-weight: bold; color:grey")),
                    fluidRow(h5("See Methods and materials to consult the specific analysis, including critical values for cth and age regulation analyses", style="font-weight: bold; color:grey")),
                    
                    DTOutput('genes_tbl')
                  )),
          
          # Go ----
          tabItem(tabName = "go", 
                  fluidPage(
                    fluidRow(titlePanel("GO enrichment analyses")),
                    fluidRow(h5("This table is inculded for sake of completeness. See Methods and materials to consult the specific analysis", style="font-weight: bold; color:grey")),
                    DTOutput('go_tbl')
                  ))
        ),
        p("See Methods and Materials for more information.")
      )
    )
  )
  
)


