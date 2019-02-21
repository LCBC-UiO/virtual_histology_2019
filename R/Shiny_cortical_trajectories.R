### Shiny - Cortical Trajectories ###
# ----------------------------------#
#Load GAMM fitting and derivatives from the 34 Desikan left cortical hemisphere ROIs
#Display trajectories along the lifespan
#Display Confidence interval of fitting/derivative if highlighted


###### loading data ######
# -----------------------#
library("shiny"); library("dplyr"); library("magrittr"); 
library("ggplot2"); library("tidyr")
  
  outdir <- here::here()
  load(paste0(outdir,"/data/db.Rda" ))
  load(paste0(outdir,"/data/varnames.Rda"))

######## Preparing data to plot ########
# .....................................#  
  region <- varnames$region %>% 
                strsplit("_") %>% 
                simplify2array()
  region <- region[1,]
  
  db <- lapply(db, setNames, nm = varnames$region)
  
  # prepare thickening/thinning database
  long.deriv <- db$ders %>% 
                    mutate(Age = as.numeric(rownames(db$ders))) %>% 
                    gather(region, ders,-c(Age))  
  long.deriv %<>% left_join(., db$cis %>% 
                                 mutate(Age = as.numeric(rownames(db$ders))) %>% 
                                 gather(region,cis,-c(Age))) %>% 
                                 separate(region, c("region")) 
  
  # prepare cth database
  long.cth <- db$cth %>% 
                    mutate(Age = as.numeric(rownames(db$cth))) %>% 
                    gather(region, cth,-c(Age))  
  long.cth %<>% left_join(., db$cth.ci %>% 
                          mutate(Age = as.numeric(rownames(db$ders))) %>% 
                          gather(region,cis,-c(Age))) %>% 
                          separate(region, c("region"))
  
##### main shiny script ####
# .........................#
  
  ui <- fluidPage(
    fluidRow(titlePanel("Cortical thickness and thickening/thinning trajectories across the lifespan"),
             selectInput("variable", "Cortical Regions (select one or more):", region, multiple = TRUE, selected = "entorhinal")),
    fluidRow(h5("Error bars represent 95% CI of the trajectories", style="color:red")),
    fluidRow(plotOutput("plot.cth")),
    fluidRow(plotOutput("plot.ders"))
    )
      
  
  server <- function(input, output, session) {
    # plot cth
    output$plot.cth <- renderPlot({
      ggplot(data = long.cth, 
             mapping = aes(Age, 
                           cth, 
                           group = region)) + 
        geom_line() + 
        geom_line(data = long.cth %>% 
                    filter(region %in% input$variable), 
                    mapping = aes(Age, 
                                  cth, 
                                  group = region, 
                                  color = region),
                    size = 1) +
        geom_ribbon(data = long.cth %>% 
                    filter(region %in% input$variable),
                    mapping = aes( ymin = cth-cis, 
                                   ymax = cth+cis, 
                                   group = region, 
                                   fill = region), 
                    alpha = 0.35) +
        theme(axis.text = element_text(size = 16), 
              axis.title = element_text(size = 16, 
                                              face ="bold")) +
        scale_y_continuous(name ="cortical thickness (mm)") 
    })
    
    # plot derivative
    output$plot.ders <- renderPlot({
      ggplot(data = long.deriv, 
             mapping = aes(Age, 
                           ders, 
                           group = region)) + 
        geom_hline(yintercept = 0, 
                   linetype = 4) + 
        geom_line() + 
        geom_line(data = long.deriv %>% 
                   filter(region %in% input$variable), 
                  mapping = aes(Age, 
                                ders, 
                                group = region, 
                                color = region),
                  size = 1) +
        geom_ribbon(data = long.deriv %>% 
                      filter(region %in% input$variable),
                      mapping = aes( ymin = ders-cis, 
                                     ymax = ders+cis, 
                                     group = region, 
                                     fill = region), 
                    alpha = 0.35) +
        theme(axis.text = element_text(size = 16), 
              axis.title = element_text(size = 16, 
                                        face ="bold")) +
        scale_y_continuous(name ="thickening/thinning (mm/yr)") 
    })
  }
  
  shinyApp(ui, server)

