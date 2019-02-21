### Shiny - Cortical Trajectories ###
# ----------------------------------#
#Load GAMM fitting and derivatives from the 34 Desikan left cortical hemisphere ROIs
#Display trajectories along the lifespan
#Display Confidence interval of fitting/derivative if highlighted


###### loading data ######
# -----------------------#
library("shiny"); library("dplyr"); library("magrittr"); 
library("ggplot2"); library(here)

outdir <- here::here()
load(paste0(outdir,"/data/db.Rda" ))
load(paste0(outdir,"/data/varnames.Rda"))

######## Preparing data to plot ########
# .....................................#  
region <- varnames$region %>% 
  strsplit("_") %>% 
  simplify2array()
region <- region[1,]

db <- lapply(db, stats::setNames, nm = varnames$region)

# prepare thickening/thinning database
long.deriv <- db$ders %>% 
  dplyr::mutate(Age = as.numeric(rownames(db$ders))) %>% 
  tidyr::gather(region, ders,-c(Age))  
long.deriv %<>% dplyr::left_join(., db$cis %>% 
                                   dplyr::mutate(Age = as.numeric(rownames(db$ders))) %>% 
                                   tidyr::gather(region,cis,-c(Age))) %>% 
  tidyr::separate(region, c("region")) 

# prepare cth database
long.cth <- db$cth %>% 
  dplyr::mutate(Age = as.numeric(rownames(db$cth))) %>% 
  tidyr::gather(region, cth,-c(Age))  
long.cth %<>% dplyr::left_join(., db$cth.ci %>% 
                                 dplyr::mutate(Age = as.numeric(rownames(db$ders))) %>% 
                                 tidyr::gather(region,cis,-c(Age))) %>% 
  tidyr::separate(region, c("region"))

##### main shiny script ####
# .........................#

ui <- shiny::fluidPage(
  shiny::fluidRow(shiny::titlePanel("Cortical thickness and thickening/thinning trajectories across the lifespan"),
                  shiny::selectInput("variable", "Cortical Regions (select one or more):", region, multiple = TRUE, selected = "entorhinal")),
  shiny::fluidRow(shiny::h5("Error bars represent 95% CI of the trajectories", style="color:red")),
  shiny::fluidRow(shiny::plotOutput("plot.cth")),
  shiny::fluidRow(shiny::plotOutput("plot.ders"))
)



server <- function(input, output, session) {
  # plot cth
  output$plot.cth <- shiny::renderPlot({
    ggplot2::ggplot(data = long.cth, 
                    mapping = ggplot2::aes(Age, 
                                           cth, 
                                           group = region)) + 
      ggplot2::geom_line() + 
      ggplot2::geom_line(data = long.cth %>% 
                           dplyr::filter(region %in% input$variable), 
                         mapping = ggplot2::aes(color = region),
                         size = 1) +
      ggplot2::geom_ribbon(data = long.cth %>% 
                             dplyr::filter(region %in% input$variable),
                           mapping = ggplot2::aes( ymin = cth-cis, 
                                                   ymax = cth+cis, 
                                                   fill = region), 
                           alpha = 0.35) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 16), 
                     axis.title = ggplot2::element_text(size = 16, 
                                                        face ="bold")) +
      ggplot2::scale_y_continuous(name ="cortical thickness (mm)") 
  })
  
  # plot derivative
  output$plot.ders <- shiny::renderPlot({
    ggplot2::ggplot(data = long.deriv, 
                    mapping = ggplot2::aes(Age, 
                                           ders, 
                                           group = region)) + 
      ggplot2::geom_hline(yintercept = 0, 
                          linetype = 4) + 
      ggplot2::geom_line() + 
      ggplot2::geom_line(data = long.deriv %>% 
                           dplyr::filter(region %in% input$variable), 
                         mapping = ggplot2::aes(color = region),
                         size = 1) +
      ggplot2::geom_ribbon(data = long.deriv %>% 
                             dplyr::filter(region %in% input$variable),
                           mapping = ggplot2::aes( ymin = ders-cis, 
                                                   ymax = ders+cis, 
                                                   fill = region), 
                           alpha = 0.35) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 16), 
                     axis.title = ggplot2::element_text(size = 16, 
                                                        face ="bold")) +
      ggplot2::scale_y_continuous(name ="thickening/thinning (mm/yr)") 
  })
}

shiny::shinyApp(ui, server)

