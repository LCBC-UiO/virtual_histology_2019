### Shiny - GammStats ###
# ----------------------------------#
# 1) Select Desire stats
# 2) Display stat as ggseg
# 3) Display stats as col graph


###### loading data ######
# .......................#
library(shiny);library(dplyr);library(magrittr);library(ggplot2);library(tidyr);
library(ggseg); library(here)

outdir <- here::here()
load(paste0(outdir, "/data/gamm.stats.Rda"))
load(paste0(outdir, "/data/varnames.Rda"))

######## settting up everything ########
# .....................................#

stat <- names(gamm.stats)[-1]
gamm.stats %<>% tidyr::separate(regions, c("label", "tmp")) %>% 
                dplyr::mutate(label = paste0("lh_", label))

ggseg_brain <- ggseg::ggseg()
ggseg_brain$data %<>%  dplyr::left_join(., gamm.stats)


####### main script ####
# .....................#


ui <- shiny::fluidPage(
  shiny::fluidRow(shiny::titlePanel("GAMM model statistics"),
           shiny::column(6,
           shiny::selectInput("variable", "Select statistic of interest:", stat, multiple = FALSE))),
  shiny::fluidRow(shiny::plotOutput("plot.geo")),
  shiny::fluidRow(shiny::plotOutput("plot.bars"))
)


server <- function(input, output, session) {
  
  output$plot.geo <- shiny::renderPlot({
      gp <- ggseg::ggseg(atlas = ggseg_brain$data,  
                  hemisphere = "left", 
                  mapping = ggplot2::aes(fill = get(input$variable)), 
                  color = "black") +
        ggplot2::scale_fill_gradient2("stat",
                             low = "blue", 
                             high = "red")
      return(gp)  
    })

  output$plot.bars <- shiny::renderPlot({
    ggplot2::ggplot(data = gamm.stats, mapping = ggplot2::aes(x = label, y = get(input$variable), group = label, fill = get(input$variable))) +
      ggplot2::geom_hline(yintercept = 0, linetype = 4) +
      ggplot2::geom_col() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -45, hjust = 0, size = 10))+
      ggplot2::scale_y_continuous(name="stat") +
      ggplot2::scale_fill_gradient2("stat",
                           low = "blue", 
                           high = "red")
  })
  
}

shiny::shinyApp(ui, server)
