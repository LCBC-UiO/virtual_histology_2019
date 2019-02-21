### Shiny - GammStats ###
# ----------------------------------#
# 1) Select Desire stats
# 2) Display stat as ggseg
# 3) Display stats as col graph


###### loading data ######
# .......................#
library(shiny);library(dplyr);library(magrittr);library(ggplot2);library(tidyr);
library(stringi);library(scales);library(devtools);library(ggseg)

outdir <- here::here()
load(paste0(outdir, "/data/gamm.stats.Rda"))
load(paste0(outdir, "/data/varnames.Rda"))

######## settting up everything ########
# .....................................#

stat <- names(gamm.stats)[-1]
gamm.stats %<>% separate(regions, c("label", "tmp")) %>% 
                mutate(label = paste0("lh_", label))

ggseg_brain <- ggseg()
ggseg_brain$data %<>%  left_join(., gamm.stats)


####### main script ####
# .....................#


ui <- fluidPage(
  fluidRow(titlePanel("GAMM model statistics"),
           column(6,
           selectInput("variable", "Select statistic of interest:", stat, multiple = FALSE))),
  fluidRow(plotOutput("plot.geo")),
  fluidRow(plotOutput("plot.bars"))
)


server <- function(input, output, session) {
  
  output$plot.geo <- renderPlot({
      gp <- ggseg(atlas = ggseg_brain$data,  
                  hemisphere = "left", 
                  mapping = aes(fill = get(input$variable)), 
                  color = "black") +
        scale_fill_gradient2("stat",
                             low = "blue", 
                             high = "red")
      return(gp)  
    })

  output$plot.bars <- renderPlot({
    ggplot(data = gamm.stats, mapping = aes(x = label, y = get(input$variable), group = label, fill = get(input$variable))) +
      geom_hline(yintercept = 0, linetype = 4) +
      geom_col() +
      theme(axis.text.x=element_text(angle = -45, hjust = 0, size = 10))+
      scale_y_continuous(name="stat") +
      scale_fill_gradient2("stat",
                           low = "blue", 
                           high = "red")
  })
  
}

shinyApp(ui, server)
