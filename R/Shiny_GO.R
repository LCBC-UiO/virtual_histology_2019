### Shiny - Virtual Histology ###
# ----------------------------------#
# 1) Select Age
# 2) Display cortical thickening/thinning profile as ggseg
# 3) Display cortical thickening/thinning profile as error bars 
# 4) Display VH results for given age 
# 5) Display Gene - Thinning correlation (plus search thingy)


###### loading data ######
# .......................#
library("shiny"); library("dplyr"); library("magrittr")
library("ggplot2"); library("DT"); library(here)

outdir <- here::here()
load(paste0(outdir, "/data/GOtable2shiny.Rda"))


######## settting up everything ########
# .....................................#
## Prepare table
tbl2plot <- GO.table %>% dplyr::mutate(TermID = as.factor(TermID), 
                                       P = -log10(P),
                                       Term = as.factor(Term)) %>% 
  dplyr::select(-Symbols)


##### main shiny script. VH  ####
ui <- shiny::fluidPage(
  shiny::fluidRow(shiny::titlePanel("GO enrichment analyses")),
  shiny::fluidRow(shiny::h5("This table is inculded for sake of completeness. See Methods and materials to consult the specific analysis", style="color:red")),
  DT::DTOutput('tbl')
)


server <- function(input, output, session) {
  # plot VH   
  output$tbl = DT::renderDT(DT::datatable(tbl2plot, filter = list(position = 'top', plain = T), option = list( pageLength = 15, autoWidth = F)))
}

shiny::shinyApp(ui, server)



