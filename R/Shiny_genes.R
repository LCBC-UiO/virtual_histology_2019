### Shiny - Virtual Histology ###
# ----------------------------------#
# 1) Select Age
# 2) Display cortical thickening/thinning profile as ggseg
# 3) Display cortical thickening/thinning profile as error bars 
# 4) Display VH results for given age 
# 5) Display Gene - Thinning correlation (plus search thingy)


###### loading data ######
# .......................#
library(shiny);library(dplyr);library(magrittr);library(ggplot2);
library(DT); library(here)

outdir <- here::here()
load(paste0(outdir, "/data/GeneTable2Shiny.Rda"))
 
  
######## settting up everything ########
# .....................................#
 ## Prepare table
  tbl2plot <- GeneTable %>% dplyr::select(-c(RegBin.old, RegBin.young))
  names(tbl2plot)[4:5] <- c("Age regulation in old (Z-score)", "Age regulation in young (Z-score)")
  tbl2plot %<>% dplyr::select(1,6,5,2,4,3,7,8) 
                               
  
##### main shiny script. VH  ####
ui <- shiny::fluidPage(
    shiny::fluidRow(shiny::titlePanel("GO enrichment analyses")),
    shiny::fluidRow(shiny::h5("This table is inculded for sake of completeness.", style="color:red")),
    shiny::fluidRow(shiny::h5("See Methods and materials to consult the specific analysis, including critical values for cth and age regulation analyses", style="color:red")),
    
    DT::DTOutput('tbl')
  )
  
  
  server <- function(input, output, session) {
    # plot VH   
    output$tbl = DT::renderDT(DT::datatable(tbl2plot, filter = list(position = 'top', plain = T), option = list( pageLength = 15, autoWidth = F)))
  }
  
  shiny::shinyApp(ui, server)
  
  
  
