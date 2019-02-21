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
library(tidyr);library(stringi);library(scales);library(DT)

outdir <- here::here()
load(paste0(outdir, "/data/GeneTable2Shiny.Rda"))
 
  
######## settting up everything ########
# .....................................#
 ## Prepare table
  tbl2plot <- GeneTable %>% select(-c(RegBin.old, RegBin.young))
  names(tbl2plot)[4:5] <- c("Age regulation in old (Z-score)", "Age regulation in young (Z-score)")
  tbl2plot %<>% select(1,6,5,2,4,3,7,8) 
                               
  
##### main shiny script. VH  ####
ui <- fluidPage(
    fluidRow(titlePanel("GO enrichment analyses")),
    fluidRow(h5("This table is inculded for sake of completeness.", style="color:red")),
    fluidRow(h5("See Methods and materials to consult the specific analysis, including critical values for cth and age regulation analyses", style="color:red")),
    
    DTOutput('tbl')
  )
  
  
  server <- function(input, output, session) {
    # plot VH   
    output$tbl = renderDT(datatable(tbl2plot, filter = list(position = 'top', plain = T), option = list( pageLength = 15, autoWidth = F)))
  }
  
  shinyApp(ui, server)
  
  
  
