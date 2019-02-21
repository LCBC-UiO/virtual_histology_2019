### Shiny - Virtual Histology ###
# ----------------------------------#
# 1) Select Age
# 2) Display cortical thickening/thinning profile as ggseg
# 3) Display cortical thickening/thinning profile as error bars 
# 4) Display VH results for given age 
# 5) Display Gene - Thinning correlation (plus search thingy)


###### loading data ######
# .......................#
  packages <- c("shiny", "dplyr", "magrittr", "ggplot2", "tidyr", "stringi", "scales","DT")
  
  outdir <- here::here()
  load(paste0(outdir, "/data/GOtable2shiny.Rda"))
 
  
######## settting up everything ########
# .....................................#
 ## Prepare table
  tbl2plot <- GO.table %>% mutate(TermID = as.factor(TermID), 
                                  P = -log10(P),
                                  Term = as.factor(Term)) %>% 
                              select(-Symbols)
    
  
##### main shiny script. VH  ####
ui <- fluidPage(
    fluidRow(titlePanel("GO enrichment analyses")),
    fluidRow(h5("This table is inculded for sake of completeness. See Methods and materials to consult the specific analysis", style="color:red")),
    DTOutput('tbl')
  )
  
  
  server <- function(input, output, session) {
    # plot VH   
    output$tbl = renderDT(datatable(tbl2plot, filter = list(position = 'top', plain = T), option = list( pageLength = 15, autoWidth = F)))
  }
  
  shinyApp(ui, server)
  
  
  
