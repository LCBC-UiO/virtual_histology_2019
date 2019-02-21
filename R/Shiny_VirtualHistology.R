### Shiny - Virtual Histology ###
# ----------------------------------#
# 1) Select Age
# 2) Display cortical thickening/thinning profile as ggseg
# 3) Display cortical thickening/thinning profile as error bars 
# 4) Display VH results for given age 
# 5) Display Gene - Thinning correlation (plus search thingy)


###### loading data ######
# .......................#
  packages <- c("shiny", "dplyr", "magrittr", "ggplot2", "tidyr", "stringi", "scales", "ggridges", "devtools","purrr","astsa", "DT")
  library(ggseg)
  
  outdir <- here::here()
  load(paste0(outdir, "/data/db.Rda" ))
  load(paste0(outdir, "/data/varnames.Rda"))
  load(paste0(outdir, "/data/VH_CorrRefPanel.Rda"))
  load(paste0(outdir, "/data/VH_Lifespan.Rda"))
  load(paste0(outdir, "/data/VH_SigThresh.Rda"))
  
######## settting up everything ########
# .....................................#
  region <- varnames$region %>% 
    strsplit("_") %>% 
    simplify2array()
  region <- region[1,]
  
  
  ## Prepare table
  tbl2plot <- cor_ReferencePanel %>% gather(Age, corr,-c(Gene, CellType, Allen.Average.donor.correlation.to.median, Allen.vs.Brainspan.correlation)) %>% 
    tidyr::separate(Age, c("tmp", "Age") , sep = "expression_phenotype_rX") %>% select(-tmp) %>%
    mutate(Age = as.factor(as.numeric(Age)),
           CellType = as.factor(CellType)) %>% 
          select(Gene, Age, corr, everything())
  names(tbl2plot) <- c("Gene","Age", "CellType","Phenotype-Gene corr", "Allen Average donor corr to median", "Allen-Brainspan corr")
  # Prepare ggseg
  cth.deriv <- data.frame(t(db$ders)) 
  names(cth.deriv) %<>% strsplit("X") %>% simplify2array() %>% .[2,]
  #names(cth.deriv) %<>% gsub("X","Age",.)
  AgeU <- names(cth.deriv)
  cth.deriv %<>%   mutate(label = paste0("lh_", region))
  
  ggseg_brain <- ggseg()
  ggseg_brain$data %<>%  left_join(., cth.deriv)
  
  # prepare thinning bar plotting
  db %<>% lapply(., setNames, nm = varnames$region)
  
  long.deriv <- db$ders %>% 
    mutate(Age = as.numeric(rownames(db$ders))) %>% 
    gather(region, ders,-c(Age))  
  long.deriv %<>% left_join(., db$cis %>% 
                              mutate(Age = as.numeric(rownames(db$ders))) %>% 
                              gather(region,cis,-c(Age))) %>% 
                              separate(region, c("region")) 
  gglim <- c(min(long.deriv$ders)-max(long.deriv$cis),max(long.deriv$ders)+max(long.deriv$cis))  

  # prepare VH data
  names(cor_ReferencePanel) %<>% gsub("expression_phenotype_rX","",.)
  names(sigp)[2] <- "ci"
  names(res_tab)[1] <- "CellType"
  res_tab %<>% left_join(., sigp, by = c("CellType" = "celltypes"))
  cor_ReferencePanel %<>% filter(!is.na(CellType)) 
  cor_ReferencePanel %<>% mutate(y = runif(nrow(cor_ReferencePanel[1]), -.5,-.1))
  
  
##### main shiny script. VH  ####
ui <- fluidPage(
    fluidRow(titlePanel("Cortical thickening/thinning at a given age"),
             column(6,
                    selectInput("variable", "Age:", AgeU, multiple = FALSE),
                    textOutput("Age_at"))),
    fluidRow(plotOutput("plot.geo")),
    fluidRow(plotOutput("plot.bars")),
    fluidRow(plotOutput("plot.VH")),
    DTOutput('tbl')
  )
  
  
  server <- function(input, output, session) {
    
    # plot brain image
    output$plot.geo <- renderPlot({
      gp <- ggseg(atlas = ggseg_brain$data,  
                  hemisphere = "left", 
                  mapping = aes(fill = get(input$variable)), 
                  color = "black") +
            scale_fill_gradient2("Thinning (mm/yr)",
                                 low = "blue", 
                                 high = "red", 
                                 mid = "white", 
                                 midpoint = 0, 
                                 oob = squish)
      return(gp)  
    })
    
    # plot error bars
    output$plot.bars <- renderPlot({
      l <- long.deriv %>% filter(Age == input$variable)
        ggplot(data = l, mapping = aes(x = region, ders, group = region, fill = ders)) +
          geom_hline(yintercept = 0, linetype = 4) +
          geom_col() +
          geom_errorbar(aes(ymin =ders-cis, ymax =  ders+cis)) +
          scale_x_discrete(labels = region) +
          theme(axis.text.x=element_text(angle = -45, hjust = 0, size = 10),
                legend.position = 'none')+
          ylim(gglim) +
          scale_fill_gradient2("Thinning (mm/yr)",
                               low = "blue", 
                               high = "red", 
                               mid = "white", 
                               midpoint = 0, 
                               oob = squish) +
          ylab("Thickening/Thinning (mm/yr)")
    })
   
    # plot text
    output$Age_at <- renderText({
      paste0("Thickening/Thinning profile assessed at age: ",as.character(input$variable))
    })
    
    # plot VH   
    output$plot.VH <- renderPlot({
      ggcloud <- cor_ReferencePanel %>% select(CellType, input$variable, y) 
      names(ggcloud) <- c("CellType", "corr", "y")
      gglim = c(min(ggcloud$corr)-.01, max(ggcloud$corr)+.01)
      res_Age <- res_tab %>% filter(Age == as.numeric(input$variable))
      ggdensity <- ggcloud %>% 
        group_by(CellType) %>% 
        nest() %>% 
        mutate(data = map(data, ~with(density(.$corr), data.frame(x,y)))) %>% 
        unnest()
      ggshades <- data.frame()
      for(i in unique(res_tab$CellType)){
        # get paramters of interest
        tmp = res_Age %>% filter(CellType %in% i)
        tmp2 <- ggdensity %>% filter(CellType == i)
        ggshades <- rbind(ggshades, ggdensity %>% filter(CellType ==i&abs(x)<tmp$ci))
        ymean <- tmp2$y[which.min(abs(tmp2$x - tmp$r_corr ))]
        res_Age[which(res_Age$CellType == i),"ymean"] <- ymean
      }
      ggplot(data = ggdensity, aes(x=x, y=y, group=CellType, fill=CellType)) + 
        geom_area(alpha=.8, color = "black") +
        geom_point(data = ggcloud, aes(x=corr, y=y, group=CellType, fill=CellType)) +  
        geom_area(data=ggshades, fill="black", alpha=.5) +
        geom_segment(data = res_Age, mapping = aes(x = r_corr, xend = r_corr, y = 0, yend = ymean), size = 1.5)  +
        theme_ridges() +
        theme(legend.position = 'none', 
              panel.background = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", angle = 0, vjust = 1.1),
              axis.title.y = element_text(vjust = 1.5, hjust = 0.5, margin = margin(0,2.2,0,0)),
              axis.title.x = element_text(vjust = 0, hjust = 0.5, margin = margin(0,0,2.2,0)),
              plot.title = element_text(vjust = 2,hjust =.5, face = "bold", size = 20))+
        scale_x_continuous(name = "Correlation coefficient (r)", limits = gglim,
                           breaks= seq(-.6,.6,by = .6)) +
        scale_y_continuous(name = "Density", breaks= seq(0,1.5,by = .5)) +
        facet_wrap(CellType~.,scales="fixed", nrow = 1,strip.position = "top")   
    })
      output$tbl = renderDT(datatable(tbl2plot, filter = list(position = 'top', plain = T), option = list( pageLength = 15, autoWidth = F)))
      
  }
  
  shinyApp(ui, server)
