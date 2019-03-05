### Shiny - Virtual Histology ###
# ----------------------------------#
# 1) Select Age
# 2) Display cortical thickening/thinning profile as ggseg
# 3) Display cortical thickening/thinning profile as error bars 
# 4) Display VH results for given age 
# 5) Display Gene - Thinning correlation (plus search thingy)


###### loading data ######
# .......................#
library(shiny); library(dplyr); library(magrittr); 
library(ggplot2); library(tidyr); library(scales); 
library(ggridges); library(purrr); library(DT)
library(ggseg); library(here)

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
tbl2plot <- cor_ReferencePanel %>% tidyr::gather(Age, corr,-c(Gene, CellType, Allen.Average.donor.correlation.to.median, Allen.vs.Brainspan.correlation)) %>% 
  tidyr::separate(Age, c("tmp", "Age") , sep = "expression_phenotype_rX") %>% dplyr::select(-tmp) %>%
  dplyr::mutate(Age = as.factor(as.numeric(Age)),
                CellType = as.factor(CellType)) %>% 
  dplyr::select(Gene, Age, corr, dplyr::everything())
names(tbl2plot) <- c("Gene","Age", "CellType","Phenotype-Gene corr", "Allen Average donor corr to median", "Allen-Brainspan corr")
# Prepare ggseg
cth.deriv <- data.frame(t(db$ders)) 
names(cth.deriv) %<>% strsplit("X") %>% simplify2array() %>% .[2,]
#names(cth.deriv) %<>% gsub("X","Age",.)
AgeU <- names(cth.deriv)
cth.deriv %<>%   dplyr::mutate(label = paste0("lh_", region))

ggseg_brain <- ggseg::ggseg()
ggseg_brain$data %<>%  dplyr::left_join(., cth.deriv)
cth.deriv <-  %>% gather(Age, val, -label)

# prepare thinning bar plotting
db %<>% lapply(., stats::setNames, nm = varnames$region)

long.deriv <- db$ders %>% 
  dplyr::mutate(Age = as.numeric(rownames(db$ders))) %>% 
  tidyr::gather(region, ders,-c(Age))  
long.deriv %<>% dplyr::left_join(., db$cis %>% 
                                   dplyr::mutate(Age = as.numeric(rownames(db$ders))) %>% 
                                   tidyr::gather(region,cis,-c(Age))) %>% 
  tidyr::separate(region, c("region")) 
gglim <- c(min(long.deriv$ders)-max(long.deriv$cis),max(long.deriv$ders)+max(long.deriv$cis))  

# prepare VH data
names(cor_ReferencePanel) %<>% gsub("expression_phenotype_rX","",.)
names(sigp)[2] <- "ci"
names(res_tab)[1] <- "CellType"
res_tab %<>% dplyr::left_join(., sigp, by = c("CellType" = "celltypes"))
cor_ReferencePanel %<>% dplyr::filter(!is.na(CellType)) 
cor_ReferencePanel %<>% dplyr::mutate(y = stats::runif(nrow(cor_ReferencePanel[1]), -.5,-.1))


##### main shiny script. VH  ####
ui <- shiny::fluidPage(
  shiny::fluidRow(shiny::titlePanel("Cortical thickening/thinning at a given age"),
                  shiny::column(6,
                                shiny::selectInput("variable", "Age:", AgeU, multiple = FALSE),
                                shiny::textOutput("Age_at"))),
  shiny::fluidRow(shiny::plotOutput("plot.geo")),
  shiny::fluidRow(shiny::plotOutput("plot.bars")),
  shiny::fluidRow(shiny::plotOutput("plot.VH")),
  DT::DTOutput('tbl')
)


server <- function(input, output, session) {
  
  # plot brain image
  output$plot.geo <- shiny::renderPlot({
    gp <- ggseg::ggseg(atlas = ggseg_brain$data,  
                       hemisphere = "left", 
                       mapping = ggplot2::aes(fill = get(input$variable)), 
                       color = "black") +
      ggplot2::scale_fill_gradient2("Thinning (mm/yr)",
                                    low = "blue", 
                                    high = "red", 
                                    mid = "white", 
                                    midpoint = 0, 
                                    oob = scales::squish)
    return(gp)  
  })
  
  # plot error bars
  output$plot.bars <- shiny::renderPlot({
    l <- long.deriv %>% dplyr::filter(Age == input$variable)
    ggplot2::ggplot(data = l, mapping = ggplot2::aes(x = region, ders, group = region, fill = ders)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 4) +
      ggplot2::geom_col() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin =ders-cis, ymax =  ders+cis)) +
      ggplot2::scale_x_discrete(labels = region) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -45, hjust = 0, size = 10),
                     legend.position = 'none')+
      ggplot2::ylim(gglim) +
      ggplot2::scale_fill_gradient2("Thinning (mm/yr)",
                                    low = "blue", 
                                    high = "red", 
                                    mid = "white", 
                                    midpoint = 0, 
                                    oob = scales::squish) +
      ggplot2::ylab("Thickening/Thinning (mm/yr)")
  })
  
  # plot text
  output$Age_at <- shiny::renderText({
    paste0("Thickening/Thinning profile assessed at age: ",as.character(input$variable))
  })
  
  # plot VH   
  output$plot.VH <- shiny::renderPlot({
    ggcloud <- cor_ReferencePanel %>% dplyr::select(CellType, input$variable, y) 
    names(ggcloud) <- c("CellType", "corr", "y")
    gglim = c(min(ggcloud$corr)-.01, max(ggcloud$corr)+.01)
    res_Age <- res_tab %>% dplyr::filter(Age == as.numeric(input$variable))
    ggdensity <- ggcloud %>% 
      dplyr::group_by(CellType) %>% 
      tidyr::nest() %>% 
      dplyr::mutate(data = purrr::map(data, ~with(stats::density(.$corr), data.frame(x,y)))) %>% 
      tidyr::unnest()
    ggshades <- data.frame()
    for(i in unique(res_tab$CellType)){
      # get paramters of interest
      tmp = res_Age %>% dplyr::filter(CellType %in% i)
      tmp2 <- ggdensity %>% dplyr::filter(CellType == i)
      ggshades <- rbind(ggshades, ggdensity %>% dplyr::filter(CellType ==i&abs(x)<tmp$ci))
      ymean <- tmp2$y[which.min(abs(tmp2$x - tmp$r_corr ))]
      res_Age[which(res_Age$CellType == i),"ymean"] <- ymean
    }
    ggplot2::ggplot(data = ggdensity, ggplot2::aes(x=x, y=y, group=CellType, fill=CellType)) + 
      ggplot2::geom_area(alpha=.8, color = "black") +
      ggplot2::geom_point(data = ggcloud, ggplot2::aes(x=corr, y=y, group=CellType, fill=CellType)) +  
      ggplot2::geom_area(data=ggshades, fill="black", alpha=.5) +
      ggplot2::geom_segment(data = res_Age, mapping = ggplot2::aes(x = r_corr, xend = r_corr, y = 0, yend = ymean), size = 1.5)  +
      ggridges::theme_ridges() +
      ggplot2::theme(legend.position = 'none', 
                     panel.background = ggplot2::element_blank(),
                     strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_text(face = "bold", angle = 0, vjust = 1.1),
                     axis.title.y = ggplot2::element_text(vjust = 1.5, hjust = 0.5, margin = ggplot2::margin(0,2.2,0,0)),
                     axis.title.x = ggplot2::element_text(vjust = 0, hjust = 0.5, margin = ggplot2::margin(0,0,2.2,0)),
                     plot.title = ggplot2::element_text(vjust = 2,hjust =.5, face = "bold", size = 20))+
      ggplot2::scale_x_continuous(name = "Correlation coefficient (r)", limits = gglim,
                                  breaks= seq(-.6,.6,by = .6)) +
      ggplot2::scale_y_continuous(name = "Density", breaks= seq(0,1.5,by = .5)) +
      ggplot2::facet_wrap(CellType~.,scales="fixed", nrow = 1,strip.position = "top")   
  })
  output$tbl = DT::renderDT(DT::datatable(tbl2plot, filter = list(position = 'top', plain = T), option = list( pageLength = 15, autoWidth = F)))
  
}

shiny::shinyApp(ui, server)
