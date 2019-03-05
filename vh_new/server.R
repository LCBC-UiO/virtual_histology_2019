#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # VH ----
  # plot brain image
  output$plot_vh_geo <- renderPlot({
    
    ggseg_cth_deriv %>% 
      filter(Age %in% input$vh_variable) %>% 
      ggseg(
        hemisphere = "left", 
        mapping = aes(fill = val), 
        color = "black") +
      scale_fill_gradient2("Thinning (mm/yr)",
                           low = "blue", 
                           high = "red", 
                           mid = "white", 
                           midpoint = 0, 
                           oob = squish)
  })
  
  # plot error bars
  output$plot_vh_bars <- renderPlot({
    vh_long_deriv %>% 
      filter(Age == input$vh_variable) %>% 
      
      ggplot(mapping = aes(x = region, ders, group = region, fill = ders)) +
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
    paste0("Thickening/Thinning profile assessed at age: ",
           as.character(input$vh_variable))
  })
  
  # plot VH   
  output$plot_vh_VH <- renderPlot({
    ggcloud <- cor_ReferencePanel %>% 
      select(CellType, input$vh_variable, y) 
    names(ggcloud) <- c("CellType", "corr", "y")
    
    gglim = c(min(ggcloud$corr)-.01, max(ggcloud$corr)+.01)
    res_Age <- res_tab %>% filter(Age == as.numeric(input$vh_variable))
    
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
    
    ggdensity %>% 
      ggplot(aes(x=x, y=y, group=CellType, fill=CellType)) + 
      geom_area(alpha=.8, color = "black") +
      geom_point(data = ggcloud, aes(x=corr, y=y, group=CellType, fill=CellType)) +  
      geom_area(data=ggshades, fill="black", alpha=.5) +
      geom_segment(data = res_Age, mapping = aes(x = r_corr, xend = r_corr, y = 0, yend = ymean), size = 1.5)  +
      theme_custom() +
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
  
  output$vh_tbl = renderDT(datatable(
    tbl2plot, filter = list(position = 'top', plain = T), 
    option = list( pageLength = 15, autoWidth = F)
  ))
  
  
  # Traj ----
  # plot cth
  output$plot_traj_cth <- renderPlot({
    ggplot(data = traj_long_cth, 
           mapping = aes(Age, 
                         cth, 
                         group = region)) + 
      geom_line() + 
      geom_line(data = traj_long_cth %>% 
                  filter(region %in% input$traj_variable), 
                mapping = aes(color = region),
                size = 1) +
      geom_ribbon(data = traj_long_cth %>% 
                    filter(region %in% input$traj_variable),
                  mapping = aes( ymin = cth-cis, 
                                 ymax = cth+cis, 
                                 fill = region), 
                  alpha = 0.35) +
      theme(axis.text = element_text(size = 16), 
            axis.title = element_text(size = 16, 
                                      face ="bold")) +
      scale_y_continuous(name ="cortical thickness (mm)") 
  })
  
  # plot derivative
  output$plot_traj_ders <- renderPlot({
    ggplot(data = traj_long_deriv, 
           mapping = aes(Age, 
                         ders, 
                         group = region)) + 
      geom_hline(yintercept = 0, 
                 linetype = 4) + 
      geom_line() + 
      geom_line(data = traj_long_deriv %>% 
                  filter(region %in% input$traj_variable), 
                mapping = aes(color = region),
                size = 1) +
      geom_ribbon(data = traj_long_deriv %>% 
                    filter(region %in% input$traj_variable),
                  mapping = aes( ymin = ders-cis, 
                                 ymax = ders+cis, 
                                 fill = region), 
                  alpha = 0.35) +
      theme(axis.text = element_text(size = 16), 
            axis.title = element_text(size = 16, 
                                      face ="bold")) +
      scale_y_continuous(name ="thickening/thinning (mm/yr)") 
  })
  # Gamm ----
  
  output$plot_gamms_geo <- renderPlot({
    gamm.stats %>% 
      filter(key %in% input$gamms_variable) %>% 
    ggseg(hemisphere = "left", 
          mapping = aes(fill = val), 
          color = "black") +
      scale_fill_gradient2("stat",
                           low = "blue", 
                           high = "red")
  })
  
  output$plot_gamms_bars <- renderPlot({
    gamm.stats %>% 
      filter(key %in% input$gamms_variable) %>% 
      ggplot(aes(x = label, y = val, 
                 group = label, fill = val)) +
      geom_hline(yintercept = 0, linetype = 4) +
      geom_col() +
      theme(axis.text.x=element_text(angle = -45, hjust = 0, size = 10))+
      scale_y_continuous(name="stat") +
      scale_fill_gradient2("stat",
                           low = "blue", 
                           high = "red")
  })
  
  # Genes ----
  output$genes_tbl = renderDT(datatable(
    tbl2plot, filter = list(position = 'top', plain = T), 
    option = list( pageLength = 15, autoWidth = F)
  ))
  
  # GO ----
  output$go_tbl = renderDT(datatable(
    tbl2plot, filter = list(position = 'top', plain = T), 
    option = list( pageLength = 15, autoWidth = F)
  ))
  
  
})
