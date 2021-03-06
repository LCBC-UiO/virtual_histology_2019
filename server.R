#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
      unnest(cols = c(data))
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
    tbl2plot_vh, filter = list(position = 'top', plain = T), 
    option = list( pageLength = 15, autoWidth = F), 
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
      scale_y_continuous(name ="cortical thickness (mm)",
                         labels = scales::number_format(accuracy = 0.01)) 
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
      scale_y_continuous(name ="thickening/thinning (mm/yr)",
                         labels = scales::number_format(accuracy = 0.01)) 
  })
  
  output$plot_traj_brain <- renderPlot({
    ggseg::dkt %>% 
      filter(label %in% paste0("lh_", input$traj_variable)) %>% 
      mutate(region = gsub("lh_", "", label)) %>% 
      ggseg(mapping=aes(fill=region), hemisphere = "left")
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
  
  # Gene trajectories ----
  gene_traj_i <- reactive({
    which(gene_traj_db$gene %in% input$variable)
  })
  
  gene_traj_plot <- eventReactive(input$variable, {
    data.frame(deriv = as.vector(t(gene_traj_db$deriv[gene_traj_i(),])), 
               se.deriv = as.vector(t(gene_traj_db$se_deriv[gene_traj_i(),])), 
               fit = as.vector(t(gene_traj_db$fit[gene_traj_i(),])), 
               se.fit = as.vector(t(gene_traj_db$se_fit[gene_traj_i(),])),
               gene = rep(gene_traj_db$gene[gene_traj_i()], each = 100), 
               age = rep(gene_traj_db$age,length(gene_traj_i())))
  })
  
  gene_traj_table <- eventReactive(input$variable, {
    gene_traj_db$stats %>% 
      dplyr::filter(gene %in% input$variable) %>% 
      as_data_frame() %>% 
      mutate(value = round(value, 2)) %>% 
      tidyr::spread(stats, value)
  })
  
  
  output$gene_traj_gene <- renderPlot({
    ggplot(data = gene_traj_plot(), 
           mapping = aes(x = age, 
                         y = fit, 
                         group = gene,
                         fill = gene, 
                         color = gene)) + 
      #geom_hline(yintercept = 0, color = "red", linetype = 4, size = 2) +
      geom_ribbon(mapping = aes(ymin = fit-1.96*se.fit, 
                                ymax = fit+1.96*se.fit),
                  alpha = .35) +
      geom_line(size = 2) + 
      theme_classic() +
      theme(#legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))+
      ylab("Trajectory (Z-norm.)") +
      xlab("Age") +
      scale_y_continuous(
        labels = scales::number_format(accuracy = 0.01))
  })
  
  # plot derivative
  output$gene_traj_ders <- renderPlot({
    ggplot(data = gene_traj_plot(), 
           mapping = aes(x = age, 
                         y = deriv, 
                         group = gene,
                         fill = gene, 
                         color = gene)) + 
      geom_hline(yintercept = 0, color = "red", linetype = 4, size = 2) +
      geom_ribbon(mapping = aes(ymin = deriv-1.96*se.deriv, 
                                ymax = deriv+1.96*se.deriv), 
                  alpha = .35) +
      geom_line(size = 2) + 
      theme_classic() +
      theme(#legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))+
      ylab("Derivative (Z/yr)") +
      xlab("Age")+ 
      scale_y_continuous(
        labels = scales::number_format(accuracy = 0.01))
  })
  
  output$gene_traj_tbl = renderDT({
    dt <- gene_traj_table()
    dt[nrow(dt)+1,] <- NA
    
    datatable(dt, 
              filter = list(position = 'top', plain = TRUE), 
              option = list( pageLength = 15, autoWidth = FALSE,
                             scrollX = TRUE))
  })  
  
  filt.change <- reactive({
    idx <- c()
    idx2 <- c()
    
    if(input$Filter2 == "Consistent") {
      idx <- which(db$consistent == 0)
    }
    
    if(input$Filter == "Young") { 
      idx2 <- which(db$Young.Bin == 0)
    }
    if(input$Filter == "Old") { 
      idx2 <- which(db$Old.Bin == 0)
    }
    
    idx3 <- unique(c(idx, idx2))
    
    if(length(idx3) > 0) {
      tmp.gene <- gene_trajs[-idx3]
    } else {tmp.gene <- gene_trajs}
    
    tmp.gene
  })
  
  observe({
    ch <- filt.change()
    updateSelectInput(session, 'variable', choices = ch, selected = ch[1])
    })
  
  # Genes ----
  output$genes_tbl = renderDT(datatable(
    tbl2plot_gene, filter = list(position = 'top', plain = T), 
    option = list( pageLength = 15, autoWidth = F)
  ))
  
  # GO ----
  output$go_tbl = renderDT(datatable(
    tbl2plot_go, filter = list(position = 'top', plain = T), 
    option = list( pageLength = 15, autoWidth = F)
  ))
  
  
})
