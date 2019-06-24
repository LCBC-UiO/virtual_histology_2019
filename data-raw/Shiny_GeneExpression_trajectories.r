### Shiny - Cortical Trajectories ###
# ----------------------------------#
#Load GAMM fitting and derivatives from the 34 Desikan left cortical hemisphere ROIs
#Display trajectories along the lifespan
#Display Confidence interval of fitting/derivative if highlighted


###### loading data ######
# -----------------------#
packages <- c("shiny", "dplyr", "magrittr", "ggplot2", "tidyr", "DT")
lapply(packages, require, character.only = T)

load("data/db.all.Rda")

gene = unique(db$gene)
options(digits = 2)
######## Preparing data to plot ########
# .....................................#  


##### main shiny script ####
# .........................#

ui <- fluidPage(
  fluidRow(titlePanel("Gene Expression trajectories across the lifespan"),
           selectInput("variable", "Gene (select one or more):", gene, multiple = TRUE, selected = "A1BG")),
  fluidRow(h5("Error bars represent 95% CI of the trajectories", style="color:red")),
  fluidRow(plotOutput("plot.gene")),
  fluidRow(plotOutput("plot.ders")),
  fluidRow(h5("main stats from GAMM fittings", style="color:red")),
  DTOutput('tbl')
)




server <- function(input, output, session) {
  
  i <- reactive({
    which(db$gene %in% input$variable)
  })
  
  db.plot <- eventReactive(input$variable, {
    data.frame(deriv = as.vector(t(db$deriv[i(),])), 
               se.deriv = as.vector(t(db$se_deriv[i(),])), 
                fit = as.vector(t(db$fit[i(),])), 
               se.fit = as.vector(t(db$se_fit[i(),])),
               gene = rep(db$gene[i()], each = 500), 
               age = rep(db$age,length(i())))
  })
  
  db.table <- eventReactive(input$variable, {
    db$stats %>% dplyr::filter(gene %in% input$variable) %>% 
      as.data.frame() %>% 
      mutate(value = round(value, 2))
  })
  
    
  output$plot.gene <- renderPlot({
    ggplot(data = db.plot(), 
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
      xlab("Age")
  })
  
  # plot derivative
  output$plot.ders <- renderPlot({
    ggplot(data = db.plot(), 
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
            legend.title = element_text(size = 14), face = "bold")+
      ylab("Derivative (Z/yr)") +
      xlab("Age")
    
  })
  
    output$tbl = renderDT(datatable(db.table(), filter = list(position = 'top', plain = T), option = list( pageLength = 15, autoWidth = F)))  
    
  
}

shinyApp(ui, server)

