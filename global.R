library(shiny); library(shinydashboard);
library(ggplot2); library(dplyr); library(tidyr)
library(purrr); library(DT)
library(ggseg);

load("data/shinydata.rda")

gene_trajs = unique(gene_traj_db$gene)

# Some steals to avoid package installs when only using single func
# Stolen from ggridges: https://github.com/clauswilke/ggridges/blob/master/R/theme.R
theme_custom <- function(font_size = 14, font_family = "", line_size = .5, grid = TRUE, center_axis_labels = FALSE) {
  half_line <- font_size / 2
  small_rel <- 0.857
  small_size <- small_rel * font_size
  color <- "grey90"
  
  if (grid) {
    panel.grid.major <- element_line(colour = color, size = line_size)
    axis.ticks       <- element_line(colour = color, size = line_size)
    axis.ticks.y     <- axis.ticks
  }
  else {
    panel.grid.major <- element_blank()
    axis.ticks       <- element_line(colour = "black", size = line_size)
    axis.ticks.y     <- element_blank()
  }
  
  if (center_axis_labels) {
    axis_just <- 0.5
  }
  else {
    axis_just <- 1.0
  }
  
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      rect              = element_rect(fill = "transparent", colour = NA, color = NA, size = 0, linetype = 0),
      text              = element_text(family = font_family, face = "plain", colour = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),
      axis.text         = element_text(colour = "black", size = small_size),
      #axis.title        = element_text(face = "bold"),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1, vjust = 0),
      axis.title.x      = element_text(
        margin = margin(t = small_size / 2, b = small_size / 4),
        hjust = axis_just
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = small_size / 2, l = small_size / 4),
        hjust = axis_just
      ),
      axis.ticks        = axis.ticks,
      axis.ticks.y      = axis.ticks.y,
      axis.line         = element_blank(),
      legend.key        = element_blank(),
      legend.key.size   = grid::unit(1, "lines"),
      legend.text       = element_text(size = rel(small_rel)),
      legend.justification = c("left", "center"),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      # make grid lines
      panel.grid.major  = panel.grid.major,
      panel.grid.minor  = element_blank(),
      strip.text        = element_text(size = rel(small_rel)),
      strip.background  = element_rect(fill = "grey80", colour = "grey50", size = 0),
      plot.background   = element_blank(),
      plot.title        = element_text(face = "bold",
                                       size = font_size,
                                       margin = margin(b = half_line), hjust = 0),
      plot.subtitle     = element_text(size = rel(small_rel),
                                       hjust = 0, vjust = 1,
                                       margin = margin(b = half_line * small_rel)),
      plot.caption      = element_text(size = rel(small_rel),
                                       hjust = 1, vjust = 1,
                                       margin = margin(t = half_line * small_rel)),
      plot.margin       = margin(half_line, font_size, half_line, half_line),
      
      complete = TRUE
    )
}


# Stolen from scales
squish <- function (x, range = c(0, 1), only.finite = TRUE) {
  force(range)
  finite <- if (only.finite) 
    is.finite(x)
  else TRUE
  x[finite & x < range[1]] <- range[1]
  x[finite & x > range[2]] <- range[2]
  x
}
