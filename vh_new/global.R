library(shiny); library(shinydashboard);
library(ggplot2); library(tidyr); library(dplyr); 
library(purrr); library(DT)
library(ggseg); library(here)

outdir <- here()

# VH data ----
load(paste0(outdir, "/data/db.Rda" ))
load(paste0(outdir, "/data/varnames.Rda"))
load(paste0(outdir, "/data/VH_CorrRefPanel.Rda"))
load(paste0(outdir, "/data/VH_Lifespan.Rda"))
load(paste0(outdir, "/data/VH_SigThresh.Rda"))

region <- varnames$region %>% 
  strsplit("_") %>% 
  simplify2array()
region <- region[1,]


## Prepare table
tbl2plot <- cor_ReferencePanel %>% 
  gather(Age, corr,-c(Gene, CellType, Allen.Average.donor.correlation.to.median, Allen.vs.Brainspan.correlation)) %>% 
  separate(Age, c("tmp", "Age") , sep = "expression_phenotype_rX") %>% 
  select(-tmp) %>%
  mutate(Age = as.factor(as.numeric(Age)),
         CellType = as.factor(CellType)) %>% 
  select(Gene, Age, corr, everything())
names(tbl2plot) <- c("Gene","Age", "CellType","Phenotype-Gene corr", "Allen Average donor corr to median", "Allen-Brainspan corr")

# Prepare ggseg
ggseg_cth_deriv <- data.frame(t(db$ders)) 
names(ggseg_cth_deriv) <- names(ggseg_cth_deriv)%>% strsplit("X") %>% simplify2array() %>% .[2,]
AgeU <- names(ggseg_cth_deriv)

ggseg_cth_deriv <- ggseg_cth_deriv %>% 
  mutate(label = paste0("lh_", region)) %>% 
  gather(Age, val, -label)


# prepare thinning bar plotting
db <- db %>% 
  lapply(., setNames, nm = varnames$region)

vh_long_deriv <- db$ders %>% 
  mutate(Age = as.numeric(rownames(db$ders))) %>% 
  gather(region, ders,-c(Age)) %>% 
  left_join(., db$cis %>% 
              mutate(Age = as.numeric(rownames(db$ders))) %>% 
              gather(region,cis,-c(Age))) %>% 
  separate(region, c("region")) 
gglim <- c(min(vh_long_deriv$ders)-max(vh_long_deriv$cis),max(vh_long_deriv$ders)+max(vh_long_deriv$cis))  

# prepare VH data
names(cor_ReferencePanel) <- names(cor_ReferencePanel) %>% 
  gsub("expression_phenotype_rX","",.)
names(sigp)[2] <- "ci"
names(res_tab)[1] <- "CellType"
res_tab <- res_tab %>% 
  left_join(sigp, by = c("CellType" = "celltypes"))

cor_ReferencePanel <- cor_ReferencePanel %>% 
  filter(!is.na(CellType)) %>% 
  mutate(y = runif(nrow(.), -.5,-.1))


# Go data ----
load(paste0(outdir, "/data/GOtable2shiny.Rda"))
tbl2plot <- GO.table %>% 
  mutate(TermID = as.factor(TermID), 
         P = -log10(P),
         Term = as.factor(Term)) %>% 
  select(-Symbols)

# genes data ----
load(paste0(outdir, "/data/GeneTable2Shiny.Rda"))

tbl2plot <- GeneTable %>% 
  select(-c(RegBin.old, RegBin.young))
names(tbl2plot)[4:5] <- c("Age regulation in old (Z-score)", 
                          "Age regulation in young (Z-score)")
tbl2plot <- tbl2plot %>%  
  select(1,6,5,2,4,3,7,8) 

# Traj data ----
load(paste0(outdir,"/data/db.Rda" ))
load(paste0(outdir,"/data/varnames.Rda"))

region <- varnames$region %>% 
  strsplit("_") %>% 
  simplify2array()
region <- region[1,]

db <- lapply(db, setNames, nm = varnames$region)

# prepare thickening/thinning database
traj_long_deriv <- db$ders %>% 
  mutate(Age = as.numeric(rownames(db$ders))) %>% 
  gather(region, ders,-c(Age)) %>% 
  left_join(db$cis %>% 
              mutate(Age = as.numeric(rownames(db$ders))) %>% 
              gather(region,cis,-c(Age))) %>% 
  separate(region, c("region")) 

# prepare cth database
traj_long_cth <- db$cth %>% 
  mutate(Age = as.numeric(rownames(db$cth))) %>% 
  gather(region, cth,-c(Age)) %>% 
  left_join(db$cth.ci %>% 
              mutate(Age = as.numeric(rownames(db$ders))) %>% 
              gather(region,cis,-c(Age))) %>% 
  separate(region, c("region"))

# Gamm stats ----

load(paste0(outdir, "/data/gamm.stats.Rda"))
load(paste0(outdir, "/data/varnames.Rda"))

stat <- names(gamm.stats)[-1]
gamm.stats <- gamm.stats %>%  
  separate(regions, c("label", "tmp")) %>% 
  mutate(label = paste0("lh_", label)) %>% 
  gather(key, val, c(-label,-tmp))

# ggseg_brain <- ggseg()
# ggseg_brain$data %<>%  
#   left_join(., gamm.stats)


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
squish <- function (x, range = c(0, 1), only.finite = TRUE) 
{
  force(range)
  finite <- if (only.finite) 
    is.finite(x)
  else TRUE
  x[finite & x < range[1]] <- range[1]
  x[finite & x > range[2]] <- range[2]
  x
}
