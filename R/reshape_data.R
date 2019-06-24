library(tidyr); library(dplyr); 
library(purrr); 

# VH data ----
load("data-raw/db.Rda" )
load("data-raw/varnames.Rda")
load("data-raw/VH_CorrRefPanel.Rda")
load("data-raw/VH_Lifespan.Rda")
load("data-raw/VH_SigThresh.Rda")
load("data-raw/gamm.stats.Rda")
load("data-raw/GeneTable2Shiny.Rda")
load("data-raw/GOtable2shiny.Rda")

region <- varnames$region %>% 
  strsplit("_") %>% 
  simplify2array()
region <- region[1,]

db <- lapply(db, stats::setNames, nm = varnames$region)

## Prepare table
tbl2plot_vh <- cor_ReferencePanel %>% 
  gather(Age, corr,-c(Gene, CellType, Allen.Average.donor.correlation.to.median, Allen.vs.Brainspan.correlation)) %>% 
  separate(Age, c("tmp", "Age") , sep = "expression_phenotype_rX") %>% 
  select(-tmp) %>%
  mutate(Age = as.factor(as.numeric(Age)),
         CellType = as.factor(CellType)) %>% 
  select(Gene, Age, corr, everything()) 
names(tbl2plot_vh) <- c("Gene","Age", "CellType","Phenotype-Gene corr", "Allen Average donor corr to median", "Allen-Brainspan corr")

# Prepare ggseg
ggseg_cth_deriv <- data.frame(t(db$ders)) 
names(ggseg_cth_deriv) <- names(ggseg_cth_deriv)%>% strsplit("X") %>% simplify2array() %>% .[2,]
AgeU <- names(ggseg_cth_deriv)

ggseg_cth_deriv <- ggseg_cth_deriv %>% 
  mutate(label = paste0("lh_", region)) %>% 
  gather(Age, val, -label)


# prepare thinning bar plotting
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
tbl2plot_go <- GO.table %>% 
  mutate(TermID = as.factor(TermID), 
         P = -log10(P),
         Term = as.factor(Term)) %>% 
  select(-Symbols)

# genes data ----

tbl2plot_gene <- GeneTable %>% 
  select(-c(RegBin.old, RegBin.young))
names(tbl2plot_gene)[4:5] <- c("Age regulation in old (Z-score)", 
                               "Age regulation in young (Z-score)")
tbl2plot_gene <- tbl2plot_gene %>%  
  select(1,6,5,2,4,3,7,8) 

# Traj data ----
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


stat <- names(gamm.stats)[-1]
gamm.stats <- gamm.stats %>%  
  separate(regions, c("label", "tmp")) %>% 
  mutate(label = paste0("lh_", label)) %>% 
  gather(key, val, c(-label,-tmp))


save(region, stat, gamm.stats, traj_long_cth, traj_long_deriv,
     db, tbl2plot_gene, tbl2plot_go, cor_ReferencePanel, 
     vh_long_deriv, gglim, ggseg_cth_deriv, AgeU, tbl2plot_vh,
     res_tab,
     file="data/shinydata.rda")
save(region, stat, gamm.stats, traj_long_cth, traj_long_deriv,
     db, tbl2plot_gene, tbl2plot_go, cor_ReferencePanel, 
     vh_long_deriv, gglim, ggseg_cth_deriv, AgeU, tbl2plot_vh,
     res_tab,
     file="vh_new/data/shinydata.rda")
