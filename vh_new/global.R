library(shiny); library(dplyr); library(magrittr); 
library(ggplot2); library(tidyr); library(scales); 
library(ggridges); library(purrr); library(DT)
library(ggseg); library(here)


# VH data ----
outdir <- here()
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
tbl2plot <- cor_ReferencePanel %>% gather(Age, corr,-c(Gene, CellType, Allen.Average.donor.correlation.to.median, Allen.vs.Brainspan.correlation)) %>% 
  separate(Age, c("tmp", "Age") , sep = "expression_phenotype_rX") %>% 
  select(-tmp) %>%
  mutate(Age = as.factor(as.numeric(Age)),
         CellType = as.factor(CellType)) %>% 
  select(Gene, Age, corr, everything())
names(tbl2plot) <- c("Gene","Age", "CellType","Phenotype-Gene corr", "Allen Average donor corr to median", "Allen-Brainspan corr")

# Prepare ggseg
cth.deriv <- data.frame(t(db$ders)) 
names(cth.deriv) %<>% strsplit("X") %>% simplify2array() %>% .[2,]
#names(cth.deriv) %<>% gsub("X","Age",.)
AgeU <- names(cth.deriv)
cth.deriv %<>% mutate(label = paste0("lh_", region))
ggseg_cth_deriv <- cth.deriv %>% gather(Age, val, -label)

# ggseg_brain <- ggseg()
# ggseg_brain$data %<>%  left_join(., cth.deriv)

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
tbl2plot %<>% 
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
long.deriv <- db$ders %>% 
  mutate(Age = as.numeric(rownames(db$ders))) %>% 
  gather(region, ders,-c(Age))  
long.deriv %<>% 
  left_join(., db$cis %>% 
              mutate(Age = as.numeric(rownames(db$ders))) %>% 
              gather(region,cis,-c(Age))) %>% 
  separate(region, c("region")) 

# prepare cth database
long.cth <- db$cth %>% 
  mutate(Age = as.numeric(rownames(db$cth))) %>% 
  gather(region, cth,-c(Age))  
long.cth %<>% 
  left_join(., db$cth.ci %>% 
              mutate(Age = as.numeric(rownames(db$ders))) %>% 
              gather(region,cis,-c(Age))) %>% 
  separate(region, c("region"))

# Gamm stats ----

load(paste0(outdir, "/data/gamm.stats.Rda"))
load(paste0(outdir, "/data/varnames.Rda"))

stat <- names(gamm.stats)[-1]
gamm.stats %<>% 
  separate(regions, c("label", "tmp")) %>% 
  mutate(label = paste0("lh_", label))

ggseg_brain <- ggseg()
ggseg_brain$data %<>%  
  left_join(., gamm.stats)
