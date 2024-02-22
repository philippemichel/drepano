importph <- function(){
    library(tidyverse)
    library(janitor)
    library(labelled)
    library(baseph)
bnom <- read_delim("datas/bnom.csv", delim = ";", show_col_types = FALSE)
ttd <- read_delim("datas/drepa2.csv", delim = ";", show_col_types = FALSE) |> 
  clean_names() |> 
  mutate_if(is.character, as.factor)
var_label(ttd) <- bnom$nom
#
ttd$imcx <- bmiph(ttd$imc, "fr")
var_label(ttd$imcx) <- "IMC"
ttd <- ttd |> 
  relocate(imcx, .after = imc)
## Recodage de tt$cvo_an en tt$cvo_an_rec
ttd$cvo_an <- cut(ttd$cvo_an,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 1, 2, 3, 4,5, 10,50), 
  labels = c("0","1","2","3","4","5-10",">10")
)
var_label(ttd$cvo_an) <- "CVO hospitalisées par an (moyenne)"
#
## Recodage de tt$sta en tt$sta_rec
# ttd$sta<- cut(ttd$sta,
#   include.lowest = TRUE,
#   right = FALSE,
#   dig.lab = 4,
#   breaks = c(0, 1, 2, 3,4,6, 11, 25),
#   labels = c("0","1","2","3","4-5","6-10",">10")
# )
#
befr <- read.csv("datas/befr.csv")
efr <- read_delim("datas/efr.csv",delim = ",") |> 
  clean_names()
var_label(efr) <- befr$nom
#
efr$tvo <- as.factor(ifelse(efr$vems_cvf_percent < (efr$vems_cvf_lin*100), "Oui","Non"))
efr$tvr <- as.factor(ifelse((efr$vems_l < efr$vems_lin_l) & (efr$cvf_l < efr$cvf_lin_l), "Oui","Non"))
efr$dlcob <- as.factor(ifelse((efr$dlc_ocor_percent < 70) & (efr$tvr == "Non") & (efr$tvo == "Non"), "Oui","Non" ))
var_label(efr$tvo) <- "TVO"
var_label(efr$tvr) <- "TVR"
var_label(efr$dlcob) <- "DLCOc basse isolée"

tt <- left_join(ttd,efr[,c(1,2,4,6,7,9,10,16:27)],"id")
#
type <- read_delim("datas/type.csv", delim = ";", show_col_types = FALSE)
tt <- left_join(tt,type,"id") |> 
  mutate(type = type2) |> 
  select(-type2) 
# bnom <- read.csv("datas/bgroupe.csv")
# bnom <- bnom$nom
# ttg <- debutph("datas/groupe.csv", bnom)
# ttg <- left_join(ttg,tt,"id")
saveRDS(tt, "drepano.RDS")
}
