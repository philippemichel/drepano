

#  ------------------------------------------------------------------------
#
# Title : Import des tables csv
#    By : Ph MICHEL
#  Date : 2023-02-06
#    
#  ------------------------------------------------------------------------


library(baseph)
library(labelled)
library(tidyverse)
#
bnom <- read.csv("datas/bnom.csv")
bnom <- bnom$nom
ttd <- debutph("datas/drepa2.csv", bnom)
ttd$imc <- bmiph(ttd$imc, "fr")
var_label(ttd$imc) <- "IMC"
## Recodage de tt$cvo.an en tt$cvo.an_rec
ttd$cvo.an <- cut(ttd$cvo.an,
                  include.lowest = TRUE,
                  right = FALSE,
                  dig.lab = 4,
                  breaks = c(0, 1, 2, 3, 4,5, 10,50), 
                  labels = c("0","1","2","3","4","5-10",">10")
)
var_label(ttd$cvo.an) <- "Nb de CVO hospitalisée par an en moyenne"
#
## Recodage de tt$sta en tt$sta_rec
ttd$sta<- cut(ttd$sta,
              include.lowest = TRUE,
              right = FALSE,
              dig.lab = 4,
              breaks = c(0, 1, 2, 3,4,6, 11, 25),
              labels = c("0","1","2","3","4-5","6-10",">10")
)
#
befr <- read.csv("datas/befr.csv")
befr <- befr$nom
efr <- debutph("datas/efr.csv",befr)
efr$tvo <- as.factor(ifelse(efr$vems.cvf.percent < (efr$vems.cvf.lin*100), "Oui","Non"))
efr$tvr <- as.factor(ifelse((efr$vems.l < efr$vems.lin.l) & (efr$cvf.l < efr$cvf.lin.l), "Oui","Non"))
efr$dlcob <- as.factor(ifelse((efr$dlc.ocor.percent < 70) & (efr$tvr == "Non") & (efr$tvo == "Non"), "Oui","Non" ))
var_label(efr$tvo) <- "TVO"
var_label(efr$tvr) <- "TVR"
var_label(efr$dlcob) <- "DLCOc basse isolée"

tt <- left_join(ttd,efr[,c(1,2,4,6,7,9,10,16:27)],"id")
#
bnom <- read.csv("datas/bgroupe.csv")
bnom <- bnom$nom
ttg <- debutph("datas/groupe.csv", bnom)
ttg <- left_join(ttg,tt,"id")


ttg |> 
  select(2:6) |> 
  mutate_all(as.numeric) |> 
  cor() |> 
  corrplot(method = "ellipse", type = "lower", diag = FALSE, , addCoef.col = 'red')

