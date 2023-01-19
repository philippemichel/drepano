

#  ------------------------------------------------------------------------
#
# Title : Facto-drepano
#    By : Philippe MICHEL
#  Date : 2022-12-28
#    
#  ------------------------------------------------------------------------


zz <- tt |> 
  select(2:4,7:22,29:45) |> 
 # drop_na(c(diabete, osteonecrose))
  drop_na()
## Recodage de zz$age en zz$age_rec
zz$age<- cut(zz$age,
  include.lowest = TRUE,
  right = FALSE,## Recodage de zz$sta en zz$sta_rec
  dig.lab = 4,
  breaks = c(0, 20, 40, 60, 100)
)
## Recodage de zz$hb.base en zz$hb.base_rec
zz$hb.base<- cut(zz$hb.base,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 7, 8, 10, 20)
)
## Recodage de zz$sta en zz$sta_rec
zz$sta<- cut(zz$sta,
                  include.lowest = TRUE,
                  right = FALSE,
                  dig.lab = 4,
                  breaks = c(0, 1, 2, 4, 20)
)
###
zza <- imputeMCA(zz,0)


mtt <- MCA(zz, quali.sup = 1,graph = FALSE)
fviz_screeplot(mtt, addlabels = TRUE, ylim = c(0, 20))

fviz_contrib(mtt, choice = "var", axes = 1, top = 20)
fviz_contrib(mtt, choice = "var", axes = 2, top = 40)
fviz_mca_var(mtt, repel = TRUE)
fviz_mca_ind(mtt, habillage = 1,addEllipses = TRUE)
