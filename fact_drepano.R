

#  ------------------------------------------------------------------------
#
# Title : 
#    By : Ph MICHEL## Recodage de zz$age en zz$age_rec

)
#  Date : 2022-12-27
#    
#  ------------------------------------------------------------------------




)
rmt <- MCA(zz, graph = FALSE)
fviz_mca_var(rmt, repel = TRUE, addEllipse = TRUE)


fviz_mca_ind(rmt, repel = TRUE, habillage = 1, addEllipse = TRUE)

fviz_contrib(rmt, choice = "var", axes = 1, top = 20)
fviz_contrib(rmt, choice = "var", axes = 2, top = 20)

#*************************************************************
#*
#*
zz <- tt |>
  select(2,3,4,7:22,29:43)
zz$age <- cut(zz$age,
                  include.lowest = TRUE,
                  right = FALSE,
                  dig.lab = 4,
                  breaks = c(20, 30, 40, 50, 60, 80)
)
zz$hb.base<- cut(zz$hb.base,
                      include.lowest = TRUE,
                      right = FALSE,
                      dig.lab = 4,
                      breaks = c(6, 8, 10, 12, 16)
)
rmt <- MCA(zz, graph = FALSE)
fviz_mca_var(rmt, repel = TRUE, habillage =1,addEllipse = TRUE)
fviz_mca_ind(rmt, repel = TRUE, habillage =1,addEllipse = TRUE)
 




