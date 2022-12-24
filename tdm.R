zz<-tt |> 
  select(contains("nodules")) |> 
  mutate_all(as.numeric)
zz$tot<-rowSums(zz)-ncol(zz)
zz$tot <- ifelse(zz$tot>0, "nodules", "non")
zz$groupe<- tt$groupe
zz<- na.omit(zz)


tdmdrep <- function(nn){
  zz<-tt |> 
    select(contains(nn)) |> 
    mutate_all(as.numeric)
  zz$tot<-rowSums(zz)-ncol(zz)
  zz$tot <- ifelse(zz$tot>0, nn, "non")
  zz$groupe<- tt$groupe
  zz<- na.omit(zz)
  chisq.test(zz$tot,zz$groupe)
}