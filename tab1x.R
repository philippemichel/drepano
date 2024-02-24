

#  ------------------------------------------------------------------------
#
# Title : Table 1 provisoire
#    By : PhM
#  Date : 2023-01-05
#    
#  ------------------------------------------------------------------------

tab1x  <- function(dfx,  test = "moy", tit = "", note ="", lt = FALSE, export = FALSE){
  #
  if (test == "moy"){
    note <- "n (%) - moyenne ± écart type"
  }
  else{
    note <- "n (%) - médiane (quartiles)"
  }
  #
  tabf <- NULL
  tlig <- 1
  nl <- 1
  #
  for (l in 1:ncol(dfx)) {
    varx <- dfx[l]
    varx <- varx[,1]
    lvarx <- length(na.omit(varx))
    nom <- var_label(varx)
    nom <- names(dfx)[l]
    print("******************")
    print(nom)
    #
      # Factoriel
    #
      if (!is.numeric(varx)) {
        tabp <- c(nom,lvarx,"")
        bb <- table(varx)
        aa <- prop.table(bb)*100
        for (i in 1:length(levels(varx))) {
          ll <- c(levels(varx)[i],
                  "",
                  paste0(bb[i]," (",round(aa[[1]],1),"%)")
          )
          tabp <- rbind(tabp,ll)
        }
        print(tabp)
        nl <- nl + length(levels(varx))
      }
      else{
        # Numérique
        if (test == "moy"){
          am <- signif(mean(varx, na.rm = TRUE),4)
          as <- signif(sd(varx, na.rm = TRUE),4)
          aa <- paste0(am, " ± ", as)
        }
        else{
          mdx <- quantile(varx)
          aa <- paste0(mdx[[3]], " (",mdx[[2]]," ; ",mdx[[4]],")")
        }
        tabp <- c(nom,lvarx,aa)
}
    
      tabf <- rbind(tabf,tabp)
      #print(tabf)
      nl <- nl + 1
      tlig <- c(tlig,nl)
    }
  #
  titn = c(" ","n", "n (%)")
  #
  # Export
  #
  if (export == TRUE) {
    txls <-  as.data.frame(rbind(titn, tabf) )
    extit <- paste0("tables/", tit, ".xls")
    WriteXLS(
      txls, extit
    )
  }
  #
  # Tracé
  #
  titn = c(" ","n", paste0("n (%)\n",note))
  tabf <- as_tibble(tabf)
  ltab <- 1:nrow(tabf)
  lg <- ltab %in% tlig
  tlig <- tlig[-length(tlig)]
  print (lg)
  trait <- tlig[-1] - 1
  tabf$V1 <- cell_spec(tabf$V1 , bold =  lg)
  kbl(
    tabf,
    booktabs = TRUE,
    longtable = lt,
    row.names = FALSE,
    col.names = titn,
    escape = FALSE
  ) |>
    kable_styling(
      latex_options = c("HOLD_position", "scale_down", "repeat_header"),
      position = "center"
    ) |>
    footnote(general = note) |>
    row_spec(trait, hline_after = TRUE)
}


