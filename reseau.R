rad <- c("kyste","emphyseme","rdm", "cond","vd", "ret","bronch", "band","diminution.volume"  ,    "augmentation.vx.pulm" )
brad <- c("Kyste", "Emphysème", "Rayon de miel", "Condensation","Verre dépoli", "Réticulation", "Bronchectasie", "Bandes d'atélectasie", "diminution volume des lobes inférieurs" ,"augmentation vaisseaux pulmonaires" )
tles <- NULL
for (i in 1:10) {
  zz <- tt |>
    select(contains(rad[i])) |>
    mutate_all(as.numeric)
  zz$tot <- rowSums(zz) - ncol(zz)
  zz <- as.factor(ifelse(zz$tot > 0, "Oui", "Non"))
  tles$zz <- zz
  names(tles)[i] <- brad[i]
}
  tles <- as_tibble(tles)
 # var_label(tles) <- brad

tlesn <- tles |> 
  mutate_all(as.numeric) |> 
  drop_na()

ccs <- cor(tlesn)
corrplot(ccs, type ="lower", tilte = "Lésions pumonaires", diag = FALSE, order = "AOE")
  

tles$groupe
mtt <-tles |> 
  drop_na() |> 
  MCA()


aa <-correlation(tlesn)
bb <- summary(aa)
plot(bb)+ theme_modern()



aa <- ttg |> 
  group_by(lesion) |> 
  summarise(mediane = median(sta, na.rm = TRUE), 
            qu1 = quantile(sta, na.rm = TRUE)[2],
            qu2 = quantile(sta, na.rm = TRUE)[4])



ttg |> 
  drop_na(sta) |> 
  ggplot() +
  aes(x = sta) +
  geom_histogram(color = "black", binwidth = 1) +
  facet_grid(~lesion) +
  labs(title = "Nb de STA & lésion radiologique prédominante",
       subtitle = "p = 0,xxx",
       y = "n",
       caption = "Nb de STA & lésion radiologique prédominante") +
  scale_y_continuous(limits = c(0, 10)) +
  theme_light() +
  scale_fill_discrete_qualitative(palette = "Dynamic") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(
      size = 12,
      angle = 0,
      vjust = .5
    ),
    axis.text.x = element_text(
      size = 12
    ),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

  
