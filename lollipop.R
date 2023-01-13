

#  ------------------------------------------------------------------------
#
# Title : lollipop  
#    By : PhM 
#  Date : 2023-01-08
#    
#  ------------------------------------------------------------------------
tits <-
  c(
    "Kyste",
    "Emphysème",
    "Rayons de miel",
    "Condensation",
    "Verre Dépoli",
    "Réticulation",
    "Bonchectasie",
    "Bandes d'atélectasie",
    "Nodules",
    "Micronodules"
  )
lls <-
  c(
    "kyste.",
    "emphyseme.",
    "rdm.",
    "cond.",
    "vd.",
    "ret.",
    "Bronch.",
    "Band.",
    "Nodules.",
    "Micronod.")
    loc <-
      c(
        "Culmen",
        "Lobe inférier droit",
        "Lobe inférier gauche",
        "Lingula",
        "Lobe moyen",
        "Lobe supérieur droit"
      )
    
for (nn in 1:10){
pp <- tt |>
  select(c(2,starts_with(lls[nn]))) |> 
  drop_na() |>
  pivot_longer(cols =  everything(),
               names_to =  "Localisation",
               values_to = "Nombre") |>
  mutate(Nombre = as.numeric(Nombre) - 1) 
  group_by(Localisation, groupe) 
  summarise(Nombre = sum(Nombre), groupe = groupe) 
  ggplot() +
  aes(x = Localisation, y = Nombre, fill = groupe) +
  geom_point(
    size = 5,
    color = "red",
    shape = 19
  ) +
  geom_segment(aes(
    x = Localisation,
    xend = Localisation,
    y = 0,
    yend = Nombre
  )) +
    labs(title = tits[nn],
         subtitle = "n = 75") +
  theme_light() +
  scale_x_discrete(labels = loc) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12 ,)
  )
print(pp)
 # ggsave(paste0("lesions/",lls[nn],"pdf"))
}

    
