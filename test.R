
histm <- function(dfx, varx, varn, tit = ""){
  dfx |> 
    drop_na({{varn}}) |> 
    drop_na({{varx}}) |> 
    ggplot() +
    aes(x = {{varn}}, fill = {{varx}}, color = {{varx}}) +
    geom_bar(stat = "count") +
    facet_grid(vars({{varx}})) +
    labs(title = tit,
         x = "Nombre de STA",
         y = "n",
         caption = tit) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0,10,2)) +
    theme_light() +
    scale_fill_discrete_qualitative(palette = "Dynamic") +
    theme(
      strip.text.y = element_text(size=12),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_text(size = 12),
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
}
