# Skapar miniatyr.png, förhandsbilden i inläggslistan: diagrammet över hur partiernas röster
# fördelar sig på kommungrupperna, som stillbild. Körs från inläggets mapp: Rscript miniatyr.R
library(funcVGRD3)
source("diagramdata.R")

g <- func_d3_stapeldiagram(
  fordelning, x_value = "parti", y_value = "andel", group = "grupp",
  bar_type = "filled", horizontal = TRUE, colors = kg_farg,
  legend_position = "none", bar_line_color = "white",
  y_axis_title = "Andel av partiets röster (%)",
  animation_grow = FALSE, interactive_sort = FALSE, interactive_bar_type_toggle = FALSE,
  interactive_export = FALSE, width = 600, height = 450
)
func_d3_spara(g, "miniatyr.png", zoom = 1, delay = 1)
