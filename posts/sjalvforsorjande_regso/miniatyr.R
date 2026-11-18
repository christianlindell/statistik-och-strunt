# Skapar miniatyr.png, förhandsbilden i inläggslistan (kartan i index.qmd är interaktiv och ger
# ingen bild). Andel självförsörjande 2023 per RegSO, med samma klasser och färger som kartan.
# Körs från inläggets mapp: Rscript miniatyr.R
library(sf)
library(dplyr)
library(ggplot2)

regso_geo <- st_read(file.path("karta", "regso2020_forenklad.gpkg"), quiet = TRUE)
statistik <- readRDS(file.path("data", "sjalvfors_regso2020.rds")) |> filter(ar == max(ar))

klasser <- c(-Inf, seq(40, 80, 5), Inf)
etiketter <- c("< 40", paste0(seq(40, 75, 5), "–", seq(44, 79, 5)), "80 +")
karta <- regso_geo |>
  inner_join(statistik, by = "regsokod") |>
  mutate(klass = cut(sjalvf_andel, klasser, labels = etiketter, right = FALSE))

farger <- colorRampPalette(c("#cde2fb", "#0d366b"))(length(etiketter))
names(farger) <- etiketter

p <- ggplot(karta) +
  geom_sf(aes(fill = klass), colour = NA) +
  scale_fill_manual(values = farger, na.value = "grey85", name = "Självförsörjande\n2023 (%)") +
  theme_void(base_size = 9) +
  theme(legend.position = "left", plot.background = element_rect(fill = "white", colour = NA))
ggsave("miniatyr.png", p, width = 4, height = 4.5, dpi = 150)
