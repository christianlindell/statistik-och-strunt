# Skapar data/valresultat_kommungrupp.rds till inlägget: röster per parti och kommungrupp
# i riksdagsvalet 2022 och 2026, plus antalet röstande (valdeltagande, inklusive blanka och
# ogiltiga röster) per kommungrupp.
#
# Källfilerna ligger i projektet valresultat_2026 (R_claude):
#   - slutligt-valresultat-riksdagen-jamforande-statistik-2022-2026.xlsx (Valmyndigheten)
#   - Kommungruppsindelning-2023.xlsx (SKR)
# Körs från inläggets mapp: Rscript forbered_data.R
library(readxl)
library(dplyr)

kalla <- file.path("..", "..", "..", "valresultat_2026")

grupp <- read_excel(file.path(kalla, "Kommungruppsindelning-2023.xlsx"),
                    sheet = "Bilaga1 Lista alla kommuner", skip = 1, col_types = "text") |>
  select(kommunkod = Kommunkod, gruppkod = Gruppkod, kommungrupp = `Kommungrupp 2023`)

val <- read_excel(file.path(kalla, "slutligt-valresultat-riksdagen-jamforande-statistik-2022-2026.xlsx"),
                  sheet = "Per kommun") |>
  select(kommunkod = Kommunkod, kategori = `Parti/Kategori`,
         roster_2026 = `Röster 2026`, roster_2022 = `Röster 2022`)

stopifnot(setequal(unique(val$kommunkod), grupp$kommunkod))

partier <- c("Vänsterpartiet", "Miljöpartiet de gröna", "Arbetarepartiet-Socialdemokraterna",
             "Centerpartiet", "Liberalerna (tidigare Folkpartiet)", "Moderaterna",
             "Kristdemokraterna", "Sverigedemokraterna", "Övriga anmälda partier")

data <- val |>
  filter(kategori %in% c(partier, "Valdeltagande")) |>
  inner_join(grupp, by = "kommunkod") |>
  group_by(gruppkod, kommungrupp, kategori) |>
  summarise(antal_kommuner = n_distinct(kommunkod),
            roster_2026 = sum(roster_2026), roster_2022 = sum(roster_2022),
            .groups = "drop")

# Kontroll: summan av partiernas röster är lika med rikets giltiga röster
riket <- read_excel(file.path(kalla, "slutligt-valresultat-riksdagen-jamforande-statistik-2022-2026.xlsx"),
                    sheet = "Hela riket")
stopifnot(sum(data$roster_2026[data$kategori %in% partier]) ==
            riket$`Röster 2026`[riket$`Parti/kategori` == "Giltiga Röster"])

dir.create("data", showWarnings = FALSE)
saveRDS(data, file.path("data", "valresultat_kommungrupp.rds"))
