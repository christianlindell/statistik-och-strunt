# Gemensam databearbetning för index.qmd och miniatyr.R. Läser data/valresultat_kommungrupp.rds
# (skapas av forbered_data.R) och ger:
#   andel      – partiernas andel av de giltiga rösterna per kommungrupp 2022 och 2026, med
#                rikets andel och förändring som referens
#   fordelning – hur varje partis röster 2026 fördelar sig på kommungrupperna (summerar till
#                100 % per parti), plus alla röstande (inklusive blanka och ogiltiga röster)
library(dplyr)

data <- readRDS(file.path("data", "valresultat_kommungrupp.rds"))

# Partierna i ideologisk ordning, med korta namn och vedertagna partifärger
partinamn <- c("Vänsterpartiet" = "V", "Miljöpartiet de gröna" = "MP",
               "Arbetarepartiet-Socialdemokraterna" = "S", "Centerpartiet" = "C",
               "Liberalerna (tidigare Folkpartiet)" = "L", "Moderaterna" = "M",
               "Kristdemokraterna" = "KD", "Sverigedemokraterna" = "SD",
               "Övriga anmälda partier" = "Övriga")
partifarg <- c(V = "#8B0000", MP = "#83CF39", S = "#E8112D", C = "#009933", L = "#006AB3",
               M = "#52BDEC", KD = "#000077", SD = "#DDDD00", "Övriga" = "#9B9B9B")

# En färgfamilj per huvudgrupp (A blå, B grön, C orange/brun), mörkast för de mest
# stadspräglade grupperna. Grannfärgerna går att skilja åt även vid färgblindhet.
kg_farg <- c("A1 Storstäder"                          = "#1F3A73",
             "A2 Pendlingskommun nära storstad"       = "#6C93C9",
             "B3 Större stad"                         = "#1B6B4A",
             "B4 Pendlingskommun nära större stad"    = "#4FA57A",
             "B5 Lågpendlingskommun nära större stad" = "#A6D6B5",
             "C6 Mindre stad/tätort"                  = "#8C3B12",
             "C7 Pendlingskommun nära mindre tätort"  = "#C8692B",
             "C8 Landsbygdskommun"                    = "#E9A066",
             "C9 Landsbygdskommun med besöksnäring"   = "#F6D3A8")

partier_df <- data |>
  filter(kategori != "Valdeltagande") |>
  mutate(parti = factor(partinamn[kategori], levels = partinamn),
         grupp = factor(paste(gruppkod, kommungrupp), levels = names(kg_farg)))
stopifnot(!anyNA(partier_df$parti), !anyNA(partier_df$grupp))

andel <- partier_df |>
  group_by(grupp) |>
  mutate(andel_2026 = 100 * roster_2026 / sum(roster_2026),
         andel_2022 = 100 * roster_2022 / sum(roster_2022)) |>
  group_by(parti) |>
  mutate(riket_2026 = 100 * sum(roster_2026) / sum(partier_df$roster_2026),
         riket_2022 = 100 * sum(roster_2022) / sum(partier_df$roster_2022)) |>
  ungroup() |>
  mutate(diff = andel_2026 - andel_2022,
         diff_riket = riket_2026 - riket_2022)

fordelning <- bind_rows(
  partier_df |>
    transmute(parti = as.character(parti), grupp = as.character(grupp), roster = roster_2026),
  data |>
    filter(kategori == "Valdeltagande") |>
    transmute(parti = "Alla röstande", grupp = paste(gruppkod, kommungrupp), roster = roster_2026)
) |>
  group_by(parti) |>
  mutate(andel = 100 * roster / sum(roster)) |>
  ungroup() |>
  # Liggande staplar ritas nerifrån och upp, så ordningen vänds för att V ska stå överst
  mutate(parti = factor(parti, levels = rev(c(partinamn, "Alla röstande"))),
         grupp = factor(grupp, levels = names(kg_farg)))
stopifnot(all(abs(tapply(fordelning$andel, fordelning$parti, sum) - 100) < 1e-9))

kalla <- "Källa: Valmyndigheten (slutligt valresultat riksdagen 2026) och SKR (kommungruppsindelning 2023). Bearbetning: Christian Lindell."
