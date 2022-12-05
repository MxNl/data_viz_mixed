library(geofacet)
library(here)
library(sf)
library(feather)
library(lubridate)
library(data.table)
library(dtplyr)
library(ggfx)
library(showtext)
library(cowplot)
library(tidyverse)

year <- 2015
pal_wetdry <- c("#002D5E", "#0C7182", "#6CB7B0", "#A7D2D8", "#E0D796", "#AF9423", "#A84E0B")
percentile_breaks = c(0, 0.05, 0.1, 0.25, 0.75, 0.9, 0.95, 1)
percentile_labels <- c("Driest", "Drier", "Dry", "Normal","Wet","Wetter", "Wettest")
color_bknd <- "#F4F4F4"
text_color = "#444444"
states_key <- structure(
  list(
    NAME_1 = c(
      "Baden-Württemberg", "Bayern", "Berlin",
      "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
      "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland",
      "Sachsen-Anhalt", "Sachsen", "Schleswig-Holstein", "Thüringen"
    ), state = c(
      "BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV",
      "NI", "NW", "RP", "SL", "ST", "SN", "SH", "TH"
    )
  ),
  row.names = c(
    NA,
    -16L
  ), class = c("tbl_df", "tbl", "data.frame"), sf_column = "geometry", agr = structure(c(
    ID_0 = NA_integer_,
    ISO = NA_integer_, NAME_0 = NA_integer_, ID_1 = NA_integer_,
    NAME_1 = NA_integer_, TYPE_1 = NA_integer_, ENGTYPE_1 = NA_integer_,
    NL_NAME_1 = NA_integer_, VARNAME_1 = NA_integer_
  ), class = "factor", .Label = c(
    "constant",
    "aggregate", "identity"
  ))
)

theme_flowfacet <- function(base = 12, color_bknd, text_color){
  theme_classic(base_size = base) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12, vjust = 1, color = text_color),
          strip.placement = "inside",
          strip.background.x = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.spacing.x = unit(-2, "pt"),
          panel.spacing.y = unit(-5, "pt"),
          plot.margin = margin(0, 0, 0, 0, "pt"),
          legend.box.background = element_rect(fill = color_bknd, color = NA))
  
}

# Import ------------------------------------------------------------------

path_data_gwl_prep <- paste(
  sep = "/",
  "J:", "NUTZER", "Noelscher.M", "Studierende", "Daten", 
  "groundwater_levels", "germany", "mixed_daily_to_monthly", 
  "tabular", "bgr", "data", "processed_timeseries_as_feather", 
  "groundwater_levels_as_feather")

path_data_germany <- paste(
  sep = "/",
  "J:", "NUTZER", "Noelscher.M", "Studierende", "Daten", 
  "administrative_borders", "germany", "time_invariant", 
  "shape", "divagis_gadm", "data", "DEU_adm1.shp"
)

data_gwl <- read_feather(path_data_gwl_prep)
data_germany <- read_sf(path_data_germany)


# Preparation -------------------------------------------------------------

data_gwl_prep <- data_gwl |> 
  mutate(state = str_sub(proj_id, end = 2), .after = proj_id) |> 
  select(-kommentar, -name) |> 
  rename(gwl = nn_messwert, date = datum) |> 
  drop_na(gwl)

data_gwl_prep <- data_gwl_prep |> 
  filter(
    between(
      year(date),
      1990,
      year
      )
  )

data_gwl_prep <- data_gwl_prep |> 
  group_by(proj_id) |> 
  filter(n() >= (year - 1 - 1990) * 52 * 0.9) |> 
  mutate(
    month = month(date),
    week = week(date), .after = date
    )


## Reference table ---------------------------------------------------------

data_gwl_prep_ref <- data_gwl_prep |> 
  data.table::as.data.table() |> 
  filter(between(year(date), 1990, year - 1)) |> 
  group_by(week, proj_id, state) |> 
  summarise(
    across(
      gwl, 
      quantile, 
      probs = percentile_breaks
      ),
    week = first(week),
    .groups = "drop"
    ) |> 
  as_tibble()


data_gwl_prep_ref <- data_gwl_prep_ref |> 
  # rename(value = gwl_quantile) |> 
  # filter( == "10") |> 
  group_by(week, proj_id, state) |> 
  mutate(percentile = percentile_breaks) |> 
  filter(percentile != 0) |> 
  mutate(
    percentile_bin = cut(percentile, breaks = percentile_breaks, include.lowest = TRUE),
    percentile_cond = factor(percentile_labels)
    ) |> 
  ungroup()

wells_remove <- data_gwl_prep_ref |> 
  group_by(proj_id, week) |> 
  summarise(n_unique = gwl |> unique() |> length()) |> 
  filter(n_unique != 7) |> 
  distinct(proj_id)

data_recent <- data_gwl_prep |> 
  ungroup() |> 
  filter(year(date) == year) |> 
  filter(!(proj_id %in% wells_remove$proj_id))

data_join <- data_recent |> 
  select(-state, -date, -month) |> 
  rename(gwl_recent = gwl) |> 
  left_join(data_gwl_prep_ref, by = c("proj_id", "week"))

data_recent_bin <- data_join |> 
  group_by(proj_id, week) |> 
  mutate(cond = cut(gwl_recent, breaks = c(-1000, gwl), labels = percentile_cond)) |> 
  ungroup() |> 
  filter(!(proj_id == "HB_207"))

data_recent_bin <- data_recent_bin |> 
  group_by(proj_id, week) |> 
  filter(percentile_cond == cond)

plot_data <- data_recent_bin |> 
  ungroup() |> 
  group_by(state, week) |> 
  mutate(n_state = unique(proj_id) |> length()) |> 
  group_by(state, cond, week) |> 
  summarise(
    n_cond = n(),
    n_state = first(n_state),
    .groups = "drop"
    ) |> 
  mutate(perc_cond = n_cond / n_state) |> 
  rename(code_state = state)
  
plot_data <- plot_data |> 
# filter(code_state == "HH") |>
  expand(code_state, week, cond) |> 
    left_join(plot_data, by = c("code_state", "week", "cond")) |> 
  arrange(code_state, cond, week) |>
  replace_na(list(perc_cond = 0.0001))
# grid_germany <- data_germany |> 
#   left_join(states_key) |> 
#   select(NAME_1, state) |> 
#   # filter(state %in% (plot_data$state |> unique())) |> 
#   grid_auto()
# 
# plot_data |> 
#   rename(code_state = state) |> 
#   ggplot(aes(week, perc_cond)) +
#   with_shadow(
#     geom_area(aes(fill = cond)),
#     colour = "black",
#     x_offset = 2,
#     y_offset = 2,
#     sigma = 5,
#     stack = TRUE,
#     with_background = FALSE
#   ) +
#   scale_fill_manual(values = rev(pal_wetdry)) +
#   facet_geo(~code_state, grid = grid_germany, move_axes = FALSE) +
#   scale_y_continuous(trans = "reverse") +
#   theme_flowfacet(base = 12, color_bknd, text_color) +
#   theme(plot.margin = margin(50, 50, 50, 50, "pt"),
#         panel.spacing.y = unit(-5, "pt"),
#         panel.spacing.x = unit(4, "pt"),
#         strip.text = element_text(vjust = -1),
#         legend.position = 'none'
#   ) +
#   coord_fixed(ratio = 28)

# test |> ggsave("test.png", device = "png")


grid_germany <- data.frame(
  row = c(1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5),
  col = c(3, 2, 4, 3, 2, 5, 1, 3, 4, 2, 4, 3, 5, 1, 4, 3),
  code_state = c("SH", "HB", "MV", "HH", "NI", "BE", "NW", "ST", "BB", "RP", "TH", "HE", "SN", "SL", "BY", "BW"),
  stringsAsFactors = FALSE
) |> 
  left_join(states_key, by = c("code_state" = "state")) |> 
  rename(name_state = NAME_1) |> 
  as_tibble()

plot_states <- plot_data |>  
  # rename(code_state = state) |>
  # filter(code_state == "HH") |>
  ggplot(aes(week, perc_cond)) +
  with_shadow(
    geom_area(aes(fill = cond)),
    colour = "black",
    x_offset = 2,
    y_offset = 2,
    sigma = 5,
    stack = TRUE,
    with_background = FALSE
  ) +
  scale_fill_manual(values = rev(pal_wetdry)) +
  facet_geo(~code_state, grid = grid_germany, move_axes = FALSE) +
  scale_y_reverse() +
  # scale_y_continuous(trans = "reverse") +
  theme_flowfacet(base = 12, color_bknd, text_color) +
  theme(plot.margin = margin(50, 50, 50, 50, "pt"),
        panel.spacing.y = unit(-5, "pt"),
        panel.spacing.x = unit(4, "pt"),
        strip.text = element_text(vjust = -1),
        legend.position = 'none'
  ) +
  coord_fixed(ratio = 28)  

plot_data_national <- data_recent_bin |> 
  ungroup() |> 
  group_by(week) |> 
  mutate(n_national = unique(proj_id) |> length()) |> 
  group_by(cond, week, percentile_bin) |> 
  summarise(
    n_cond = n(),
    n_national = first(n_national),
    .groups = "drop"
  ) |> 
  mutate(prop = n_cond / n_national) |> 
  rename(percentile_cond = cond)

plot_national <- plot_data_national |> 
  plot_national_area(pal_wetdry, color_bknd)


combine_plots(file_svg = "flow_cartogram", 
              plot_left = plot_national, 
              plot_right = plot_states, 
              width = 16, height = 9, color_bknd)
