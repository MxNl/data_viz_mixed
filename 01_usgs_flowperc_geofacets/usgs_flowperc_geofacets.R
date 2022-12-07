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

year_to_plot <- 2015
year_ref_period_start <- 1980
year_ref_period_end <- 2010
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
grid_germany <- data.frame(
  row = c(1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5),
  col = c(3, 2, 4, 3, 2, 5, 1, 3, 4, 2, 4, 3, 5, 1, 4, 3),
  code_state = c("SH", "HB", "MV", "HH", "NI", "BE", "NW", "ST", "BB", "RP", "TH", "HE", "SN", "SL", "BY", "BW"),
  stringsAsFactors = FALSE
) |> 
  left_join(states_key, by = c("code_state" = "state")) |> 
  rename(name_state = NAME_1) |> 
  as_tibble()


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
data_germany <- read_sf(path_data_germany) |> 
  left_join(states_key)


# Preparation -------------------------------------------------------------
# start function

data_gwl_prep <- data_gwl

data_gwl_prep <- data_gwl_prep |> 
  mutate(state = str_sub(proj_id, end = 2), .after = proj_id) |> 
  select(-kommentar, -name) |> 
  rename(gwl = nn_messwert, date = datum, well_id = proj_id) |> 
  drop_na(gwl) |> 
  mutate(
    year = year(date),
    month = month(date),
    week = week(date), .after = date
    ) 
  # rough filter on time
  # filter(
  #   between(
  #     year,
  #     year_ref_period_start,
  #     year_to_plot
  #     )
  # ) |> 
  # filter_min_num_weeksamples()



  # only keep wells with more than 90% of non-missing data
  # filter(n() >= (year_to_plot - 1 - 1980) * 52 * 0.9) |> 


## Reference table ---------------------------------------------------------

data_ref <- data_gwl_prep |> 
  # truncate time series to reference period
  generate_reference_table()


# Iteration over years ----------------------------------------------------
# import fonts
font_legend <- 'Noto Sans Mono'
font_add_google(font_legend)
sysfonts::font_add("sand", "fonts/Angeline Vintage_Demo.ttf")
showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
showtext_auto(enable = TRUE)

data_germany_simple <- data_germany |>
  left_join(states_key) |>
  rmapshaper::ms_simplify(keep = 0.0008, keep_shapes = FALSE) |> 
  # filter(!(state %in% c("HH", "HB", "BE"))) |> 
  select(state)
  

plot_overviewmap <-
  data_germany_simple |>
  ggplot() +
  geom_sf(
    fill = color_bknd,
    colour = text_color,
    size = 5,
    inherit.aes = FALSE
  ) +
  # geom_sf(
  #   data = st_cast(data_germany_simple, "MULTILINESTRING"),
  #   colour = text_color,
  #   lwd = 5
  # ) +
  geom_sf_text(
    data = data_germany,
    aes(label = state),
    colour = text_color,
    size = 1
  ) +
  theme_void() +
  theme(
    text = element_text(family = font_legend, face = "bold")
  )

for (year_to_plot in 2011:2018) {
  data_recent <- data_gwl_prep |>
    filter(year == year_to_plot) |>
    filter_min_weeks_year_to_plot(year_to_plot)

  data_recent_bin <- data_recent |>
    select(-state, -date, -month) |>
    rename(gwl_recent = gwl) |>
    inner_join(data_ref, by = c("well_id", "week")) |>
    group_by(well_id, week) |>
    # group_split() |> chuck(20)
    filter(gwl |> unique() |> length() == 7) |>
    mutate(cond = cut(gwl_recent, breaks = c(-1000, gwl), labels = percentile_cond)) |>
    ungroup() |>
    # Remove clearly noisy time series identified after visual inspection
    filter(!(well_id == "HB_207")) |>
    group_by(well_id, week) |>
    filter(percentile_cond == cond)

  plot_data <- data_recent_bin |>
    group_by(state) |> 
    filter(n() > 500) |> 
    ungroup() |>
    group_by(state, week) |>
    mutate(n_state = unique(well_id) |> length()) |>
    group_by(state, cond, week) |>
    summarise(
      n_cond = n(),
      n_state = first(n_state),
      .groups = "drop"
    ) |>
    mutate(perc_cond = n_cond / n_state) |>
    rename(code_state = state)

  plot_data <- plot_data |>
    expand(code_state, week, cond) |>
    left_join(plot_data, by = c("code_state", "week", "cond")) |>
    arrange(code_state, cond, week) |>
    replace_na(list(perc_cond = 0)) |> 
    # Add missing states explicitly
    full_join(states_key |> select(state), by = c("code_state" = "state"))
  
  plot_states <- plot_data |>
    make_plot_states(grid_germany, color_bknd, pal_wetdry, text_color)

  plot_data_national <- data_recent_bin |>
    ungroup() |>
    group_by(week) |>
    mutate(n_national = unique(well_id) |> length()) |>
    group_by(cond, week, percentile_bin) |>
    summarise(
      n_cond = n(),
      n_national = first(n_national),
      .groups = "drop"
    ) |>
    mutate(prop = n_cond / n_national) |>
    rename(percentile_cond = cond)
  
  plot_data_national <- plot_data_national |> 
    expand(week, percentile_bin) |>
    left_join(plot_data_national, by = c("week", "percentile_bin")) |>
    replace_na(list(prop = 0))

  plot_national <- plot_data_national |>
    make_plot_national_area(pal_wetdry, color_bknd)


  combine_plots(
    file_svg = str_glue("gwlevel_cartogram_{year_to_plot}"),
    plot_left = plot_national,
    plot_right = plot_states,
    plot_overviewmap,
    width = 16, height = 9, color_bknd,
    year_to_plot
  )
}
