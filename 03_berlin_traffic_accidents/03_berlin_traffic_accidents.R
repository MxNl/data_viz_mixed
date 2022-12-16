library(janitor)
# library(tidygeocoder)
library(sf)
library(tidyverse)

data_traffic <- read_csv2(
  "03_berlin_traffic_accidents/dreijahreskartekp_openoffice.csv",
  skip = 2
  )

data_streets <- read_sf("03_berlin_traffic_accidents/Straßennetz_-_Berlin/Strassenabschnitte.shp") |> 
  clean_names() |> 
  mutate(strassenkl = as.numeric(as.roman(strassenkl))) |> 
  group_by(strassenna) |> 
  summarise(
    strassenkl = median(strassenkl, na.rm = TRUE)
  )

data_traffic <- data_traffic |> 
  clean_names() |> 
  rename(unfallkosten = unfallkosten_in_euro_preisstand_ba_st_2009) |> 
  mutate(unfallkosten = str_remove(unfallkosten, " €")) |> 
  mutate(unfallkosten = str_remove_all(unfallkosten, ",") |> as.numeric()) |> 
  rename(
    accidents = u_insgesamt,
    accidents_with_injured = darunter_u_p,
    accidents_with_heavily_injured = darunter_u_sp,
    accident_main_type = hut,
    accident_main_type_sum = hut_anzahl,
    costs = unfallkosten,
    location = knotenpunkt,
    rank = rang_nach_u_p_u_sp_kosten,
    involved_pedestrians = zu_fuss_gehende_fg,
    involved_cyclists = radfahrende_rf,
    involved_kids = kinder_unter_15j,
    involved_seniors = alte_menschen_65j,
    involved_young_drivers = junge_kfz_fuhrer_18_24j,
  ) 

data_traffic_loc <- data_traffic |> 
  mutate(location = str_squish(location)) |> 
  mutate(location = str_replace_all(location, " ", "_")) |> 
  mutate(location = str_replace_all(location, "_*\\/_", " ")) |> 
  tidyr::separate(location, sep = " ", into = paste0("location", 1:6)) |> 
  # select(contains("location")) |> 
  pivot_longer(contains("location")) |> 
  mutate(value = str_replace_all(value, "_", " ")) |> 
  mutate(value = str_to_title(value)) |> 
  mutate(value = str_replace_all(value, "str.", "straße")) |> 
  mutate(value = str_replace_all(value, "Str.", "Straße")) |> 
  pivot_wider() |> 
  relocate(bezirk, contains("location"))

data_streets |> 
  select(strassenna) |> 
  # filter(str_detect(strassenna, "Frankfurter"))
  filter(
    str_detect(strassenna, word(data_traffic_loc$location1[2])) |
    str_detect(strassenna, word(data_traffic_loc$location2[2]))
    ) |> 
  st_intersection()

data_traffic_loc |>
  slice_head(n = 1) |> 
  left_join(select(data_streets, strassenna, geometry), by = c("location1" = "strassenna")) |> 
  select(geometry)
  ggplot() +
  geom_sf()

data_streets |> 
  select(strassenna) |> 
  filter(str_detect(strassenna, "Ammbrücke"))
  
data_streets |> 
  # slice_head(n = 10000) |> 
  ggplot() +
  # geom_sf(aes(lwd = as.factor(strassenkl))) +
  geom_sf(
    lwd = ifelse(data_streets$strassenkl == 1, 5, 0.01)
  ) +
  # scale_size_manual(values = c(0.1, 0.5, 0.7, 1, 5)) +
  # scale_color_viridis_c() +
  theme_minimal()
  
  
data_traffic |> 
  select(location) |> 
  mutate(location = str_squish(location)) |> 
  mutate(location = str_replace_all(location, " ", "_")) |> 
  mutate(location = str_replace_all(location, "_*\\/_", " ")) |> 
  mutate(words = str_count(location, " ") + 1) |> 
  arrange(-words)

data_plot <- data_traffic |> 
  group_by(bezirk) |> 
  summarise(
    across(c(where(is_numeric), -rank), sum),
    rank = mean(rank)
    ) |> 
  pivot_longer(-bezirk) |>
  group_by(name) |> 
  arrange(-value) |> 
  ungroup() |> 
  mutate(value = value / 3)

data_plot |> 
  group_by(name) |> 
  group_split() |> 
  map(
    ~ .x |> 
  ggplot(aes(value, reorder(bezirk, value))) +
  geom_col(
    fill = "steelblue",
    show.legend = FALSE
  ) +
    gghighlight::gghighlight(
      bezirk == "WD",
      unhighlighted_params = list(fill = NULL, alpha = .3)
      ) +
  scale_fill_viridis_c() +
    labs(
      y = "Bezirk",
      x = .x$name |> unique()
    ) +
  # facet_wrap(~name, scales = "free") +
  theme_minimal()
    ) |> 
  patchwork::wrap_plots()


data_traffic |>
  ggplot(aes(accidents_with_injured, involved_pedestrians)) +
  geom_jitter()

data_traffic |> 
  select(where(is_numeric)) |> 
  GGally::ggpairs()
