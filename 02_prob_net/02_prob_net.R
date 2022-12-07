library(dplyr)
library(ggplot2)
library(patchwork)

data <- rnorm(1:100, mean = 80, sd = 20) |> 
  as_tibble()


p1 <- data |> 
  ggplot(aes(value)) +
  geom_histogram(
    fill = "steelblue3",
    alpha = .5
  ) +
  labs(
    x = "",
    y = "Relative Häufigkeit in %"
  ) +
  theme_light()

p2 <- data |> 
  dplyr::arrange(value) |> 
  dplyr::mutate(value_cum = cumsum(value) / sum(value)) |> 
  ggplot(aes(value, value_cum)) +
  geom_line(
    colour = "steelblue3"
  ) +
  geom_point(
    shape = 21,
    fill = "white",
    colour = "steelblue3"
  ) +
  labs(
    x = "",
    y = "Summe der relativen Häufigkeit in %"
  ) +
  theme_light()

p2

p3 <- data |> 
  ggplot(aes(sample = value)) +
  stat_qq(
    shape = 21,
    fill = "white",
    colour = "steelblue3"
  ) +
  stat_qq_line() +
  labs(
    x = "Theoretische Quantile",
    y = "Sample Quantile"
  ) +
  theme_light()

p3

p4 <- data |> 
  ggplot(aes(sample = value)) +
  geom_hline(
    yintercept = quantile(data$value, probs = c(0.1, 0.9)),
    linetype = "dashed",
    colour = "red"
  ) +
  geom_hline(
    yintercept = quantile(data$value, probs = c(0.5)),
    linetype = "dashed",
    colour = "darkgreen"
  ) +
  stat_qq(
    shape = 21,
    fill = "white",
    colour = "steelblue3"
  ) +
  stat_qq_line() +
  labs(
    x = "Theoretische Quantile",
    y = "Sample Quantile"
  ) +
  theme_light()

p4

(p1 + p2 + p3) / (p4 + patchwork::plot_spacer() + patchwork::plot_spacer())
