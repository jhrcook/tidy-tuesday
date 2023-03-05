# 2023-01-24 -- Alone
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-03-07/readme.md


survivalists <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv")
# loadouts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
# episodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
# seasons <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')


season_lbller <- function(data) {
  list(glue::glue("Season {data$season}"))
}

xmin <- min(survivalists$age)
xmax <- max(survivalists$age)
ymin <- min(survivalists$days_lasted)
ymax <- max(survivalists$days_lasted)


reason_category_pal <- c(
  "Family / personal" = "#66c2a5",
  "Loss of inventory" = "#4393c3",
  "Medical / health" = "#9970ab",
  "Winner" = "#f46d43"
)

dataset_desc <- "This dataset contains data from the TV series 'Alone' collected and shared by Dan Oehm. In the survival, ten survivalists are dropped in an extremely remote area and must fend for themselves. They aim to last 100 days in the Artic winter, living off the land through their survival skills, endurance, and mental fortitude. In Season 4, the contestants were in pairs."

p <- survivalists %>%
  mutate(
    result = as.integer(result),
    winner = result == 1,
    shape = ifelse(winner, 23, 21),
    season = factor(as.integer(season)),
    reason_category = ifelse(is.na(reason_category), "Winner", reason_category)
  ) %>%
  ggplot(
    aes(
      x = age,
      y = days_lasted,
      color = reason_category,
      fill = reason_category,
      shape = shape,
    )
  ) +
  facet_wrap(vars(season), labeller = season_lbller, scales = "free") +
  geom_point(size = 2, alpha = 0.9) +
  geom_linerange(
    x = xmin - 4,
    ymin = ymin,
    ymax = ymax,
    data = tibble(x = c(1)),
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  geom_linerange(
    xmin = 20,
    xmax = 60,
    y = ymin - 8,
    data = tibble(x = c(1)),
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  scale_x_continuous(limits = c(xmin, xmax), expand = expansion(add = 4)) +
  scale_y_continuous(limits = c(ymin, ymax), expand = expansion(add = 8)) +
  scale_shape_identity() +
  scale_color_manual(
    values = reason_category_pal,
    guide = guide_legend(nrow = 2, keyheight = unit(5, "mm"))
  ) +
  scale_fill_manual(values = reason_category_pal) +
  theme_minimal(base_size = 11, base_family = "Helvetica") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10, color = "gray25", hjust = 0),
    panel.grid = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    axis.text = element_text(size = 9, color = "gray25"),
    axis.title = element_text(size = 10, color = "gray25"),
    plot.title = element_text(size = 20, color = "gray25"),
    plot.subtitle = element_text(color = "gray55", size = 9),
    legend.title = element_text(color = "gray25", size = 10),
    legend.text = element_text(color = "gray25", size = 10),
    legend.position = c(0.8, 1.15),
  ) +
  labs(
    x = "contestant's age",
    y = "days lasted alone",
    fill = "Quitting reason",
    color = "Quitting reason",
    title = "Results of \"Alone\" contestants by age",
    subtitle = str_wrap(dataset_desc, width = 107)
  )

ggsave(
  here::here("tuesdays", "output-images", "2023-01-24_alone.jpeg"),
  plot = p, 
  width = 10, 
  height = 7, 
  units="in",
  dpi = 400
)
