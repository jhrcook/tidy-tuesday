# TidyTuesday submission for Dec 1, 2020 on Toronto Shelters.

library(jhcutils)
library(mustashe)
library(glue)
library(nakedpipe)
library(ggtext)
library(ggimage)
library(patchwork)
library(tidyverse)

theme_set(theme_minimal())
set.seed(0)
dir <- "2020-12-01_toronto-shelters_files"
data_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv"

#### ---- Data preparation ---- ####

shelters <- read_csv(data_url) %.% {
  rename(date = occupancy_date)
  filter(shelter_city == "Toronto")
}

# shelters %>%
#   slice(1:100) %>%
#   View(title = "Shelters sample")

sector_levels <- c("Youth", "Women", "Men", "Families")

shelters_week_summary <- shelters %.% {
  filter(sector %in% sector_levels)
  mutate(date_week = lubridate::floor_date(date, unit = "week", week_start = 7))
  group_by(date_week, sector)
  summarise(
    capacity = mean(capacity),
    occupancy = mean(occupancy)
  )
  ungroup()
  mutate(available_space = capacity - occupancy)
  filter(!is.na(occupancy))
  filter(date_week != max(date_week))
}


#### ---- Plotting ---- ####

ppl_images <- purrr::map(
  c("child.png", "woman.png", "man.png", "family.png"),
  ~ here::here(dir, .x)
)

base_height <- 100
ppl_heights <- c(0.5 * base_height, base_height, base_height, base_height)
sector_to_images <- tibble(
  sector = sector_levels,
  image = ppl_images,
  height = ppl_heights
) %>%
  mutate(sector_label = glue("<img src='{image}' height='{height}'>"))

plot_background_fill <- "slategray3"
axis_text_color <- "white"
grid_color <- "grey95"
title_color <- "white"
anno_color <- "aliceblue"

shelter_data_p <- shelters_week_summary %>%
  left_join(sector_to_images, by = "sector") %>%
  mutate(
    sector_label = factor(sector_label, levels = sector_to_images$sector_label)
  ) %>%
  ggplot(aes(x = date_week, y = occupancy)) +
  facet_wrap(~sector_label, nrow = 1) +
  geom_smooth(
    aes(color = sector),
    method = "loess", formula = "y ~ x",
    se = FALSE,
    size = 0.9
  ) +
  geom_point(
    aes(color = sector),
    alpha = 0.5,
    size = 0.8
  ) +
  scale_color_viridis_d(
    begin = 0.3, end = 0.7, direction = -1, option = "A"
  ) +
  scale_x_datetime() +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.text = element_markdown(valign = -1),
    plot.background = element_rect(fill = plot_background_fill),
    panel.background = element_rect(
      fill = plot_background_fill,
      color = NA
    ),
    panel.border = element_blank(),
    axis.text = element_text(color = axis_text_color),
    axis.title.x = element_blank(),
    axis.title = element_text(color = axis_text_color),
    panel.grid = element_line(color = grid_color),
    panel.spacing.x = unit(0.2, "in"),
    plot.title = element_markdown(
      family = "Helvetica",
      face = "bold",
      color = title_color,
      hjust = 0.03,
      size = 26,
      halign = 1
    )
  ) +
  labs(
    x = "date",
    y = "number of occupants (avg. per week)",
    title = "Toronto<br>Shelter<br>Occupancy"
  )

annotation_text <- paste0(
  "The plots below show the weekly average number of children, women, men, and\n",
  "individuals present in families that occupied Toronto's homeless shelters from 2017\n",
  "through 2020. While the number of homeless children, women, and men remained\n",
  "constant over this period, the number of families steadily increased, reaching a peak\n",
  "in early-2019, followed by a slight decline in the fall and winter."
)

shelter_data_p_anno <- cowplot::ggdraw(shelter_data_p) +
  cowplot::draw_label(
    label = annotation_text,
    x = 0.31, y = 0.907,
    color = anno_color,
    fontfamily = "Helvetica",
    size = 12,
    hjust = 0,
    vjust = 0.5
  ) +
  cowplot::draw_line(
    x = c(0.297, 0.297),
    y = c(0.83, 0.98),
    color = "white", size = 0.4
  )

ggsave(
  filename = "2020-12-01_toronto-shelters.pdf",
  plot = shelter_data_p_anno,
  device = "pdf",
  path = here::here(dir),
  width = 10, height = 6
)

# Family by Wilson Joseph from the Noun Project
# Man by Funky from the Noun Project
# Female by Mushu from the Noun Project
# Child by Musmellow from the Noun Project
