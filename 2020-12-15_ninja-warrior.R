# TidyTuesday submission for Dec 15, 2020 on Ninja Warrior.
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-15/readme.md

library(jhcutils)
library(mustashe)
library(glue)
library(nakedpipe)
library(ggtext)
library(ggimage)
library(patchwork)
library(tidygraph)
library(ggraph)
library(tidyverse)

theme_set(theme_minimal())
set.seed(0)
dir <- "2020-12-15_ninja-warrior_files"
suppressWarnings(dir.create(dir))
ninja_warrior <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv")


ninja_warrior_graph <- ninja_warrior %.%
  {
    group_by(season, location, round_stage)
    arrange(obstacle_order)
    mutate(
      from = obstacle_name,
      to = dplyr::lead(obstacle_name)
    )
    ungroup()
    filter(!is.na(to))
    select(from, to, season, location, round_stage)
  } %>%
  as_tbl_graph()


seasons <- ninja_warrior %.% {
  filter(!str_detect(location, "Japan"))
  u_pull(season)
}

locations <- ninja_warrior %.% {
  filter(!str_detect(location, "Japan"))
  u_pull(location)
}

set.seed(2)
# location_pal <- randomcoloR::randomColor(length(locations), luminosity = "bright")
location_pal <- c(
  "#ea5da6", "#b8f42c", "#c43a6b",
  "#efed53", "#d31fa0", "#0cb58a",
  "#6130f4", "#ce5a08", "#47e08c",
  "#0cd37a", "#294887", "#ea5d60",
  "#68d32a", "#3cbf11", "#4edd44",
  "#c918bd", "#5533ea", "#a113f9",
  "#e4f252", "#db46d6", "#f40263",
  "#c93932", "#784fb5", "#83ef23",
  "#566de2", "#d123a2", "#4e7bbf"
)
names(location_pal) <- locations

MAIN_FONT <- "Trebuchet MS"

season_graph_plots <- lapply(seasons, function(s) {
  ninja_warrior_graph %E>%
    filter(!str_detect(location, "Japan") & season == !!s) %N>%
    filter(centrality_degree(mode = "all") > 0) %>%
    mutate(label = str_wrap(name, width = 20)) %>%
    ggraph(layout = "sugiyama") +
    geom_edge_link(
      aes(color = location),
      arrow = arrow(
        length = unit(1, "mm"),
        type = "closed"
      ),
      start_cap = circle(1, "mm"),
      end_cap = circle(1, "mm")
    ) +
    geom_node_text(
      aes(label = label),
      size = 1,
      color = "white"
    ) +
    scale_edge_color_manual(
      values = location_pal
    ) +
    theme_graph(base_size = 10, base_family = MAIN_FONT) +
    theme(
      text = element_text(color = "white"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        color = "white",
        size = 10
      ),
      plot.margin = margin(2, 2, 2, 2, "mm"),
      panel.background = element_rect(
        fill = "grey15",
        color = "black"
      ),
      plot.background = element_rect(
        fill = "black",
        color = "black"
      ),
      legend.position = "bottom",
      legend.margin = margin(-3, 0, 0, 0, "mm"),
      legend.title = element_blank(),
      legend.key.width = unit(3, "mm"),
      legend.key.height = unit(2, "mm"),
      legend.text = element_text(size = 8)
    ) +
    labs(title = glue::glue("Season {s}"))
})

description <- paste(
  "Each subplot represents the directed graph of in which obstacles were used over a season.",
  "The nodes are individual obstacles with directed edges connecting successive obstacles.",
  "Each subgraph is a separate series of challenges and is colored by its location.",
  sep = "\n"
)

title_plot <- ggplot(tibble(x = 1)) +
  geom_text(
    x = 0.5, y = 2.7,
    label = "Ninja Warrior\nObstacle Variability",
    color = "white",
    size = 17,
    fontface = "bold",
    family = "Impact",
    lineheight = 0.9
  ) +
  geom_text(
    x = 0.5, y = -1,
    label = description,
    color = "grey90",
    size = 4.6,
    family = MAIN_FONT,
    lineheight = 0.9
  ) +
  scale_y_continuous(
    limits = c(-2, 5),
    expand = c(0, 0)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "black",
      color = "black"
    )
  )

season_graph_plots[[11]] <- title_plot

layout_design <- "
KKA
BCD
EFG
HIJ
"

p <- patchwork::wrap_plots(
  season_graph_plots,
  guides = "keep",
  design = layout_design
)

ggsave(
  filename = here::here(dir, "ninja-warrior-graph.png"),
  plot = p,
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)
