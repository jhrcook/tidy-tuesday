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
    capacity = sum(capacity),
    occupancy = sum(occupancy)
  )
  ungroup()
  mutate(available_space = capacity - occupancy)
  filter(!is.na(occupancy))
  filter(date_week != max(date_week))
}


#### ---- Plotting ---- ####

ppl_images <- purrr::map(c("child.png", "woman.png", "man.png", "family.png"), ~ here::here(dir, .x))

base_height <- 100
ppl_heights <- c(0.5 * base_height, base_height, base_height, base_height)
sector_to_images <- tibble(sector = sector_levels, image = ppl_images, height = ppl_heights) %>%
  mutate(sector_label = glue("<img src='{image}' height='{height}'>"))

shelter_data_p <- shelters_week_summary %>%
  left_join(sector_to_images, by = "sector") %>%
  mutate(sector_label = factor(sector_label, levels = sector_to_images$sector_label)) %>%
  ggplot() +
  facet_wrap(~sector_label, nrow = 1) +
  geom_point(aes(x = date_week, y = occupancy, color = sector)) +
  theme(
    legend.position = "none",
    strip.text = element_markdown(valign = -1)
  ) +
  labs(
    x = "date",
    y = "number of occupants",
    title = "Toronto Shelter Occupancy"
  )

ggsave(
  filename = "2020-12-01_toronto-shelters.pdf",
  plot = shelter_data_p,
  device = "pdf",
  path = here::here(dir),
  width = 10, height = 6
)



# Family by Wilson Joseph from the Noun Project
# Man by Funky from the Noun Project
# Female by Mushu from the Noun Project
# Child by Musmellow from the Noun Project
