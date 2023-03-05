#> title: Friends
#> author: Joshua Cook
#> date: August 11, 2020


library(mustashe)
library(jhcutils)
library(glue)
library(magrittr)
library(patchwork)
library(ggridges)
library(tidyverse)
library(ggtext)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("setdiff", "dplyr")
conflict_prefer("lag", "dplyr")

theme_set(theme_minimal())

# To shut-up `summarise()`.
options(dplyr.summarise.inform = FALSE)

set.seed(0)

save_dir <- "2020-09-08_friends_files"

friends <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv")
friends_emotions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv")
friends_info <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv")

emotion_direction <- tibble(
    emotion = c("Joyful", "Peaceful", "Powerful",
                "Neutral",
                "Sad", "Mad", "Scared"),
    direction = c(1, 1, 1, 1, -1, -1, -1)
)

emotion_pal <- c(
    rev(RColorBrewer::brewer.pal(6, "Blues")[2:5]),
    RColorBrewer::brewer.pal(5, "Reds")[2:4]
)
names(emotion_pal) <- emotion_direction$emotion


emotion_data <- friends %>%
    inner_join(friends_emotions, 
               by = c("season", "episode", "scene", "utterance")) %>%
    mutate(idx = glue("season: {season}, episode: {episode}")) %>%
    arrange(season, episode) %>%
    mutate(idx = fct_inorder(idx)) %>%
    count(idx, season, episode, emotion, name = "emotion_ct") %>%
    group_by(idx, season, episode) %>%
    mutate(emotion_frac = emotion_ct / sum(emotion_ct)) %>%
    ungroup() %>%
    left_join(emotion_direction, by = "emotion") %>%
    mutate(emotion_ct = emotion_ct * direction,
           emotion_frac = emotion_frac * direction,
           emotion = factor(emotion, levels = names(emotion_pal)),
           season_lbl = glue("Season {season}"))

emotions_summary <- emotion_data %>%
    arrange(idx) %>%
    filter(emotion != "Neutral") %>%
    group_by(emotion) %>%
    mutate(smooth_emotion_frac = as.numeric(smooth(emotion_frac)),
           smooth_emotion_ct = as.numeric(smooth(emotion_ct))) %>%
    ungroup()


background_clr <- "#d7d9de"
panel_clr <- "#e8e9ed"
grid_line_clr <- "#f0f1f5"

medium_blue_text <- "#616575"
dark_blue_text <- "#3d404a"

emotion_data %>%
    filter(emotion != "Neutral") %>%
    ggplot(aes(x = idx)) +
    facet_grid(. ~ season_lbl, space = "free_x", scales = "free_x") +
    geom_hline(yintercept = 0, size = 0.8, color = grid_line_clr) +
    geom_line(aes(y = emotion_frac, color = emotion, group = emotion), 
              alpha = 0.1) +
    geom_point(aes(y = emotion_frac, color = emotion)) +
    geom_line(aes(y = smooth_emotion_frac, color = emotion, group = emotion),
              data = emotions_summary) +
    scale_color_manual(values = emotion_pal, 
                       guide = guide_legend(nrow = 1,
                                            label.position = "top",
                                            label.vjust = -4,
                                            keywidth = 2)) +
    scale_y_continuous(breaks = round(seq(-0.3, 0.4, 0.1), 1)) +
    theme_minimal(base_size = 11, base_family = "Helvetica") +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = grid_line_clr, lineend = "round"),
          panel.grid.minor.y = element_line(color = grid_line_clr, lineend = "round"),
          panel.background = element_rect(fill = background_clr, color = NA),
          plot.background = element_rect(fill = background_clr, color = NA),
          strip.text = element_text(face = "bold", color = dark_blue_text),
          axis.title = element_text(color = medium_blue_text),
          axis.text = element_text(color = medium_blue_text),
          legend.text = element_text(color = dark_blue_text),
          legend.position = c(0.75, 1),
          legend.box.margin = margin(0, 0, 70, 0),
          plot.title = element_markdown(color = dark_blue_text, 
                                        face = "bold",
                                        family = "Impact",
                                        size = 20)) +
    labs(x = NULL,
         y = "fraction of emotion in episode",
         color = NULL,
         title = "Emotional sentiment in \"Friends\"")
ggsave(file.path(save_dir, "friends-emotions.png"),
       width = 8, height = 5, dpi = 600)
