
library(jhcutils)
library(mustashe)
library(glue)
library(nakedpipe)
library(gganimate)
library(tidyverse)

theme_set(theme_minimal())

set.seed(0)

datasaurus <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv")


ggplot(datasaurus, aes(x = x, y = y, color = dataset)) +
  facet_wrap(~dataset, ncol = 3) +
  geom_point() +
  theme(legend.position = "none")


dino <- datasaurus %>%
  filter(dataset == "dino") %>%
  select(x, y)
slant_down <- datasaurus %>%
  filter(dataset == "slant_down") %>%
  select(x, y)

summarise_data <- function(df) {
  tibble(
    `X Mean` = mean(df$x), `Y Mean` = mean(df$y),
    `X SD` = mean(df$x), `Y SD` = mean(df$y),
    `Corr.` = cor(df$x, df$y)
  ) %>% knitr::kable(format = "simple", digits = 5)
}
summarise_data(dino)
summarise_data(slant_down)








dist_between_points <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

compare_all_positions <- function(df1, df2) {
  point_dists <- matrix(0, nrow = nrow(df1), ncol = nrow(df2))

  for (i in seq(1, nrow(df1))) {
    x_i <- df1$x[[i]]
    y_i <- df1$y[[i]]
    for (j in seq(1, nrow(df2))) {
      x_j <- df2$x[[j]]
      y_j <- df2$y[[j]]
      point_dists[i, j] <- dist_between_points(x_i, y_i, x_j, y_j)
    }
  }

  return(sum(point_dists))
}


plot_datasaur <- function(d) {
  ggplot(d, aes(x, y)) +
    geom_point()
}


compare_positions_byrow <- function(df1, df2) {
  d <- dist_between_points(df1$x, df1$y, df2$x, df2$y)
  sum(d)
}


perturb_data <- function(df, min, max) {
  df$x <- df$x + runif(length(df$x), min = min, max = max)
  df$y <- df$y + runif(length(df$y), min = min, max = max)
  return(df)
}

calc_summary_stats <- function(df) {
  tibble(
    mean_x = mean(df$x),
    sd_x = sd(df$x),
    mean_y = mean(df$y),
    sd_y = sd(df$y),
    corr = cor(df$x, df$y)
  )
}


acceptable_diff <- function(basis, compare, tol) {
  all(abs(basis - compare) < tol)
}


closer_to_target <- function(target, current, new) {
  target_to_current <- compare_positions_byrow(target, current)
  target_to_new <- compare_positions_byrow(target, new)
  return(target_to_new < target_to_current)
}


data_transition <- function(start,
                            target,
                            tol = 0.01,
                            step_size = 0.3,
                            max_iter = 1e6,
                            final_tolerance = 50) {
  stopifnot(nrow(start) == nrow(target))

  iter_num <- 0
  data <- start
  original_stats <- calc_summary_stats(data)
  target_diff <- compare_positions_byrow(start, target)

  stats_collection <- rep(list(tibble()), iter_num / 500)
  position_collection <- rep(list(tibble()), iter_num / 500)
  stats <- stats_collection

  while (iter_num < max_iter) {
    iter_num <- iter_num + 1

    p_data <- perturb_data(data, -step_size, step_size)
    p_stats <- calc_summary_stats(p_data)

    if (acceptable_diff(p_stats, original_stats, tol = tol)) {
      if (closer_to_target(target, data, p_data)) {
        data <- p_data
        stats <- p_stats
        target_diff <- compare_positions_byrow(target, data)
      }
    }

    if (iter_num %% 500 == 0) {
      stats_collection[[iter_num / 500]] <- stats %>% add_column(i = iter_num)
      position_collection[[iter_num / 500]] <- data %>% add_column(i = iter_num)
    }

    if (target_diff < final_tolerance) {
      break
    }
  }
  
  

  return(list(
    final_data = data,
    stats = bind_rows(stats_collection),
    positions = bind_rows(position_collection),
    final_target_distance = target_diff,
    final_iter = iter_num
  ))
}



stash("dino_to_slant", {
  dino_to_slant <- data_transition(
    start = dino,
    target = slant_down,
    tol = 0.01,
    step_size = 0.1,
    max_iter = 2e5,
    final_tolerance = nrow(dino) / 3
  )
})

stash("slant_to_dino", {
  slant_to_dino <- data_transition(
    start = dino_to_slant$final_data,
    target = dino,
    tol = 0.01,
    step_size = 0.1,
    max_iter = 2e5,
    final_tolerance = nrow(dino) / 3
  )
})


plot_datasaur(dino_to_slant$final_data)
plot_datasaur(slant_to_dino$final_data)

dino_to_slant$final_target_distance
dino_to_slant$final_iter

slant_to_dino$final_target_distance
slant_to_dino$final_iter


dino_to_slant_to_dino <- bind_rows(
  dino_to_slant$positions,
  slant_to_dino$positions %>% mutate(i = i + max(dino_to_slant$positions$i))
) %>%
  group_by(i) %>%
  nest() %>%
  ungroup() %>%
  mutate(i = row_number() - 1) %>%
  unnest(data)


animation <- dino_to_slant_to_dino %>%
  filter(i %% 10 == 0) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  transition_states(i, state_length = 0, transition_length = 0.05) +
  ease_aes("linear")

anim_save(
  file.path("2020-10-13_datasaurus-dozen_files", "dino-slant-dino.gif"),
  animation = animation
)
