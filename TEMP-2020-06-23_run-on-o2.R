library(mustashe)
library(glue)
library(magrittr)
library(lubridate)
library(tidyverse)
library(rstanarm)
library(conflicted)

options(dplyr.summarise.inform = FALSE)
memoise_cache <- memoise::cache_filesystem("./.memoise")


conflict_prefer("setdiff", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

blue <- "#5eafe6"
red <- "#eb5e60"
light_grey <- "grey80"
grey <- "grey50"
dark_grey <- "grey25"




caribou_locations <- read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv"
) %>%
    janitor::clean_names()






# Get the distance between each longitude and latitude point (in meters).
get_distance_between_event <- function(lng, lat) {
    dist_traveled <- c(0)
    for (i in seq(2, length(lat))) {
        d <- geosphere::distm(c(lng[[i-1]], lat[[i-1]]),
                              c(lng[[i]], lat[[i]]),
                              fun = geosphere::distHaversine)
        dist_traveled <- c(dist_traveled, d)
    }
    return(dist_traveled)
}
get_distance_between_event <- memoise::memoise(get_distance_between_event,
                                               cache = memoise_cache)


caribou_locations %<>%
    arrange(animal_id, timestamp) %>%
    group_by(animal_id) %>%
    filter(n() > 1) %>%
    mutate(dist_traveled = get_distance_between_event(longitude, latitude)) %>%
    ungroup()





# The duration (in hours) between each successive timestamp value.
get_duration_between_event <- function(ts) {
    as.numeric(ts - dplyr::lag(ts, n = 1, default = ts[[1]])) / (1810800)
}
get_duration_between_event <- memoise::memoise(get_duration_between_event,
                                               cache = memoise_cache)


caribou_locations %<>%
    arrange(animal_id, timestamp) %>%
    group_by(animal_id) %>%
    mutate(diff_in_timestamp = get_duration_between_event(timestamp)) %>%
    ungroup()




caribou_locations %<>%
    mutate(speed = dist_traveled / diff_in_timestamp)





d <- caribou_locations %>%
    group_by(animal_id) %>%
    slice(-1) %>%
    filter(n() >= 50) %>%
    ungroup() %>%
    select(animal_id, event_id, season, dist_traveled, diff_in_timestamp, speed) %>%
    mutate(event_id = as.character(event_id),
           summer = as.numeric(season == "Summer"),
           speed = log10(speed)) %>%
    filter(is.finite(speed))



stash("caribou_speed_m1", {
    caribou_speed_m1 <- stan_glm(
        speed ~ 1 + summer, 
        data = d,
        prior_intercept = normal(),
        prior = normal(),
        cores = 4
    )
})

stash("caribou_speed_m2", {
    caribou_speed_m2 <- stan_lmer(
        speed ~ 1 + season + (1 | animal_id),
        data = d,
        prior_intercept = normal(),
        prior = normal(),
        prior_aux = exponential(),
        prior_covariance = decov(),
        cores = 4
    )
})

stash("caribou_speed_m3", {
    caribou_speed_m3 <- stan_lmer(
        speed ~ 1 + season + (1 + season | animal_id),
        data = d,
        prior_intercept = normal(0, 10),
        prior = normal(0, 2),
        prior_aux = exponential(),
        prior_covariance = decov(),
        cores = 4
    )
})
