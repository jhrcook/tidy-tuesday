# Data cleaning script.
# source: https://www.census.gov/library/publications/2020/demo/p60-270.html

library(jhcutils)
library(mustashe)
library(nakedpipe)
library(magrittr)
library(tidyverse)

files_dir <- "2020-12-29_usa-household-income_files"
data_path <- here::here(
  files_dir,
  "Table A-2. Households by Total Money Income, Race, and Hispanic Origin of Householder 1967 to 2019.xlsx"
)
raw_data <- xlsx::read.xlsx2(data_path, sheetIndex = 1)

annotations <- raw_data[seq(357, nrow(raw_data)), 1] %.% {
  as_tibble()
  separate(value, into = c("annotation", "information"), sep = " ", extra = "merge")
}

data <- raw_data
column_names <- unname(unlist(data[4, ])) %>% janitor::make_clean_names()
column_names[1:2] <- c("race_and_year", "number")
column_names[13:16] <- c(
  "estimated_median", "estimated_median_error",
  "estimated_mean", "estimated_mean_error"
)

data <- data[5:353, ]
colnames(data) <- column_names
data <- as_tibble(data)


## Clean the data frame.

clean_data <- data
current_race <- NA
race_column <- rep(NA_character_, nrow(clean_data))
for (i in seq(1, nrow(clean_data))) {
  if (clean_data$number[[i]] == "") {
    current_race <- clean_data$race_and_year[[i]]
  } else {
    race_column[i] <- current_race
  }
}

clean_data$race <- race_column

clean_data <- clean_data %.% {
  filter(!is.na(race))
  rename(year = race_and_year)
  separate(year, into = c("year", "year_annotation"), sep = " ")
  mutate(
    year = as.numeric(year),
    race_annotation = str_extract(race, "[:digit:]+$"),
    race = str_remove(race, " [:digit:]+$")
  )
  relocate(race, race_annotation, .after = year_annotation)
  select(-total)
  mutate(
    number = as.numeric(number),
    across(
      starts_with("under") | starts_with("x") | contains("estimated"),
      as.numeric
    )
  )
}

## Final data frames.

average_income <- clean_data %>%
  select(year:race_annotation, estimated_median:estimated_mean_error)

income_by_race <- clean_data %.% {
  select(-c(estimated_median:estimated_mean_error))
  pivot_longer(-c(year:race_annotation, number), names_to = "income", values_to = "percent")
  mutate(
    income = str_remove(income, "^x_|^x"),
    income = str_replace_all(income, "_", " ")
  )
}

write_csv(annotations, here::here(files_dir, "annotations.csv"))
write_csv(average_income, here::here(files_dir, "average_income.csv"))
write_csv(income_by_race, here::here(files_dir, "income_by_race.csv"))
