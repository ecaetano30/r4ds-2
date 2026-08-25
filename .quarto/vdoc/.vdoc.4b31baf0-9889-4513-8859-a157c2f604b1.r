#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(readxl)
#
#
#
births <- read_excel("data/us_births_1994_2014.xlsx")
glimpse(births)
#
#
#
summary(select(births, births, year))
#
#
#
#| cache: true
births_tibble <- births |>
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
      ordered = TRUE
    )
  )
#
#
#
#
