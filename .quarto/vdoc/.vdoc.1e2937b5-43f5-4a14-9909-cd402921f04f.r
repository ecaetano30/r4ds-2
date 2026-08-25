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
christmas_data <- births_tibble |>
  filter(month == 12, date_of_month %in% c(20:25, 27:30)) |>
  group_by(year) |>
  summarise(
    christmas_births = births[date_of_month == 25],
    christmas_weekday = day_of_week[date_of_month == 25],
    baseline_births = mean(births[date_of_month != 25]),
    pct_of_baseline = 100 * christmas_births / baseline_births,
    .groups = "drop"
  )

christmas_data
#
#
#
summary(christmas_data)
#
#
#
births_by_calendar_position <- births_tibble |>
  group_by(month, date_of_month) |>
  summarise(
    average_births = mean(births),
    .groups = "drop"
  )

births_by_calendar_position
#
#
#
births_by_calendar_position |>
  mutate(month = factor(month, levels = 1:12, labels = month.name)) |>
  ggplot(aes(x = date_of_month, y = month, fill = average_births)) +
  geom_tile() +
  scale_y_discrete(limits = rev(month.name)) +
  scale_fill_viridis_c() +
  labs(
    x = "Day of month",
    y = "Month",
    fill = "Mean births"
  )
#
#
#
ggplot(christmas_data, aes(x = year, y = pct_of_baseline)) +
  geom_line() +
  geom_point(aes(color = christmas_weekday)) +
  labs(
    x = "Year",
    y = "December 25 births (% of baseline)",
    color = "Weekday"
  )
#
#
#
births_model <- lm(
  births ~ year + month + day_of_week,
  data = births_tibble
)

summary(births_model)$r.squared
#
#
#
births_adjusted <- births_tibble |>
  mutate(pct_resid = 100 * resid(births_model) / mean(births))

glimpse(births_adjusted)
#
#
#
#
