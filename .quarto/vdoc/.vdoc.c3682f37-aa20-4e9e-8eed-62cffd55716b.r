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
nba_recruits <- read_excel("data/nba_recruits.xlsx")
glimpse(nba_recruits)
#
#
#
summary(
  select(
    nba_recruits,
    rank,
    nba_mean_ws48,
    top_mean_wa,
    total_seasons,
    drafted
  )
)
#
#
#
read_excel("data/nba_recruits.xlsx") |>
  count(tier)
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
calendar_resid <- births_adjusted |>
  filter(!(month == 2 & date_of_month == 29)) |>
  group_by(month, date_of_month) |>
  summarise(
    mean_pct_resid = mean(pct_resid),
    .groups = "drop"
  ) |>
  mutate(calendar_date = as.Date(sprintf("2001-%02d-%02d", month, date_of_month)))

calendar_resid
#
#
#
holiday_labels <- tibble(
  calendar_date = as.Date(c("2001-01-01", "2001-11-22", "2001-12-25")),
  holiday = c("New Year's Day", "Thanksgiving", "Christmas Day")
) |>
  left_join(calendar_resid, by = "calendar_date")

ggplot(calendar_resid, aes(x = calendar_date, y = mean_pct_resid)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_label(
    data = holiday_labels,
    aes(x = calendar_date, y = mean_pct_resid, label = holiday),
    inherit.aes = FALSE,
    nudge_y = 0.5
  ) +
  labs(
    x = "Calendar date",
    y = "Mean residual (% of overall mean birth count)"
  )
#
#
#
#
