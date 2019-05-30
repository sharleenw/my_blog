library(rclimateca)
library(dplyr)
library(ggplot2)
library(measurements)
library(lubridate)


ec_climate_search_locations("parry sound")

ec_climate_geosearch_locations("parry sound",
                               year = 2015:2018,
                               timeframe = "daily")

parry_sound <- ec_climate_data("PARRY SOUND CCG ON 32128",
                               timeframe = "daily",
                               start = "2002-01-01",
                               end = "2018-12-21")

# parry_sound <- ec_climate_data("WESTERN ISLAND (AUT) ON 7733",
#                                timeframe = "daily",
#                                start = "2017-01-01",
#                                end = "2018-12-01")

parry_sound %>%
  mutate(knots = conv_unit(spd_of_max_gust_km_h, "kph", "knot")) %>%
  ggplot(aes(date, knots, group = year)) +
  geom_line() +
  geom_smooth()


# No real wind speed info
# parry_sound <- ec_climate_data("PARRY SOUND HARBOUR ON 31047",
#                                timeframe = "daily",
#                                start = "2010-01-01",
#                                end = "2013-12-01")




ec_climate_geosearch_locations("parry sound", year = 2018, timeframe = "hourly")
# No real wind speed info
parry_sound_climate <- ec_climate_data("PARRY SOUND CCG ON 32128",
                               timeframe = "hourly",
                               start = "2002-01-01",
                               end = "2018-12-21")

saveRDS(parry_sound_climate, "parry_sound_rclimateca.rds")

parry_sound_climate %>%
  mutate(hour = hour(date_time_local),
         wind_speed_knots = conv_unit(wind_spd_km_h, "kph", "knot"),
         month_names = month(date_time_local, label = TRUE)) %>%
  filter(between(hour, 10, 18),
         wind_speed_knots < 30) %>%
  ggplot(aes(x = month_names, y = wind_speed_knots, group = month)) +
  geom_boxplot()
  