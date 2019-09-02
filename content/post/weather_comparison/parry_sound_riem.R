library(riem)
library(dplyr)
library(measurements)
library(summarytools)
library(visdat)
library(lubridate)
library(beepr)
library(ggplot2)

riem_networks()

riem_stations(network = "CA_ON_ASOS") %>% View()


parry_sound_riem <- riem_measures(station = "CXPC", date_start = "2000-01-01", date_end = "2018-12-21")


beep("complete.wav")

parry_sound_riem_cleaned <- parry_sound_riem %>%
  select(date = valid,
         air_temp_f = tmpf,
         wind_dir = drct,
         wind_speed = sknt) %>%
  mutate(air_temp_c = conv_unit(air_temp_f, "F", "C"),
         day = day(date),
         month = month(date),
         month_names = month(date, label = TRUE),
         year = year(date),
         hour = hour(date))

saveRDS(parry_sound_riem_cleaned, "parry_sound_riem.rds")

parry_sound_riem_cleaned %>%
  filter(between(hour, 10, 18),
         wind_speed < 25) %>%
  ggplot(aes(x = month_names, y = wind_speed, group = month)) +
  geom_boxplot() 
