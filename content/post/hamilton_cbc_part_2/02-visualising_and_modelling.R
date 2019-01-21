
# Rare birds (count less than 10?)
# Does temperature affect the number of birds seen? Redo this with accurate historical weather data
# Filter by scientific name to get Genus, not just species for plots
# Model number of birds since 1980's
# Shiny app to look at 1: bird counts through the years and 2: lists of what birds were seen each year

library(dplyr)
library(janitor)
library(readr)
library(tidyr)
library(naniar)
library(lubridate)
library(measurements)
library(stringr)
library(ggplot2)
library(purrr)
library(data.table)

hamilton_cbc <- read_rds("01-output.rds")

# Data counting and visualizing! ----

# This person went out for 8 hours in 1921 and saw 9 different species, including a grey jay!
hamilton_cbc %>%
  filter(year == 1921,
         !is.na(how_many_counted)) %>%
  select(species, how_many_counted) %>% View()

# This was the only grey jay ever seen in the Hamilton CBC... Maybe it was a mistake??
hamilton_cbc %>% filter(species == "Gray Jay", !is.na(how_many_counted))

# 25 species were seen this year
hamilton_cbc %>%
  filter(year == 1926,
         !is.na(how_many_counted)) %>%
  select(species, how_many_counted) %>% View()

random_sample_of_species <- hamilton_cbc %>%
  group_by(species) %>%
  nest() %>%
  sample_n(4) %>%
  pull(species)

hamilton_cbc %>% 
  filter(year > 1980,
         species %in% random_sample_of_species) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = species_count_per_hour, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted per hours") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() +
  geom_miss_point()

# I prefer this one (numbers are more "real", even though the y-axis is transformed, and naniar's plot works better on this one)
hamilton_cbc %>% 
  filter(year > 1980,
         species %in% random_sample_of_species) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>%
  ggplot(aes(x = year, y = total_hours)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number of hours") +
  theme_minimal() +
  geom_miss_point()

hamilton_cbc %>%
  ggplot(aes(x = year, y = count_of_birds_seen_in_this_year)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number of birds seen") +
  theme_minimal()

hamilton_cbc %>%
  filter(year > 1978, species != "European Starling") %>%
  ggplot(aes(x = year, y = count_of_birds_seen_in_this_year_per_hours)) +
  geom_point(size = 1) +
  xlab("Year") +
  ylab("Number of birds seen per hour") +
  theme_minimal() +
  geom_smooth(method = "lm", formula = y ~ x)

invasive <- c("European Starling", "House Sparrow", "Rock Pigeon (Feral Pigeon)", "Ring-necked Pheasant")

hamilton_cbc %>% 
  filter(species %in% invasive) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  geom_miss_point() +
  scale_y_log10()

hamilton_cbc %>% 
  filter(year > 1950,
         species == "Eastern Bluebird") %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  # scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Warbler")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  # scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Cat")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  # scale_y_log10() +
  geom_miss_point()
  

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Wren")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  # scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Sparrow"),
         !str_detect(species, "House")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Duck")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         !str_detect(species, "\\("),
         str_detect(species_latin, "Larus")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species_latin, "Aythya"),
         !str_detect(species, "\\(")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() +
  geom_miss_point()

# Getting the genus
# I did the regex myself!! (Just looking at my other code)
hamilton_cbc %>%
  distinct(species_latin) %>%
  mutate(species_latin = str_extract(species_latin, "^.+?(?=\\s)")) %>%
  count(species_latin, sort = TRUE) %>% View("species latin")

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Cormorant")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Eagle")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()


hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Junco")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Long-tailed Duck")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Cardinal")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Chickadee")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(year > 1950,
         str_detect(species, "Owl")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Hawk")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

# Snow buntings had huge peaks in the 1970's
hamilton_cbc %>% 
  filter(str_detect(species, "Snow Bunting")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y =
               how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Meadowlark")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y =
               how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Nuthatch")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y =
               how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Woodpecker")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y =
               how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Goose")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y =
               how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Swan")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y =
               how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Wood Duck")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y =
               how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Grouse")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() # +
  #scale_y_log10() +
  #geom_miss_point()

hamilton_cbc %>% 
  filter(str_detect(species, "Harrier")) %>% # https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
  ggplot(aes(x = year, y = how_many_counted, color = species)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  #scale_y_log10() +
  geom_miss_point()

hamilton_cbc %>%
  filter(!str_detect(species, "Starling")) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(x = year, y = count_of_birds_seen_in_this_year_excl_starlings_per_hour, color = "blue")) +
  geom_line(aes(x = year, y = count_of_birds_seen_in_this_year_per_hour, color = "red")) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() #+
  #scale_y_log10() +
  #geom_miss_point()

hamilton_cbc %>%
  ggplot(aes(x = year)) +
  geom_line(aes(x = year, y = count_of_birds_seen_in_this_year, color = "blue")) +
  geom_line(aes(x = year, y = count_of_birds_seen_in_this_year_per_hour, color = "red")) +
  xlab("Year") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() #+
#geom_miss_point()

# TEmeprature
hamilton_cbc %>%
  ggplot(aes(x = high_temp)) +
  geom_line(aes(y = count_of_birds_seen_in_this_year_per_hour, color = "red")) +
  xlab("High Temperature") +
  ylab("Number counted") +
  labs(color = "Species") +
  theme_minimal() +
  scale_y_log10() #+
#geom_miss_point()

