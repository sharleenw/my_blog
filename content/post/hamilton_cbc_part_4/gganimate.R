library(dplyr)
library(here)
library(readr)
library(gganimate)
library(ggplot2)

hamilton_cbc <- read_rds(here("content",
                              "post",
                              "hamilton_cbc_part_3",
                              "hamilton_cbc_shiny",
                              "hamilton_cbc_output_part_2.rds"))


theme_set(theme_minimal())

# Every year had at least 9 species
hamilton_cbc %>% 
  group_by(year) %>% 
  filter(how_many_counted > 0) %>%
  summarise(n_counted = n()) %>%
  arrange(n_counted)

hamilton_cbc_ranked <- hamilton_cbc %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding (because it makes rank a double, not integer variable)
  arrange(year, -(how_many_counted), species) %>%
  mutate(rank = row_number() * 1) %>%
  ungroup() %>%
  filter(rank < 8,
         year >= 1955)

p <- hamilton_cbc_ranked %>% 
  ggplot(aes(rank, group = species, 
                     fill = as.factor(species), color = as.factor(species))) +
  geom_tile(aes(y = how_many_counted / 2,
                height = how_many_counted,
                width = 0.9), alpha = 0.8, color = NA) +
  
  # text in x-axis (requires clip = "off" in coord_*)
  # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1 
  #   leads to weird artifacts in text spacing.
  geom_text(aes(y = 0, label = paste(species, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = how_many_counted, label = paste0(" ", how_many_counted), hjust = 0)) + # value label thanks to Nitish
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='{closest_state}', x = "", y = "Number of birds counted") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

# Have to install the gifski package in order for the renderer to work
# Make fps 25 to make it smoother
# Duration is how many centiseconds between years
animate(p, fps = 25, duration = 80, width = 800, height = 600, renderer = gifski_renderer())
