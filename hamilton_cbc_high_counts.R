
## For George Bryant: Top counts for each bird in what year

hamilton_cbc %>%
  group_by(species) %>%
  filter(how_many_counted == max(how_many_counted),
         how_many_counted > 0) %>%
  ungroup() %>%
  select(Species = species,
         Year = year,
         `How many counted` = how_many_counted) %>% 
  arrange(Species, Year) %>%
  write_csv("hamilton_cbc_high_counts.csv")
