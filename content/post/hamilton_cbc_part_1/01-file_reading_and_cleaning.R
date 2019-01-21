# DAta received from: http://netapp.audubon.org/CBCObservation/Historical/ResultsByCount.aspx
# All years for "Hamilton" were downloaded as a csv
# Look more into documentation to try and figure out metadata meaning
# Don't forget to adhere to this : https://www.audubon.org/content/policy-regarding-use-christmas-bird-count-data
# Documentation: http://docs.audubon.org/sites/default/files/documents/cbc_historical_results_download_file_format.pdf
# What is "cw"?? It is "count week"
# Better documentation: https://www.audubon.org/sites/default/files/documents/compilers_manual_0.pdf
# For each species seen, record the number observed on count day, or enter cw if the species was observed count week only.  The count week runs from three days before to three days after the count day.  The count week can therefore extend outside the official count period; for example if your count is run on 14 December, the first day of the period, your count week is still from three days before to three days after your count (11 through 17 December). 

# Other Flags:  The US (unusual) flag is for species that are out of range or seasonal or are difficult to identify.  The HC (high count) flag is if the number of birds observed is an unusually high count. The LC (Low count) flag is for unusually low numbers. 
# Total Party Hours and Distance:  Totals for Party Hours and Party Miles are calculated automatically on the website.


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

# Importing and doing basic cleaning ----
hamilton_cbc <- read_csv("hamilton-cbc-all-years-csv.csv")

hamilton_cbc <- hamilton_cbc %>%
  clean_names()

current_circle_name <- hamilton_cbc[1, 1]
lat_long <- hamilton_cbc[1, 3]

hamilton_cbc <- hamilton_cbc %>%
  slice(3 : n())

# Getting the meta data tidied ----
meta_data_table <- list()
meta_data_table_headers <- list()


for (i in 1:3) {
  
  meta_data_table_headers[[i]] <- hamilton_cbc[1, ] %>%
    replace(is.na(.), "NA")
    #remove_empty(which = "cols") # we need to have this ready to be placed onto the data after the while loop
  # This assumes if the header is empty, the rest of the column is (and vice versa). Through a manual check, we know that this is true.
  # To make the code more robust (e.g., if a header is NA but there is data underneath it or vice versa), I replaced the NAs in the column headers with character "NA"s, in order to do the rename below.
  
  j <- 2
  meta_data_row <- list()
  
  while (!is.na(hamilton_cbc[["circle_name"]][j])) {
    meta_data_row[[j]] <- hamilton_cbc[j, ]
    j <- j + 1
  }
  
  meta_data_table[[i]] <- meta_data_row %>%
    bind_rows()

  #colnames(meta_data_table[[i]]) <- meta_data_table_headers[[i]] # Non-tidyverse way
  meta_data_table[[i]] <- meta_data_table[[i]] %>%
    rename_all(funs(meta_data_table_headers[[i]])) %>% # order of steps is important, we need to do this before removing the empty columns
    remove_empty(which = "cols") %>% 
    rename(count_year = 1) %>% # rename knows we are referring to the first column!
    clean_names # order of steps is important (we need the new headers in place)

  
  hamilton_cbc <- hamilton_cbc %>%
    slice((j + 1) : n())
}

# Documentation: reduce(list(x1, x2, x3), f) is equivalent to f(f(x1, x2), x3)
# Just doing a full join to be safe!
overall_meta_data <- meta_data_table %>%
  reduce(full_join, by = "count_year")

overall_meta_data <- overall_meta_data %>%
  replace_with_na_all(condition = ~.x == "Unknown")

overall_meta_data <- overall_meta_data %>%
  mutate(date = mdy(low_temp3),
         low_temp = word(low_temp), # Extracts the first word from a string!
         high_temp = word(high_temp),
         year = year(date)) %>%
  select(year, date, count_year, low_temp:pm_snow)
# I am removing high_temp2:pm_clouds2
# I could look into what those variables mean on the CBC website but I will not do that now


# Trying to plot in order the temperatures to see if I can figure out which ones are Celsius and Fahrenheit
overall_meta_data %>%
  ggplot(aes(x = as.double(low_temp), y = as.double(high_temp), color = year)) +
  geom_point() +
  geom_text(aes(label = year), angle = -10, hjust = 0.5, vjust = -0.4) +
  xlab("Day's low temperature (C)") +
  ylab("Day's high temperature (C)") +
  labs(color = "Year") +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "darkblue")

overall_meta_data %>%
  filter(year == 1993) %>%
  select(low_temp) %>%
  as.double() %>%
  conv_unit("F", "C")
# 1993 daily temp according to StatsCan was -21 to -13 deg C, so 1993 is in fact in Fahrenheit
# http://climate.weather.gc.ca/historical_data/search_historic_data_e.html

# I could check all of the temperatures but I think it's an okay assumption to make based on the above plot that all years prior to 2011 were in Fahrenheit

overall_meta_data <- overall_meta_data %>%
  mutate(low_temp = ifelse(year < 2011, conv_unit(as.double(low_temp), "F", "C"), low_temp),
         high_temp = ifelse(year < 2011, conv_unit(as.double(high_temp), "F", "C"), high_temp),
         low_temp = round(as.double(low_temp), digits = 1),
         high_temp = round(as.double(high_temp), digits = 1))

# It was still really cold in 2017!!
# Same plot as above
overall_meta_data %>%
  ggplot(aes(x = as.double(low_temp), y = as.double(high_temp), color = year)) +
  geom_point() +
  geom_text(aes(label = year), angle = -10, hjust = 0.5, vjust = -0.4) +
  xlab("Day's low temperature (C)") +
  ylab("Day's high temperature (C)") +
  labs(color = "Year") +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "darkblue")


# Actual data cleaning time ----

hamilton_cbc <- hamilton_cbc %>%
  remove_empty(which = "cols")

hamilton_cbc <- hamilton_cbc %>% # R got mad at me when I tried to pipe this line after the above line
  rename_all(funs(hamilton_cbc[1, ]))

hamilton_cbc <- hamilton_cbc %>%
  slice(-1) %>%
  clean_names()

hamilton_cbc <- hamilton_cbc %>%
  rename(species = com_name)


hamilton_cbc %>% count(species) #%>% View()

hamilton_cbc %>%
  mutate(id = row_number()) %>%
  filter(!(str_detect(species, "\\["))) #%>% View()
# It turns out there is another secret data table at the bottom, showing the full names of the counters! I could add it to my meta data file, but I think only the primary counter might be of interest, and we only have that data since 2011. We only have secondry counter data since 2003
# So for now I will just remove it from the data set

hamilton_cbc <- hamilton_cbc %>%
  filter(str_detect(species, "\\["))



# I wanted to see, if I used the word function, if the species name would have a space after it. It turned out to have a "\n" after it, so I added that to the real word function in the permanent pipe below this one
hamilton_cbc %>%
  mutate(species = word(species, 1, sep = fixed('['))) %>%
  filter(row_number() == 1) %>%
  pull(species)


# I like @kohske's regex, which looks behind for an open parenthesis ?<=\\(, looks ahead for a closing parenthesis ?=\\), and grabs everything in the middle (lazily) .+?, in other words (?<=\\().+?(?=\\))
# https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
hamilton_cbc <- hamilton_cbc %>%
  mutate(species_latin = str_extract(species, "(?<=\\[).+?(?=\\])"),
         species = word(species, 1, sep = fixed('\n[')))


# From the below three pipes of code, it looks like the count_year variable is actually several variables in one:
# year
# [count_year]
# count date
# number of participants
# number of the participants
# number of species reported
# total hours spent
# This is all metadata and we can take it out of here and put it in our metadata file
# But first, we should parse out the year, so we can join it with the metadata later if we would like
hamilton_cbc %>%
  filter(row_number() == 1) %>%
  pull(count_year)

hamilton_cbc %>%
  filter(row_number() == n()) %>%
  pull(count_year)

hamilton_cbc %>%
  filter(row_number() == n() - 1) %>%
  pull(count_year)

# Yes, it is metadata, with only one value per year!
hamilton_cbc %>% count(count_year) #%>% View()

# Take it out of the hamilton_cbc dataset
count_participant_metadata <- hamilton_cbc %>%
  distinct(participant_info = count_year) %>%
  mutate(year = word(participant_info)) %>%
  mutate(number_of_participants = str_extract(participant_info, "(?<=Participants:\\s).+?(?=\\s#)")) %>% # Getting everything between the "Participants: " and " #"
  mutate(species_reported = str_extract(participant_info, "(?<=Reported:\\s).+?(?=\\nTotal)")) %>%
  mutate(total_hours = str_extract(participant_info, "(?<=Hrs\\.:\\s).*$")) # Different regex because it is at the end of the string: https://forum.sublimetext.com/t/regex-match-everything-after-this-word/20764
  
count_participant_metadata <- count_participant_metadata %>%
  select(year, total_hours)


overall_meta_data <- count_participant_metadata %>% 
  mutate(year = as.integer(year),
         total_hours = as.double(total_hours)) %>%
  full_join(overall_meta_data, by = "year")

hamilton_cbc <- hamilton_cbc %>%
  rename(participant_info = count_year,
         how_many_counted = how_many_cw) %>%
  mutate(year = as.integer(word(participant_info)))


# I believe the number_by_party_hours variable is derived from total number seen/that year's party hours, which I may end up deriving myself, butfor now I will leave that variable in the data set (no longer accurate)
# Also, remove flags variable as per documentation at top of this script
hamilton_cbc <- hamilton_cbc %>%
  select(year, species, species_latin, how_many_counted)

# How many count week only birds were seen? Or any other non-numeric values in how_many_counted
hamilton_cbc %>% filter(str_detect(how_many_counted, letters)) #%>% View()

# I am making the decision to count "cw" as NA, as they were NA on count day.
# While I could probably put a count of "1" for most of them, I can't say for certain that there was only "1" American Widgeon seen.
# I almost put 0, but perhaps they were still in the area but not seen on that day (so it is unknown)

hamilton_cbc <- hamilton_cbc %>%
  mutate(how_many_counted = ifelse(how_many_counted == "cw", NA, how_many_counted),
         how_many_counted = as.integer(how_many_counted))
  


hamilton_cbc <- hamilton_cbc %>%
  group_by(year) %>%
  mutate(count_of_species_seen_in_this_year = sum(!is.na(how_many_counted))) %>% 
  ungroup()


hamilton_cbc <- hamilton_cbc %>%
  group_by(species) %>%
  mutate(how_many_birds_of_this_species_were_counted_over_all_years = sum(how_many_counted, na.rm = TRUE),
         species_level = case_when(how_many_birds_of_this_species_were_counted_over_all_years > 100000 ~ "Very high",
                                   how_many_birds_of_this_species_were_counted_over_all_years > 10000 ~ "High",
                                   how_many_birds_of_this_species_were_counted_over_all_years > 1000 ~ "Medium",
                                   how_many_birds_of_this_species_were_counted_over_all_years > 100 ~ "Low",
                                   TRUE ~ "Very low")) %>%
  ungroup() %>%
  arrange(-how_many_birds_of_this_species_were_counted_over_all_years, species) %>%
  mutate(species_rank_by_count = rleidv(species)) # https://stackoverflow.com/questions/33507868/is-there-a-dplyr-equivalent-to-data-tablerleid I just had to sort it the way I wanted it to be indexed. If I hadn't sorted it, rleidv would have given me the numbering in whatever order the dataset was currently in.
  # mutate(ranking2 = group_indices(., -how_many_birds_of_this_species_were_counted_over_all_years, species)) # https://stackoverflow.com/questions/39650511/r-group-by-variable-and-then-assign-a-unique-id/39756310 I needed to do the group indexing by two variables or it would create the index alphabetically for species only. But if I created the group indexing with just species_count_over_all_years, then if multiple species had the same value for species_count_over_all_years, they would be indexed the same value. This now gives the same result as the arrange/rleidv combo above.

# Do we want to include species with "sp." in them?
# I think I will exclude them because generally their numbers are pretty low
# Ebird excludes them too
# We don't have sp
# hamilton_cbc %>% filter(str_detect(species, "sp\\.")) %>% distinct(species, how_many_birds_of_this_species_were_counted_over_all_years)

hamilton_cbc <- hamilton_cbc %>%
  filter(!(str_detect(species, "sp\\.")))

hamilton_cbc <- hamilton_cbc %>%
  left_join(overall_meta_data, by = "year")



# Get the total species and bird count per year, and divide it by number of hours, to get the species and bird count per hour
hamilton_cbc_extra <- hamilton_cbc %>%
  group_by(year) %>%
  mutate(count_of_birds_seen_in_this_year = sum(how_many_counted, na.rm = TRUE),
         count_of_birds_seen_in_this_year_per_hour = count_of_birds_seen_in_this_year / total_hours) %>%
  ungroup() %>%
  mutate(species_count_per_hour_ungrouped = how_many_counted / total_hours,
         species_count_per_hour_ungrouped = round(species_count_per_hour_ungrouped, 6))


# START HERE - Fix this code so you can rerun the last graph
hamilton_cbc_excl_starlings <- hamilton_cbc_extra %>%
  filter(species != "European Starling") %>%
  group_by(year) %>%
  mutate(count_of_birds_seen_in_this_year_excl_starlings = sum(how_many_counted, na.rm = TRUE),
         count_of_birds_seen_in_this_year_excl_starlings_per_hour = count_of_birds_seen_in_this_year_excl_starlings / total_hours) %>% 
  ungroup() %>%
  select(year, species, count_of_birds_seen_in_this_year_excl_starlings, count_of_birds_seen_in_this_year_excl_starlings_per_hour)


hamilton_cbc_extra <- hamilton_cbc_extra %>%
  left_join(hamilton_cbc_excl_starlings, by = c("year", "species"))

hamilton_cbc_extra %>% write_rds("01-output.rds")
