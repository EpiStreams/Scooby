
# Loading packages --------------------------------------------------------
library(tidyverse)




# Raw data ----------------------------------------------------------------
scooby_raw <- read_rds("C:/Users/colvic01/Downloads/scoobydoo.rds")
scooby_t <- tibble(scooby_raw)




# Primary data set creation -----------------------------------------------

# First step: Restrict the data set to Scooby franchises and not Scooby cameos
# in other franchises.

scooby_nocameo <- scooby_t %>%
 #lower casing the series names to make them easier to work with
  mutate(
    series_name = tolower(series_name)) %>%
  #filtering the series names to retain only those with the words "scooby",
  #"doo", "warner", or "hanna" in them. These titles were chosen by visual inspection.
  #Note: I excluded the arabian nights "TV Special" b/c it lacks a traditional scooby plot.
  filter(grepl("scooby|doo|warner|hanna", series_name)) %>%
  #There are more series that don't fit the traditional scooby plot but that had
  #scooby in the name, here I will remove those.
  filter(!grepl("night|shaggy|hollywood|project", series_name))
  



# Now, I will separate the television shows from the movies/specials
# Within the categories, separating them into non-segmented and segmented shows
# and TV versus theatrical movies
scooby_tv <- filter(scooby_nocameo, grepl("TV", format))
scooby_movie <- filter(scooby_nocameo, grepl("Movie", format))
scooby_tv_full <- filter(scooby_tv, !grepl("segmented", format))
scooby_tv_segment <- filter(scooby_tv, grepl("segmented", format))
scooby_movie_tv <- filter(scooby_movie, !grepl("Theatrical", format))
scooby_movie_thr <- filter(scooby_movie, grepl("Theatrical", format))
