## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7645]
##' [FILE: Data wrangling V: IPEDS Application]
##' [INIT: 31 January 2020]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##' [EDIT: Jue Wu]
##' [UPDT: 04 February 2026]
##
## -----------------------------------------------------------------------------


## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)

data_info <- read_csv("data/hd2022.csv") |>
  rename_all(tolower) # convert all column names to lowercase

data_enroll <- read_csv("data/effy2022.csv") |>
  rename_all(tolower)

data_info

data_enroll

data_enroll <- data_enroll |>
  select(unitid, effylev, efytotlt, efynralt)

data_enroll


data_enroll <- data_enroll |>
  filter(effylev %in% c(2,4))

data_enroll



data_enroll <- data_enroll |>
  mutate(perc_intl = efynralt/efytotlt*100) |>
  select(-efytotlt, -efynralt) # - in select means drop this variable

data_enroll

data_enroll <- data_enroll |>
  pivot_wider(names_from = effylev,
              values_from = perc_intl,
              names_prefix = "perc_intl_")

data_enroll

data_enroll <- data_enroll |>
  mutate(perc_intl_diff = perc_intl_2 - perc_intl_4)

data_enroll

data_enroll |>
  drop_na() |>
  summarize(mean = mean(perc_intl_diff),
            min = min(perc_intl_diff),
            max = max(perc_intl_diff))


data_info <- data_info |>
  select(unitid, control)

data_joined <- left_join(data_enroll, data_info, by = "unitid")

data_joined

data_joined |>
  group_by(control) |>
  drop_na() |>
  summarize(mean = mean(perc_intl_diff))

library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

data_info_state <- read_csv("data/hd2022.csv") |>
  rename_all(tolower) |>
  select(unitid, stabbr)

data_joined_state <- left_join(data_enroll, data_info_state, by = "unitid")

data_joined_state

data_joined_state <- data_joined_state |>
  group_by(stabbr) |>
  drop_na() |>
  summarize(mean_perc_intl_diff = mean(perc_intl_diff))

data_joined_state

states_sf <- states(cb = TRUE, year = 2022) |>
  filter(STUSPS %in% state.abb | STUSPS == "DC") |>
  left_join(data_joined_state, by = c("STUSPS" = "stabbr"))

ggplot() +
  geom_sf(data = shift_geometry(states_sf),
          aes(fill = mean_perc_intl_diff),
          size = 0.1) +
  geom_sf_text(data = shift_geometry(st_centroid(states_sf)),
               aes(label = STUSPS))


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
