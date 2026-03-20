## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7645]
##' [FILE: Data Wrangling IV: Advanced tidyverse & Data Retrieval]
##' [INIT: Jan 12th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##' [UPTD: Jan 12th 2025]
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)


## ---------------------------
##' [Read in Tidyverse Tricks Data]
## ---------------------------

data_18_pub <- read_csv("data/ipeds-finance/f1819_f1a_rv.csv")
data_18_np <- read_csv("data/ipeds-finance/f1819_f2_rv.csv")
data_18_fp <- read_csv("data/ipeds-finance/f1819_f3_rv.csv")

data_18 <- bind_rows(data_18_pub, data_18_np, data_18_fp)

data_18 |>
  count(UNITID) |>
  filter(n > 1)


## ---------------------------
##' [coalesce()-ing Split Data]
## ---------------------------

data_18 <- data_18 |>
  select(UNITID,
         F1C011, F1C021, F1C061,
         F2E011, F2E021, F2E051,
         F3E011, F3E02A1, F3E03B1)

print(data_18[100:105,])
print(data_18[3000:3005,])


## Split back up into separate files
pub <- data_18 |> filter(!is.na(F1C011)) |>
  ## Rename the variable
  rename(inst_spend = F1C011) |>
  ## Drop the other variables
  select(UNITID, inst_spend)
np <- data_18 |> filter(!is.na(F2E011)) |>
  rename(inst_spend = F2E011) |>
  select(UNITID, inst_spend)
fp <- data_18 |> filter(!is.na(F3E011)) |>
  rename(inst_spend = F3E011) |>
  select(UNITID, inst_spend)


## Re-bind the colleges back up
hard_way <- bind_rows(pub, np, fp)


easy_way <- data_18 |>
  mutate(inst_spend = coalesce(F1C011, F2E011, F3E011)) |>
  select(UNITID, inst_spend)

print(easy_way[100:105,])
print(easy_way[3000:3005,])

all.equal(hard_way, easy_way)

data_18_clean <- data_18 |>
  mutate(inst_spend = coalesce(F1C011, F2E011, F3E011),
         rsch_spend = coalesce(F1C021, F2E021, F3E02A1),
         serv_spend = coalesce(F1C061, F2E051, F3E03B1)) |>
  select(UNITID, inst_spend, rsch_spend, serv_spend)

print(data_18_clean[100:105,])
print(data_18_clean[3000:3005,])

## ---------------------------
##' [Finding if_any() Issues]
## ---------------------------

data_0_inst <- data_18_clean |> filter(inst_spend == 0)
data_0_rsch <- data_18_clean |> filter(rsch_spend == 0)
data_0_serv <- data_18_clean |> filter(serv_spend == 0)
data_0 <- bind_rows(data_0_inst, data_0_rsch, data_0_serv)

## Plus we end up with duplicates
data_0 |>
  count(UNITID) |>
  filter(n > 1)


data_0 <- data_18_clean |>
  filter(if_any(everything(), ~ . == 0)) ## h/t https://stackoverflow.com/questions/69585261/dplyr-if-any-and-numeric-filtering

print(data_0)

## ---------------------------
##' [Working across() Columns]
## ---------------------------

data_0 |>
  select(-UNITID) |>
  count(across(everything(), ~ . == 0))

## ---------------------------
##' [From ifelse() to case_when()]
## ---------------------------

data_18_clean |>
  mutate(highest_cat = case_when(inst_spend > rsch_spend & rsch_spend > serv_spend ~ "inst_rsch_serv",
                                 inst_spend > serv_spend & serv_spend > rsch_spend ~ "inst_serv_rsch",
                                 rsch_spend > inst_spend & inst_spend > serv_spend ~ "rsch_inst_serv",
                                 rsch_spend > serv_spend & serv_spend > inst_spend ~ "rsch_serv_inst",
                                 serv_spend > inst_spend & inst_spend > rsch_spend ~ "serv_inst_rsch",
                                 serv_spend > rsch_spend & rsch_spend > inst_spend ~ "serv_rsch_inst",
                                 TRUE ~ NA)) |>
  count(highest_cat)

data_18_clean |>
  mutate(highest_cat = case_when(inst_spend >= rsch_spend & rsch_spend >= serv_spend ~ "inst_rsch_serv",
                                 inst_spend >= serv_spend & serv_spend >= rsch_spend ~ "inst_serv_rsch",
                                 rsch_spend >= inst_spend & inst_spend >= serv_spend ~ "rsch_inst_serv",
                                 rsch_spend >= serv_spend & serv_spend >= inst_spend ~ "rsch_serv_inst",
                                 serv_spend >= inst_spend & inst_spend >= rsch_spend ~ "serv_inst_rsch",
                                 serv_spend >= rsch_spend & rsch_spend >= inst_spend ~ "serv_rsch_inst",
                                 TRUE ~ NA)) |>
  count(highest_cat)

selected_files <- c("HD2021", "EFFY2022", "SFA2122")

## OR

selected_files <- c(
  "HD2021",
  "EFFY2022",
  "SFA2122"
)

selected_files <- c(
  "HD2021",
  # "IC2022",
  # "IC2022_AY",
  "EFFY2022",
  # "EFFY2022_DIST",
  "SFA2122"
)

library(haven)
library(labelled)

data_info <- read_dta("data/hd2021.dta")
data_enroll <- read_dta("data/effy2022.dta")
data_aid <- read_dta("data/sfa2122.dta")

data <- left_join(data_info, data_enroll, by = "unitid") |>
  left_join(data_aid, by = "unitid")

nrow(data)

data |> count(effylev)


data <- data |> filter(effylev == 2)


data |>
  group_by(obereg) |>
  summarize(median_perc_out_of_state = median(scfa13p, na.rm = TRUE))



data |>
  group_by(as_factor(obereg)) |>
  summarize(median_perc_out_of_state = median(scfa13p, na.rm = TRUE))

ggplot(data |> filter(efytotlt < 50000),
       aes(x = efytotlt,
           y = scfa13p)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  facet_wrap(~obereg)

ggplot(data |> filter(efytotlt < 50000),
       aes(x = efytotlt,
           y = scfa13p)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  labs(x = var_label(data$efytotlt),
       y = var_label(data$scfa13p)) +
  facet_wrap(~as_factor(obereg))

ggplot(data |> filter(efytotlt < 50000),
       aes(x = efytotlt,
           y = scfa13p)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8)) +
  labs(x = var_label(data$efytotlt),
       y = str_wrap(var_label(data$scfa13p), 40)) +
  facet_wrap(~as_factor(obereg),
             labeller = label_wrap_gen(multi_line = TRUE))

# install.packages("tidycensus")
# library(tidycensus)
library(tidycensus)

# census_api_key("<key>", install = T)

data <- get_acs(geography = "tract",
                state = "MN",
                year = 2022,
                survey = "acs5",
                variables = c("DP02_0065PE", "DP03_0119PE"), # Pop >=25 with Bachelors, Pop below poverty line
                output = "wide",
                geometry = FALSE)

ggplot(data) +
  geom_point(aes(x = DP03_0119PE,
                 y = DP02_0065PE))

# install.packages("educationdata")
# library(educationdata)
library(educationdata)

# data_info <- get_education_data(level = "college-university",
#                            source = "ipeds",
#                            topic = "directory",
#                            filters = list(year = 2021),
#                            add_labels = TRUE)
# 
# data_aid <- get_education_data(level = "college-university",
#                            source = "ipeds",
#                            topic = "sfa-by-tuition-type",
#                            filters = list(year = 2021),
#                            add_labels = TRUE)

load("data/ui-ed-data.Rdata")

nrow(data_info)
nrow(data_aid)

data_aid <- data_aid |>
  filter(tuition_type == "Out of state")

nrow(data_aid)

# install.packages("EdSurvey")
# library(EdSurvey)
library(EdSurvey)

# downloadHSLS(".")

# hsls <- readHSLS("HSLS/2009")

# data_hsls <- getData(hsls,
#                      varnames = c("x4evratndclg", "x1paredexpct"))
load("data/ed-survey.Rds")

data_hsls_plot <- data_hsls |>
  group_by(x4evratndclg) |>
  count(x1stuedexpct) |>
  mutate(sum = sum(n),
         perc = n/sum*100) |>
  select(x4evratndclg, x1stuedexpct, perc)

ggplot(data_hsls_plot) +
  geom_col(aes(y = perc, x = x1stuedexpct, fill = x4evratndclg),
           position = "dodge") +
  scale_fill_manual(values = c("pink2", "navy")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  labs(x = "Student Educational Expectations in Wave 1",
       y = "% of Students Respondants",
       fill = str_wrap("Did the Student Ever Attend College by Wave 4", 30))

download.file(url = "https://www.kaggle.com/api/v1/datasets/download/ben1989/target-store-dataset",
              destfile = "data/target-data.zip")

unzip("data/target-data.zip", exdir = "data/")

data_target <- read_csv("data/targets.csv")

data_target |>
  count(SubTypeDescription)
