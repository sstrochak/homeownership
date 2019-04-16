
# Homeownership data

## Set up

options(scipen = 999)

library(tidyverse)
library(readxl)
library(urbnthemes)
library(tidycensus)

set_urbn_defaults("print")

if (!dir.exists("data")) {
  dir.create("data")
}


## Download and clean CPS data

### Download

if (!file.exists("data/histtab14.xlsx")) {
  
  download.file(url = "https://www.census.gov/housing/hvs/data/histtab14.xlsx",
                destfile = "data/histtab14.xlsx",
                mode="wb")
  
}

if (!file.exists("data/B5.xlsx")) {
  
  download.file(url = "https://www.census.gov/housing/hvs/data/B5.xlsx",
                destfile = "data/B5.xlsx",
                mode="wb")
  
}

### Read and clean
#### Homeownership rate

rate <- read_excel("data/histtab14.xlsx",
                   skip = 3)

# Get rid of rows of footnotes
rate <- rate %>% 
  head(-6)


rate <- rate %>% 
  mutate(year = as.integer(`Year and Area`)) %>% 
  filter(!is.na(`Year and Area`)) %>% 
  fill(year, .direction = "down") %>% 
  filter(str_detect(`Year and Area`, "[:digit:]") == FALSE) 

# Get only second set
rev <- rate %>% 
  group_by(year) %>% 
  filter(n() > 5) %>% 
  mutate(num = row_number()) %>% 
  filter(num %in% 6:10) %>% 
  select(-num)

rate <- rate %>% 
  filter(!year %in% unique(rev$year)) %>% 
  bind_rows(rev)

stopifnot(
  rate %>% group_by(year) %>% filter(n() > 5) %>% nrow() == 0
)

rm(rev)

names(rate) <- c("region", "Q1", "Q2", "Q3", "Q4", "year")

rate <- rate %>% 
  mutate(region = str_remove_all(region, "[:punct:]")) %>% 
  gather(key = "quarter", value = "horate", -region, -year) %>% 
  arrange(region, year, quarter) %>% 
  mutate(period = paste0(year, quarter),
         date = lubridate::yq(period),
         horate = horate / 100)


margin <- read_excel("data/B5.xlsx", skip = 7,
                     col_names = c("year", "Q1", "Q2", "Q3", "Q4")) %>% 
  head(-5)


margin <- margin %>% 
  mutate(year = str_remove_all(year, "[:punct:]")) %>% 
  gather(key = "quarter", value = "moe", -year) %>% 
  mutate(period = paste0(year, quarter),
         date = lubridate::yq(period),
         year = as.integer(year),
         moe = moe / 100)

cps <- rate %>% 
  filter(region == "United States") %>% 
  left_join(margin, by = c("year", "quarter", "period", "date")) %>% 
  filter(year >= 2000) %>% 
  mutate(horate_low = horate - moe,
         horate_high = horate + moe,
         horate_yoy = horate / lag(horate, 4) - 1,
         horate_mom = horate - lag(horate, 1),
         survey = "cps")

rm(margin, rate)

acs <- map_df(2012:2017, ~ mutate(get_acs(geography = "US",
                                          year = .x,
                                          survey = "acs1",
                                          variable = "B25003_002",
                                          summary_var = "B25003_001"),
                                  year = .x))

# Hard code estimates from other years from fact finder
acs <- acs %>% 
  bind_rows(tribble(
    ~year, ~estimate, ~moe, ~summary_est, ~summary_moe,
    2005, 74318982, 293104, 111090617, 143575,
    2006, 75086485, 218471, 111617402, 145530,
    2007, 75515104, 227236, 112377977, 144356,
    2008, 75373053,	224087, 113101329, 146859,
    2009, 74843004, 217682, 113616229, 161397,
    2010, 74873372,	216091, 114567419, 163249,
    2011, 74264435,	230440, 114991725, 179541
  ))


acs <- acs %>% 
  mutate(date = as.Date(paste0(year, "-12-01")),
         horate = estimate / summary_est,
         horate_low = (estimate - moe) / summary_est,
         horate_high = (estimate + moe) / summary_est) %>% 
  fill(GEOID, NAME, variable) %>% 
  arrange(year) %>% 
  mutate(survey = "acs")


jointdata <- acs %>% 
  select(year, date, survey,
         horate, horate_low, horate_high) %>% 
  bind_rows(select(cps, year, date, survey, period,
                   horate, horate_low, horate_high))

