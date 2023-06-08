library(tidyverse)
library(lubridate)
library(readxl)

county_populations <- read_csv("co-est2022-alldata.csv") %>%# source https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/
  filter(STNAME == "California") %>% 
  mutate(county = sub(" County", "", CTYNAME)) %>%
  transmute(county = county,
            population = POPESTIMATE2020)

hospitalization <- read_csv("statewide-covid-19-hospital-county-data.csv") %>%
  filter(county %in% county_populations$county) %>%
  mutate(date = todays_date) %>%
  group_by(date,county) %>%
  summarize(confirmed_hosp = sum(hospitalized_covid_confirmed_patients),
            suspected_hosp  = sum(hospitalized_suspected_covid_patients),
            total_hosp  = confirmed_hosp+suspected_hosp)

vaccines <- read_csv("covid19vaccinesbycounty.csv") %>%
  filter(county %in% county_populations$county) %>%
  mutate(date = administered_date) %>%
  group_by(date,county) %>%
  summarize(total_doses = sum(cumulative_total_doses),
            cumulative_fully_vaccinated =sum(cumulative_fully_vaccinated))

county_populations$county %>% unique()
vaccines$county %>% unique()


# Merge it ----------------------------------------------------------------

county_data <- hospitalization %>%
  left_join(county_populations) %>%
  left_join(vaccines) %>%
  mutate(pct_fully_vaccinated = 100*cumulative_fully_vaccinated/population)

ggplot(county_data %>% filter(date>ymd("2021-5-1")), aes(x=date)) +
  geom_line(aes(y=total_doses/population)) +
  facet_wrap(~county)


# sum up For Whole State Data ---------------------------------------------

state_data <- county_data %>%
  group_by(date) %>%
  reframe(population = county_populations$population[county_populations$county=="California"],
            confirmed_hosp = sum(confirmed_hosp,na.rm=TRUE),
            suspected_hosp = sum(suspected_hosp,na.rm=TRUE),
            total_hosp = sum(total_hosp,na.rm=TRUE),
            total_doses = sum(total_doses,na.rm=TRUE),
            cumulative_fully_vaccinated = sum(cumulative_fully_vaccinated,na.rm = TRUE)) %>%
  mutate(pct_fully_vaccinated = 100*cumulative_fully_vaccinated/population) %>%
  mutate(county = "All California")

ggplot(state_data %>% filter(date>ymd("2021-5-1")), aes(x=date)) +
  geom_line(aes(y=total_hosp/population)) 


# add in state data to county data and export -----------------------------
metric_data <- county_data %>%
  bind_rows(state_data)

write_csv(metric_data, "Metric Data.csv")


# Separate thing for variants ---------------------------------------------

variant_data <- read_csv("covid-19-variant-data.csv")

prevalent_variant <- variant_data %>%
  filter(area=="California") %>%
  filter(date >= ymd("2021-01-12")) %>%
  group_by(date) %>%
  filter(variant_name != "Total") %>%
  slice_max(n=1,order_by = percentage) %>%
  ungroup() %>%
  group_by(variant_name) %>%
  summarize(Start = min(date),
            End = max(date)) %>%
  bind_rows(
    data.frame(
      variant_name = "Ancestral",
      Start = ymd("2019-9-1"),
      End = ymd("2021-01-11")
    )
  ) %>%
  mutate(midpoint = as.Date((as.numeric(End) + 
                               as.numeric(Start)) / 2, origin = '1970-01-01') )

write_csv(prevalent_variant,"prevalent variant.csv")
