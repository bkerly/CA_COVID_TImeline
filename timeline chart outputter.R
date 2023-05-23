library(tidyverse)
library(lubridate)

hospital_data <- read_csv("statewide-covid-19-hospital-county-data.csv")

hospital_state <- hospital_data %>%
  mutate(date = todays_date) %>%
  group_by(date) %>%
  summarize(confirmed = sum(hospitalized_covid_confirmed_patients),
            suspected = sum(hospitalized_suspected_covid_patients),
            total = confirmed+suspected)

hospital_state %>%
  select(-total) %>%
  pivot_longer(-date) %>%
  ggplot(aes(x=date,y=value,fill=name)) +
  geom_area(position = position_stack(reverse = TRUE),
            stat="identity") +
  scale_fill_manual(values = c("darkred","red"))+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("COVID Hospitalizations")+
  xlab("")

vaccine_data <- read_csv("covid-19-vaccines-administered-by-demographics.csv")

vaccine_state <- vaccine_data %>%
  filter(demographic_category %in% "Age Group") %>%
  mutate(date = administered_date) %>%
  group_by(date) %>%
  summarize(bv_boosted = sum(cumulative_bivalent_booster_recip_count),
            boosted = sum(cumulative_booster_recip_count) - bv_boosted,
            fully_vax = sum(cumulative_fully_vaccinated) - (boosted+bv_boosted),
            one_dose = sum(cumulative_at_least_one_dose) - (bv_boosted+boosted+fully_vax)
  )

vaccine_state %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, 
                        levels = c(
                          "one_dose",
                          "fully_vax",
                          "boosted",
                          "bv_boosted")
                        )
  ) %>%
  ggplot(aes(x=date,y=value,fill=name)) +
  geom_area(position = position_stack(reverse = TRUE),
            stat="identity") +
  scale_fill_manual(values = c("lightblue","blue","darkblue","purple"))+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("COVID Vaccinations")+
  xlab("")

function(start_date,end_date){
  hosp_graph <- hospital_state %>%
    select(-total) %>%
    pivot_longer(-date) %>%
    ggplot(aes(x=date,y=value,fill=name)) +
    geom_area(position = position_stack(reverse = TRUE),
              stat="identity") +
    scale_fill_manual(values = c("darkred","red"))+
    theme_minimal()+
    theme(legend.position = "none")+
    ylab("COVID Hospitalizations")+
    xlab("")+
    xlim(start_date,end_date)
  
  vax_graph <- vaccine_state %>%
    pivot_longer(-date) %>%
    mutate(name = factor(name, 
                         levels = c(
                           "one_dose",
                           "fully_vax",
                           "boosted",
                           "bv_boosted")
    )
    ) %>%
    ggplot(aes(x=date,y=value,fill=name)) +
    geom_area(position = position_stack(reverse = TRUE),
              stat="identity") +
    scale_fill_manual(values = c("lightblue","blue","darkblue","purple"))+
    theme_minimal()+
    theme(legend.position = "none")+
    ylab("COVID Vaccinations")+
    xlab("")+
    xlim(start_date,end_date)
  
  variant_graph <- 
  
}