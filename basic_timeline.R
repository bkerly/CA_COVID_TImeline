library(tidyverse)
library(lubridate)
library(timevis)

timeline_data <- readxl::read_excel("COVID Timeline.xlsx") 

tv_data <- timeline_data %>%
  transmute(id= row_number(),
         content = event,
         start = as.Date(date) ,
         end = NA_Date_,
         group = source) %>%
  filter(group == "NYT") %>%
  data.frame()

timevis(tv_data)

