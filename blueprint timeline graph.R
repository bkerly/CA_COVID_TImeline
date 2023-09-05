library(tidyverse)
library(lubridate)
library(readr)

blueprint <- read_csv("blueprint timeline.csv")%>%
  rename("county" = `...1`,
         "size" = Size) %>%
  select(-(`...47`)) %>%
  pivot_longer(cols = (3:46),
               names_to = "date",
               values_to = "level"
  ) %>%
  mutate(date = mdy(date)) %>%
  mutate(day = wday(date,label = TRUE)) %>%
  mutate(level = factor(level,labels = c("1-Purple",
                                            "2-Red",
                                            "3-Orange",
                                            "4-Yellow")))

blueprint %>%
  ggplot(aes(x=date, y=county,color = level, fill=level)) +
  geom_point(size = 5)

blueprint %>%
  group_by(date,level) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  ggplot(aes(x=date, y=count, color = level, fill=level)) +
  geom_area(position = position_fill(reverse=TRUE))+
  scale_fill_manual(values = c("purple","red","orange","yellow"))+
  scale_color_manual(values = c("purple","red","orange","yellow"))+
  ylab("")+
  xlab("")+
  labs(title = "Blueprint Levels by County")+
  theme_minimal()+
  
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
    
  )
