#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)


# Hospitalizations --------------------------------------------------------



hospital_data <- read_csv("statewide-covid-19-hospital-county-data.csv")

hospital_state <- hospital_data %>%
  mutate(date = todays_date) %>%
  group_by(date) %>%
  summarize(confirmed = sum(hospitalized_covid_confirmed_patients),
            suspected = sum(hospitalized_suspected_covid_patients),
            total = confirmed+suspected)

hospital_state %>%
  select(-total,-suspected) %>% # suspected removed per preference of James Watt
  pivot_longer(-date) %>%
  ggplot(aes(x=date,y=value,fill=name)) +
  geom_area(position = position_stack(reverse = TRUE),
            stat="identity") +
  scale_fill_manual(values = c("darkred"#,"red"
                               ))+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("COVID Hospitalizations")+
  xlab("")


# Vaccines ----------------------------------------------------------------



vaccine_data <- read_csv("covid-19-vaccines-administered-by-demographics.csv")

vaccine_categories <- vaccine_data %>%
  filter(demographic_category %in% "Age Group") %>%
  mutate(date = administered_date) %>%
  group_by(date) %>%
  summarize(bv_boosted = sum(cumulative_bivalent_booster_recip_count),
            boosted = sum(cumulative_booster_recip_count) - bv_boosted,
            fully_vax = sum(cumulative_fully_vaccinated) - (boosted+bv_boosted),
            one_dose = sum(cumulative_at_least_one_dose) - (bv_boosted+boosted+fully_vax)
  )

vaccine_doses <- vaccine_data %>%
  filter(demographic_category %in% "Age Group") %>%
  mutate(date = administered_date) %>%
  group_by(date) %>%
  summarize(total_doses = sum(cumulative_total_doses)
  )

vaccine_fully <- vaccine_data %>%
  filter(demographic_category %in% "Age Group") %>%
  mutate(date = administered_date) %>%
  group_by(date) %>%
  summarize(pct_vaccinated = 100*sum(cumulative_fully_vaccinated) / (39029342) # https://www.census.gov/quickfacts/CA, population July 1 2022
  )


# vaccine_state %>%
#   pivot_longer(-date) %>%
#   mutate(name = factor(name, 
#                        levels = c(
#                          "one_dose",
#                          "fully_vax",
#                          "boosted",
#                          "bv_boosted")
#   )
#   ) %>%
#   ggplot(aes(x=date,y=value,fill=name)) +
#   geom_area(position = position_stack(reverse = TRUE),
#             stat="identity") +
#   scale_fill_manual(values = c("lightblue","blue","darkblue","purple"))+
#   theme_minimal()+
#   theme(legend.position = "none")+
#   ylab("COVID Vaccinations")+
#   xlab("")


vaccine_doses %>%
  ggplot(aes(x=date,y=total_doses)) +
  geom_area(position = position_stack(reverse = TRUE),
            stat="identity", fill = "lightgreen") +
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("COVID Vaccine Doses")+
  xlab("")

vaccine_fully %>%
  ggplot(aes(x=date,y=pct_vaccinated)) +
  geom_area(position = position_stack(reverse = TRUE),
            stat="identity", fill = "darkgreen") +
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("Percent Population Fully Vaccinated")+
  xlab("")

# Variants ----------------------------------------------------------------

library(vistime)

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

prevalent_variant %>%
  ggplot() +
  geom_rect(aes(
    xmin = Start,xmax=End,ymin = 1, ymax = 0, fill = variant_name))+
  geom_label(aes(
    x = midpoint,
    y = 0.5,
    label = variant_name
  ))+
  theme(legend.position = "none")+
  ylab("")+
  xlab("")


# Events ------------------------------------------------------------------

COVID_Timeline <- readxl::read_xlsx("COVID Timeline.xlsx") %>%
  mutate(date = as.Date(date))

COVID_Timeline %>%
  slice_max(importance,n=10) %>%
  ggplot(
    aes(
      x=date,
      y=1,
      label = event
    )) +
  geom_hline(yintercept = 1)+
  #theme_void() +
  geom_point(
    color = "red",
    size = 3
  ) +
  ggrepel::  geom_text_repel(
    force_pull   = 0, # do not pull toward data points
    nudge_y      = 0.05,
    direction    = "x",
    angle        = 90,
    vjust        = 1,
    segment.size = 0.2,
    max.iter = 1e4, max.time = 1
  ) +
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
    
  )


# Make it a function ------------------------------------------------------



master_graph <-function(start_date = ymd("2020-3-1"),end_date = ymd("2022-3-1")){
  hosp_graph <- hospital_state %>%
    filter( date >= start_date,
            date <= end_date
    ) %>%
    select(-total) %>%
    pivot_longer(-date) %>%
    filter(name == "confirmed") %>%
    ggplot(aes(x=date,y=value,fill=name)) +
    geom_area(position = position_stack(reverse = TRUE),
              stat="identity") +
    scale_fill_manual(values = c("darkred","red"))+
    theme_minimal()+
    theme(      axis.line.y  = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y  = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                legend.position = "none")+
    ylab("")+
    xlab("")+
    labs(title = "Daily Hospitalizatons")+
    xlim(start_date,end_date)
  
  vax_dose_graph <- vaccine_doses %>%
    ggplot(aes(x=date,y=total_doses)) +
    geom_area(position = position_stack(reverse = TRUE),
              stat="identity", fill = "lightgreen") +
    theme_minimal()+
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      
      legend.position = "none"
      
    )+
    ylab("")+
    xlab("")+
    labs(title = "Cumulative Vaccine Doses")+
    xlim(start_date,end_date)
  
  vax_fully_graph <- vaccine_fully %>%
    ggplot(aes(x=date,y=pct_vaccinated)) +
    geom_area(position = position_stack(reverse = TRUE),
              stat="identity", fill = "darkgreen") +
    theme_minimal()+
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      
      legend.position = "none"
      
    )+
    ylab("")+
    xlab("")+
    labs(title = "Percent Fully Vaccinated")+
    xlim(start_date,end_date)
  
  variant_graph <- prevalent_variant %>%
    filter(Start <= end_date,
           End >= start_date) %>%
    mutate(Start = case_when(
      Start > start_date ~ Start,
      TRUE ~ start_date
    )) %>%
    mutate(End = case_when(
      End < end_date ~ End,
      TRUE ~ end_date
    )) %>%
    mutate(midpoint = as.Date((as.numeric(End) + 
                                 as.numeric(Start)) / 2, origin = '1970-01-01') ) %>%
    ggplot() +
    geom_rect(aes(
      xmin = Start,xmax=End,ymin = 1, ymax = 0, fill = variant_name))+
    geom_label(aes(
      x = midpoint,
      y = 0.5,
      label = variant_name
    ))+
    ylab("")+
    xlab("")+
    labs(title = "Dominant Variant")+
    theme_minimal()+
    
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      
    )+
    xlim(start_date,end_date)
  
  
  timeline_graph <- COVID_Timeline %>%
    filter( date >= start_date,
            date <= end_date
    ) %>%
    slice_max(importance,n=15) %>%
    ggplot(
      aes(
        x=date,
        y=1,
        label = event
      )) +
    geom_hline(yintercept = 1)+
    #theme_void() +
    geom_point(
      color = "red",
      size = 3
    ) +
    ggrepel::  geom_text_repel(
      force_pull   = 0, # do not pull toward data points
      nudge_y      = 0.05,
      direction    = "x",
      angle        = 90,
      vjust        = 1,
      segment.size = 0.2,
      max.iter = 1e4, max.time = 1
    ) +
    theme_minimal()+
    
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
      
    )+
    xlab("")+
    xlim(start_date,end_date)
  
  #combine
  
  ggpubr::ggarrange(
    timeline_graph,
    hosp_graph,
    vax_dose_graph,
    vax_fully_graph,
    variant_graph,
    nrow =5,
    heights = c(5,2,2,2,1)
  )
  
  
  
}


# UI ----------------------------------------------------------------------



ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(
        "daterange", label = "Date Range",
        min = ymd("2019-9-1"), max = ymd("2023-4-1"),
        value = c(ymd("2020-3-1"),ymd("2022-3-1"))
      ), #/ sliderInput
      
      checkboxGroupInput(
        "graphs", label = "Select Graphs to Display",
        choices = c(
          "Timeline",
          "Hospitalizatons",
          "Vaccine Doses",
          "Percent Fully Vaccinated",
          "Variants",
          "Reopening Plan"
        ),
        selected = c(
          "Timeline",
          "Hospitalizatons",
          "Vaccine Doses",
          "Percent Fully Vaccinated",
          "Variants"
        )
      ), #/ checkboxGroupInput Graphs
      
      sliderInput(inputId = "height", label = "Height", min = 500, max = 2000, value = 1000),
      sliderInput(inputId = "width", label = "Width", min = 500, max = 2000, value = 1250),

        downloadButton('downloadData', 'Download Data'),
        downloadButton('downloadPlot', 'Download Plot'
                       ) #/ Download Buttons
        
      
    ), #/ sidebar panel
    
    mainPanel(
      plotOutput("timeline", height="800px")
    ) #/ main panel
  )

)


# Server ------------------------------------------------------------------



server <- function(input, output, session) {

  # Make the plot
  
  make_plot <- function(){
    master_graph(start_date = input$daterange[1],
                 end_date = input$daterange[2])
  }
  
  # Draw the plot
  
  output$timeline <- renderPlot(
    width = function() input$width,
    height = function() input$height,
    res = 96,
    make_plot()
    )
  
  # Download Plot
  
  output$downloadPlot <- downloadHandler(
    filename = "Timeline plot.png",
    
    content = function(file){
      png(file,
          width = input$width,
          height = input$height)
      
        make_plot() %>% print()
      
      dev.off()
        
    }
  )
    
  # Downloading Data
    data <- COVID_Timeline

    output$downloadData <- downloadHandler(
      filename = "Timeline data.csv",

      content = function(file){
          write.csv(data, file)
      }
    )
      
    
    
  
}

shinyApp(ui = ui, server = server)
