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


# Load Data --------------------------------------------------------

metric_data <- read_csv("Metric Data.csv")

COVID_Timeline <- readxl::read_xlsx("COVID Timeline.xlsx") %>%
  mutate(date = as.Date(date))

variant_data <- read_csv("prevalent variant.csv")

c = metric_data %>% 
  select(county) %>%
  filter(county != "All California") %>%
  unique() %>%
  arrange() %>%
  unlist() %>%
  append("All California",0) %>%
  unname()


# Create Graph Themes ------------------------------------------------------

metric_graph <- function(county_choice = "All California",
                         line_color = "blue",
                         column_name = "total_hosp",
                         graph_title = "Daily Hospitalizations",
                         start_date = ymd("2020-3-1"),
                         end_date = ymd("2022-3-1")
) {
  
  metric_data %>%
    filter(county == county_choice) %>%
    filter( date >= start_date,
            date <= end_date
    ) %>%
    ggplot(aes(x=date,y=.[[column_name]])) +
    geom_area(stat="identity",fill = line_color) +
    theme_minimal()+
    theme(      axis.line.y  = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y  = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_blank(),
                legend.position = "none")+
    ylab("")+
    xlab("")+
    labs(title = graph_title)+
    xlim(start_date,end_date)
  
}

# metric_graph(column_name = "total_doses",
#              line_color = "green",
#              graph_title = "total doses",
#              county_choice = "Yolo")

timeline_graph <- function(start_date = ymd("2020-3-1"),
                           end_date = ymd("2022-3-1")
) {
  
  COVID_Timeline %>%
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
  
}

# timeline_graph(start_date = ymd("2020-3-15"),
#                end_date = ymd("2020-4-1"))

variant_graph <- function(start_date = ymd("2020-3-1"),
                          end_date = ymd("2022-3-1")
){
  
  variant_data %>%
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
  
}

# variant_graph(start_date = ymd("2021-1-1"),
#               end_date = ymd("2023-1-1")
# )


# Create a Single Merged Graph --------------------------------------------
merged_graph <- function(
    start_date = ymd("2020-3-1"),
    end_date = ymd("2022-3-1"),
    metric_graphs = list(
      "Total Hospitalizations",
      "Vaccine Doses"
    ),
    county = "Yolo"
)
{
  
  graphs = list(
    timeline_graph(
      start_date = start_date,
      end_date = end_date
    ),
    variant_graph(start_date = start_date,
                  end_date = end_date)
  ) #/ list 
  
  if("Percent Fully Vaccinated" %in% metric_graphs){
    graphs <- append(graphs,
                     list(metric_graph(column_name = "pct_fully_vaccinated",
                                       line_color = "darkgreen",
                                       graph_title = "Percent Population Fully Vaccinated",
                                       county_choice = county)
                     ),1)
  }
  
  if("Vaccine Doses" %in% metric_graphs){
    graphs <- append(graphs,
                     list(metric_graph(column_name = "total_doses",
                                       line_color = "green",
                                       graph_title = "Total Vaccine Doses",
                                       county_choice = county)
                     ),1)
  }
  

  
  
  if("Total Hospitalizations" %in% metric_graphs){
    graphs <- append(graphs,
                     list( metric_graph(column_name = "total_hosp",
                                        line_color = "darkred",
                                        graph_title = "Daily Confirmed Hospitalizations",
                                        county_choice = county)
                     ),1)
  }
  
  if("Confirmed Hospitalizations" %in% metric_graphs){
    graphs <- append(graphs,
                     list( metric_graph(column_name = "confirmed_hosp",
                                        line_color = "red",
                                        graph_title = "Daily Confirmed and Suspected Hospitalizations",
                                        county_choice = county)
                     ),1)
  }
  
  
  
  
  
  num_graphs = length(graphs)
  
  graph_heights = c(
    5,
    rep.int(2,times = num_graphs-2),
    1
  )
  
  ggpubr::ggarrange(
    plotlist = graphs,
    nrow = num_graphs,
    heights = graph_heights
  )
}


#merged_graph()


# UI ----------------------------------------------------------------------



ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "county", label = "Select County",
        choices = c(county_list)
      ), #/ selectInput
      
      
      sliderInput(
        "daterange", label = "Date Range",
        min = ymd("2019-9-1"), max = ymd("2023-4-1"),
        value = c(ymd("2020-3-1"),ymd("2022-3-1"))
      ), #/ sliderInput
      
      checkboxGroupInput(
        "metrics", label = "Select Metric Graphs to Display",
        choices = c(
          "Total Hospitalizations",
          "Confirmed Hospitalizations",
          "Vaccine Doses",
          "Percent Fully Vaccinated"
        ),
        selected = c(
          "Total Hospitalizations",
          "Percent Fully Vaccinated"
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
    merged_graph(start_date = input$daterange[1],
                 end_date = input$daterange[2],
                 metric_graphs = 
                   input$metrics
                 ,
                 input$county)
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
