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

county_list <- metric_data %>%
  select(county) %>%
  filter(county != "All California") %>%
  unique() %>%
  arrange() %>%
  unlist() %>%
  append("All California", 0) %>%
  unname()


# Create Graph Themes ------------------------------------------------------

metric_graph <- function(county_choice = "All California",
                         line_color = "blue",
                         column_name = "total_hosp",
                         graph_title = "Daily Hospitalizations",
                         start_date = ymd("2020-3-1"),
                         end_date = ymd("2022-3-1")) {
  metric_data %>%
    filter(county == county_choice) %>%
    filter(date >= start_date,
           date <= end_date) %>%
    ggplot(aes(x = date, y = .[[column_name]])) +
    geom_area(stat = "identity", fill = line_color) +
    theme_minimal() +
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    ) +
    ylab("") +
    xlab("") +
    labs(title = graph_title) +
    xlim(start_date, end_date)
  
}

# metric_graph(column_name = "total_doses",
#              line_color = "green",
#              graph_title = "total doses",
#              county_choice = "Yolo")

timeline_graph <- function(start_date = ymd("2020-3-1"),
                           end_date = ymd("2022-3-1"),
                           input_data = COVID_Timeline) {
  input_data %>%
    filter(date >= start_date,
           date <= end_date) %>%
    slice_max(importance, n = 15) %>%
    ggplot(aes(x = date,
               y = 1,
               label = event)) +
    geom_hline(yintercept = 1) +
    #theme_void() +
    geom_point(aes(color = type),
               size = 3) +
    ggrepel::geom_text_repel(
      force_pull   = 0,
      # do not pull toward data points
      nudge_y      = 0.05,
      direction    = "x",
      angle        = 90,
      vjust        = 1,
      segment.size = 0.2,
      max.iter = 1e4,
      max.time = 1
    ) +
    theme_minimal() +
    
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
      
    ) +
    xlab("") +
    xlim(start_date, end_date)
  
}

# timeline_graph(start_date = ymd("2020-3-15"),
#                end_date = ymd("2020-4-1"))

variant_graph <- function(start_date = ymd("2020-3-1"),
                          end_date = ymd("2022-3-1")) {
  variant_data %>%
    filter(Start <= end_date,
           End >= start_date) %>%
    mutate(Start = case_when(Start > start_date ~ Start,
                             TRUE ~ start_date)) %>%
    mutate(End = case_when(End < end_date ~ End,
                           TRUE ~ end_date)) %>%
    mutate(midpoint = as.Date((as.numeric(End) +
                                 as.numeric(Start)) / 2, origin = '1970-01-01')) %>%
    ggplot() +
    geom_rect(aes(
      xmin = Start,
      xmax = End,
      ymin = 1,
      ymax = 0,
      fill = variant_name
    )) +
    geom_label(aes(x = midpoint,
                   y = 0.5,
                   label = variant_name)) +
    ylab("") +
    xlab("") +
    labs(title = "Dominant Variant") +
    theme_minimal() +
    
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      
    ) +
    xlim(start_date, end_date)
  
}

# variant_graph(start_date = ymd("2021-1-1"),
#               end_date = ymd("2023-1-1")
# )

# Blueprint Graph

blueprint_graph <- function(county_choice = "All California",
                            start_date = ymd("2020-3-1"),
                            end_date = ymd("2022-3-1")) {
  
  
    metric_data %>%
    filter(case_when(
      county_choice == 'All California' ~ TRUE,
      TRUE ~ county %in% county_choice
    )) %>%
    filter(date >= start_date,
           date <= end_date) %>%
    group_by(date, blueprint_level) %>%
    summarize(count = n()) %>%
    ungroup()  %>%
    complete(date, blueprint_level, fill = list(count = 0)) %>%
    ggplot(aes(
      x = date,
      y = count,
      color = blueprint_level,
      fill = blueprint_level
    )) +
    geom_area(na.rm = TRUE, position = position_fill(reverse = TRUE),linewidth=0) +
    scale_fill_manual(
      values = c("purple", "red", "orange", "yellow"),
      na.value = rgb(0, 0, 255, max = 255, alpha = 0, names = "transparent")
    ) +
    scale_color_manual(
      values = rgb(0, 0, 255, max = 255, alpha = 0, names = "transparent"),
      na.value = rgb(0, 0, 255, max = 255, alpha = 0, names = "transparent")
    ) +
    ylab("") +
    xlab("") +
    labs(title = "Blueprint Levels by County") +
    theme_minimal() +
    
    theme(
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
      
    )
}


# Create a Single Merged Graph --------------------------------------------
merged_graph <- function(start_date = ymd("2020-3-1"),
                         end_date = ymd("2022-3-1"),
                         metric_graphs = list("Blueprint Levels",
                                              "Total Hospitalizations",
                                              "Vaccine Doses"),
                         county = "Yolo",
                         timeline_input = COVID_Timeline)
{
  graphs = list(
    timeline_graph(
      start_date = start_date,
      end_date = end_date,
      input_data = timeline_input
    ),
    variant_graph(start_date = start_date,
                  end_date = end_date)
  ) #/ list
  
  if ("Percent Fully Vaccinated" %in% metric_graphs) {
    graphs <- append(graphs,
                     list(
                       metric_graph(
                         column_name = "pct_fully_vaccinated",
                         line_color = "darkgreen",
                         graph_title = "Percent Population Fully Vaccinated",
                         county_choice = county
                       )
                     ), 1)
  }
  
  if ("Vaccine Doses" %in% metric_graphs) {
    graphs <- append(graphs,
                     list(
                       metric_graph(
                         column_name = "total_doses",
                         line_color = "green",
                         graph_title = "Total Vaccine Doses",
                         county_choice = county
                       )
                     ), 1)
  }
  
  
  
  
  if ("Total Hospitalizations" %in% metric_graphs) {
    graphs <- append(graphs,
                     list(
                       metric_graph(
                         column_name = "total_hosp",
                         line_color = "darkred",
                         graph_title = "Daily Confirmed Hospitalizations",
                         county_choice = county
                       )
                     ), 1)
  }
  
  if ("Confirmed Hospitalizations" %in% metric_graphs) {
    graphs <- append(graphs,
                     list(
                       metric_graph(
                         column_name = "confirmed_hosp",
                         line_color = "red",
                         graph_title = "Daily Confirmed and Suspected Hospitalizations",
                         county_choice = county
                       )
                     ), 1)
  }
  
  if ("Blueprint Levels" %in% metric_graphs) {
    graphs <- append(graphs,
                     list(
                       blueprint_graph(
                         start_date = start_date,
                         end_date = end_date,
                         county_choice = county
                       )
                     ), 1)
  }
  
  
  
  num_graphs = length(graphs)
  
  graph_heights = c(5,
                    rep.int(2, times = num_graphs - 2),
                    1)
  
  ggpubr::ggarrange(plotlist = graphs,
                    nrow = num_graphs,
                    heights = graph_heights)
}


#merged_graph()


# UI ----------------------------------------------------------------------



ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    selectInput("county", label = "Select County",
                choices = c(county_list)),
    #/ selectInput
    
    
    sliderInput(
      "daterange",
      label = "Date Range",
      min = ymd("2019-9-1"),
      max = ymd("2023-4-1"),
      value = c(ymd("2020-3-1"), ymd("2022-3-1"))
    ),
    #/ sliderInput
    
    uiOutput("eventTypes"),
    #/ checkboxGroupInput Graphs
    
    checkboxGroupInput(
      "metrics",
      label = "Select Metric Graphs to Display",
      choices = c(
        "Blueprint Levels",
        "Total Hospitalizations",
        "Confirmed Hospitalizations",
        "Vaccine Doses",
        "Percent Fully Vaccinated"
      ),
      selected = c("Blueprint Levels",
                   "Total Hospitalizations",
                   "Percent Fully Vaccinated")
    ),
    #/ checkboxGroupInput Graphs
    
    sliderInput(
      inputId = "height",
      label = "Height",
      min = 500,
      max = 2000,
      value = 1000
    ),
    sliderInput(
      inputId = "width",
      label = "Width",
      min = 500,
      max = 2000,
      value = 1250
    ),
    
    downloadButton('downloadData', 'Download Timeline Data'),
    downloadButton('downloadPlot', 'Download Plot'),
    #/ Download Buttons
    
    tags$hr(),
    # horizontal line
    
    fileInput(
      "custom_timeline",
      "Upload a Custom Timeline File",
      multiple = TRUE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    #/ fileinput
    
    p(
      "Note: Only CSV files accepted. Be sure to preserve column names and formats from the Timeline Data file."
    )
    
  ),
  #/ sidebar panel
  
  mainPanel(tabsetPanel(
    type = "tabs",
    tabPanel("Timeline", plotOutput("timeline", height = "800px")),
    tabPanel(
      "Notes",
      h1("California COVID-19 Timeline"),
      p(
        "This is an interactive timeline of events related to the COVID-19 pandemic in California, along with data covering key metrics related to the course of the pandemic. The events are gathered from various news sources and internal CDPH resources, with citations available after downloading the data. "
      ),
      p(
        "This timeline is designed as an interactive tool. The menu items on the right allow you to adjust disease and vaccination metrics for specific counties, choose which metrics to show or hide, along with facilitating some control of what events are visible on the timeline."
      ),
      p(
        "All graphs automatically adjust to the range selected in the date slider. You can see more granular event details by choosing a shorter time span. Events are ranked in rough order of importance."
      ),
      p(
        "You can also create your own events timeline by downloading, modifying, and uploading new data into the timeline file. Data must be uploaded as a csv with the same formatting as the downloadable file. You can save your edited file for later use, although if you leave and restart the applicaton your data will not be saved."
      ),
      p(
        "You can alter the size of the diagram for export for use in slide presentations. You can save the timeline either by right clicking direclty on the application or by clicking the save button."
      ),
      em("Any questions or errors should be sent to brian.erly@cdph.ca.gov")
      
    )
  ) #/ tabset panel
  ) #/ main panel
  )
  )
  
  
  # Server ------------------------------------------------------------------
  
  
  
  server <- function(input, output, session) {
    #Update with Custom Data
    TimelineUpdator <- function() {
      if (isTruthy(input$custom_timeline$datapath)) {
        COVID_Timeline <-
          read_csv(file = input$custom_timeline$datapath) %>%
          mutate(date = mdy(date))
      } else {
        COVID_Timeline <- COVID_Timeline
      }
    }
    
    output$eventTypes = renderUI({
      checkboxGroupInput(
        "events",
        label = "Select Event Types to Display",
        choices = c(
          TimelineUpdator() %>%  select(type) %>% unique() %>% unlist() %>% as.character()
        ),
        selected = c(
          TimelineUpdator() %>%  select(type) %>% unique() %>% unlist() %>% as.character()
        )
      )
    })
    
    #
    #   # Check for custom data
    #   COVID_Timeline1 <- reactive({
    #
    #     #COVID_Timeline
    #
    #     print(input$custom_timeline$datapath)
    #
    #     req(input$custom_timeline)
    #
    #     read_csv(file= input$custom_timeline$datapath) %>%
    #       mutate(date = mdy(date))
    #
    #       })
    #
    
    
    # Make the plot
    
    make_plot <- function() {
      merged_graph(
        start_date = input$daterange[1],
        end_date = input$daterange[2],
        metric_graphs =
          input$metrics
        ,
        input$county,
        timeline_input = TimelineUpdator() %>%
          filter(type %in% input$events)
      )
    }
    
    # Draw the plot
    
    output$timeline <- renderPlot(
      width = function()
        input$width,
      height = function()
        input$height,
      res = 96,
      make_plot()
    )
    
    # Download Plot
    
    output$downloadPlot <- downloadHandler(
      filename = "Timeline plot.png",
      
      content = function(file) {
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
      
      content = function(file) {
        write.csv(data, file)
      }
    )
    
    
    
    
  }
  
  shinyApp(ui = ui, server = server)
  