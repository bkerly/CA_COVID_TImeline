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
# Look how few packages I loaded! It's because I am an efficient and parsimonious coder.


# Load Data --------------------------------------------------------

metric_data <- read_csv("Metric Data.csv")
# This loads the day-by-day COVID metrics created by data_merger.R

COVID_Timeline <- readxl::read_xlsx("COVID Timeline.xlsx") %>%
  # This loads the written timeline data, which is hand coded in an .xlsx format.
  mutate(date = as.Date(date))
  # Makes sure the dates are read as dates! Also and by the way, excel is really bad at formatting dates consistently, so if you don't find an event you expect it's probably excel's fault

variant_data <- read_csv("prevalent variant.csv")
# This loads data about when different variants were most prevalent. 

county_list <- metric_data %>%
  select(county) %>%
  filter(county != "All California") %>%
  unique() %>%
  arrange() %>%
  unlist() %>%
  append("All California", 0) %>%
  unname()
# This (probably inefficient) bit just creates a list of all the counties in California, formatted as per the "metric data" list.


# Create Graph Template Functions  ------------------------------------------------------

# This function generates grapphs for any of the metrics in the "LOAD DATA" step
metric_graph <- function(county_choice = "All California", # Which county
                         line_color = "blue", # What color line on the graph
                         column_name = "total_hosp", # Which column of Metric Data do you want to pull data from
                         graph_title = "Daily Hospitalizations", # What do you want the graph title to be
                         start_date = ymd("2020-3-1"), # What are the start and end dates of the graph?
                         end_date = ymd("2022-3-1")) { 
  metric_data %>%
    filter(county == county_choice) %>%
    filter(date >= start_date,
           date <= end_date) %>% # Up to here just filteres the data to show only what you want
    ggplot(aes(x = date, y = .[[column_name]])) + 
    geom_area(stat = "identity", fill = line_color) + # Draws the basic graph
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
    xlim(start_date, end_date) # Everything down to here is just to make it pretty and consistent.
  
  
}

# Here's a test graph to try out! Uncomment to run
# metric_graph(column_name = "total_doses",
#              line_color = "green",
#              graph_title = "total doses",
#              county_choice = "Yolo")

# Since the timeline graph is formatted differently, it uses a different function
timeline_graph <- function(start_date = ymd("2020-3-1"), # Just picks the start and end dates as above.
                           end_date = ymd("2022-3-1"),
                           input_data = COVID_Timeline # You specify the input here so if you want you can filter it later.
                           ) {
  input_data %>%
    filter(date >= start_date,
           date <= end_date) %>% # Filter for the dates you want
    slice_max(importance, n = 15) %>% # Only shows the top 15 events for a given time span so it's not too crowded. Includes ties, though, so it could be more than 15 things.
    ggplot(aes(x = date,
               y = 1,
               label = event)) + 
    geom_hline(yintercept = 1) + # Draws a nice line for all the points to go on
    geom_point(aes(color = type),
               size = 3)  + # Adds a point on the line for each event
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
    ) + # Creates text for each event and draws a line to the point. See the help for "geom_text_repel" to figure out what those options are.
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
    xlim(start_date, end_date) # The rest just makes it pretty
  
}

# This is a trial graph you can uncomment to try. 
# timeline_graph(start_date = ymd("2020-3-15"),
#                end_date = ymd("2020-4-1"))


# Finally, the variant graphs also have their own formatting
variant_graph <- function(start_date = ymd("2020-3-1"),
                          end_date = ymd("2022-3-1") # Adds in start and stop dates
                          ) {
  variant_data %>%
    filter(Start <= end_date,
           End >= start_date) %>% 
    mutate(Start = case_when(Start > start_date ~ Start,
                             TRUE ~ start_date)) %>%
    mutate(End = case_when(End < end_date ~ End,
                           TRUE ~ end_date)) %>% 
    # OK, so this is wonky and took me a while to work out, so I'll try to explain it here.
    # Basically, every variant has a start and an end point.
    # And if those are all within the bounds of the graph, great! That's easy.
    # But if they're NOT, then you need to truncate the start and end point to match the start and end date from the graph.
    # That's what that logic does in the above two "mutates"
    mutate(midpoint = as.Date((as.numeric(End) +
                                 as.numeric(Start)) / 2, origin = '1970-01-01')) %>%
    # Oh, and this finds the middle of each variant time span for labeling purposes. Why didn't I use lubridate for this? I don't know. 
    ggplot() +
    geom_rect(aes(
      xmin = Start,
      xmax = End,
      ymin = 1,
      ymax = 0,
      fill = variant_name
    )) + # This draws a rectangle for each variant.
    geom_label(aes(x = midpoint,
                   y = 0.5,
                   label = variant_name)) +
    # This labels each variant rectangle
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
    xlim(start_date, end_date) # Everything down to here is just formatting, again. Why isn't this repetitive formatting a single function? Again, I don't know.
  
}

# Here's a test
# variant_graph(start_date = ymd("2021-1-1"),
#               end_date = ymd("2023-1-1")
# )

# Blueprint Graph

blueprint_graph <- function(county_choice = "All California",
                            start_date = ymd("2020-3-1"),
                            end_date = ymd("2022-3-1") # Sets your county filters and date filters, as above.
                            ) { 
  
  
    metric_data %>%
    filter(case_when(
      county_choice == 'All California' ~ TRUE,
      TRUE ~ county %in% county_choice
    )) %>% 
    filter(date >= start_date,
           date <= end_date) %>% # Filters based on your county and dates of interest. Could probably be one "filter" with commas, but here we are.
    group_by(date, blueprint_level) %>%
    summarize(count = n()) %>%
    ungroup()  %>% # This collapses the county- and date-wise listing of blueprint levels into just date-wise listings.
    complete(date, blueprint_level, fill = list(count = 0)) %>%
    # This makes sure that missing values (if no county is in that blueprint level) are 0 or else the graph would try to interpolate! And that's not good.
    ggplot(aes(
      x = date,
      y = count,
      color = blueprint_level,
      fill = blueprint_level
    )) +
    geom_area(na.rm = TRUE, position = position_fill(reverse = TRUE),linewidth=0) + # Makes a basic area graph
    scale_fill_manual(
      values = c("purple", "red", "orange", "yellow"),
      na.value = rgb(0, 0, 255, max = 255, alpha = 0, names = "transparent")
    ) + # Makes the colors match the blueprint colors, and makes missing values transparent. 
    scale_color_manual(
      values = rgb(0, 0, 255, max = 255, alpha = 0, names = "transparent"),
      na.value = rgb(0, 0, 255, max = 255, alpha = 0, names = "transparent")
    ) + # Also makes misisng values transparent. Are both necessary? I haven't tried without it.
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
      
    ) # Repetative formatting? Nice.
}


# Create a Single Merged Graph --------------------------------------------
# Once you have all the individual graphs written as functions up above, you need to make them into one super-image with consistent dates, county filters, etc. That makes it convenient to code within shiny and export etc.  
# The way it works is you make a list of the graphs you want and then use "ggpubr::ggarrange" to make them into one big graph image!

merged_graph <- function(start_date = ymd("2020-3-1"),
                         end_date = ymd("2022-3-1"), # Start and end dates for ALL graphs
                         metric_graphs = list("Blueprint Levels",
                                              "Total Hospitalizations",
                                              "Vaccine Doses"), # Chooses which of the metric graphs you want to show. 
                         county = "Yolo", #County you want to graph, `All California` is statewide data.
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
# This initializes the list of graphs you'll show. It makes sure you show at least the timeine and the variant graphs. 
  
  # And then, graph by graph, you add each additional graph you want to add. It builds from second to last up to second to first, if that makes sense. 
  
  if ("Percent Fully Vaccinated" %in% metric_graphs) {
    graphs <- append(graphs, # adds to the growing list of graphs
                     list(
                       metric_graph(
                         column_name = "pct_fully_vaccinated",
                         line_color = "darkgreen",
                         graph_title = "Percent Population Fully Vaccinated",
                         county_choice = county
                       ) 
                     )# Graph function arguments are given as a list.
                     , 1 # Adds after position 1 (which is the second position)
                     )
  }
  
  # This repeats what was above! Could it be a function to avoid rewriting, especially since they're all metric graphs? Yeah...
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
  
  # Oh, but see! This one is a little different, it's the blueprint graph! Couldn't make this a function!
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
  
  
  # These next two things help to make sure the graphs are properly formatted, with the timeline real big and the variants real small and everything else in the middle. 
  num_graphs = length(graphs) 
  
  graph_heights = c(5, # Timeline graph; big!
                    rep.int(2, times = num_graphs - 2), # Blueprint and metric graphs; in the middle!
                    1) # variant graph; small!
  
  ggpubr::ggarrange(plotlist = graphs, # The lsit of all the graphs
                    nrow = num_graphs, # One per row
                    heights = graph_heights) # with the heights you define above
}

# Uncomment to try it out with the default settings
#merged_graph()


# UI ----------------------------------------------------------------------

#Nice! Now we're doing shiny apps.


ui <- fluidPage(sidebarLayout(
  
  # This defines the sidebar, where all the switches and sliders and knobs and buttons are. The sidebar stays the same throughout. 
  sidebarPanel(
    selectInput("county", label = "Select County",
                choices = c(county_list)), # A dropdown list of all the counties
    #/ selectInput
    
    
    sliderInput(
      "daterange",
      label = "Date Range",
      min = ymd("2019-9-1"),
      max = ymd("2023-4-1"),
      value = c(ymd("2020-3-1"), ymd("2022-3-1"))
    ), # A slider of all the available dates. It defaults to a little less than the maximum width for no good reason.
    #/ sliderInput
    
    uiOutput("eventTypes"),
    # Lets you choose the different times of events you want to show. "eventTypes" is defined in the server based on what is available in the timeline. This makes it so it's reactive if you (the person updating this) adds a new event category to the timeline, or if someone uploads a new timeline with some funky new catagory of events, like a "Diamond Princess" timeline.
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
    # Lets you choose which metric graphs to display.
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
    ), # Theese let you manually resize the graph, rather than having it scale to your screen. Could be good if you are making a powerpoint.
    
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
    ), # People can upload custom timeline data! But they'll probably screw up the formatting somehow. 
    #/ fileinput
    
    p(
      "Note: Only CSV files accepted. Be sure to preserve column names and formats from the Timeline Data file."
    )
    
  ),
  #/ sidebar panel
  
  mainPanel(tabsetPanel(
    type = "tabs",
    tabPanel("Timeline", plotOutput("timeline", height = "800px")), # So this is tab 1! That's it, just one graph (made up of many graphs)
    tabPanel( # This is tab 2, which is a description of the timeline. This could probably use a read through and edit, but it's good filler text for now. 
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
      em("Any questions or errors should be sent to brian.erly@cdph.ca.gov") # Gotta change this!
      
    )
  ) #/ tabset panel
  ) #/ main panel
  ) #/ sidebar layout
  ) #/ UI
  
  
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
    } # If someone has uploaded custom timeline data this will switch over to using that. 
    #/ TimelineUpdator
    
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
      )# This changes the event types options displayed based on what's available in the timeline (either the default or custom one)
    })
    

    
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
      ) # OK, I know it was a lot of functions before the UI setting, but look how little this function is!
    }
    
    # Draw the plot
    
    output$timeline <- renderPlot(
      width = function()
        input$width,
      height = function()
        input$height,
      res = 96, # I tried making this variable but it didn't work.
      make_plot()
    ) # And this just draws the plot at the size you said.
    
    # Download Plot
    
    output$downloadPlot <- downloadHandler(
      filename = "Timeline plot.png",
      
      content = function(file) {
        png(file,
            width = input$width,
            height = input$height)
        
        make_plot() %>% print()
        
        dev.off()
        
      } # Lets you download the plot. Actually redraws it, weirdly enough.
    )
    
    # Downloading Data
    data <- COVID_Timeline
    
    output$downloadData <- downloadHandler(
      filename = "Timeline data.csv",
      
      content = function(file) {
        write.csv(data, file)
      }
    ) # This lets folks download the data for the timeline, which might be useful if they want to check the sourcing or make their own timeline
    
    
    
    
  } #/ server
  
  shinyApp(ui = ui, server = server) # Run it!
  