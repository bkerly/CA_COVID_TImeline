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
library(ggrepel)
library(ggthemes)
library(plotly)
        
timeline_data <- readxl::read_excel("COVID Timeline.xlsx") %>%
  mutate(type = as.factor(type))


ui <- fluidPage(
  plotOutput("timeline")
)

server <- function(input, output, session) {
  output$timeline <- renderPlot({
    ggplot(data = timeline_data,
                   aes(
                     y=date,
                     x=0,
                     label = event,
                     text = description
                   )) +
      geom_vline(xintercept = 0)+
      #theme_void() +
      coord_cartesian(clip = "off") +
      geom_point(
        aes(color = type)
      ) +
      geom_text_repel(
        force        = 0.5,
        nudge_x      = 0.15,
        direction    = "y",
        hjust        = 0,
        segment.size = 0.2
      )  +
      xlim(-0.1, 0.8)+
      theme(
        axis.line.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.title.y = element_blank()
      )
    
    plot <- ggplot(data = timeline_data,
                   aes(
                     y=date,
                     x=0,
                     label = event,
                     text = description
                   )) +
      geom_vline(xintercept = 0)+
      #theme_void() +
      coord_cartesian(clip = "off") +
      geom_point(
        aes(color = type)
      ) +
      geom_text(
        hjust = 0, nudge_x = 0.1
      )  +
      xlim(-1, 10)+
      theme(
        axis.line.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.title.y = element_blank()
      )
    
    plot
    
    ggplotly(plot, tooltip = "text")%>% 
      style(textposition = "right")
    
    
  })
}

shinyApp(ui = ui, server = server)
