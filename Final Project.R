library(shinydashboard)
library(fontawesome)
library(quantmod)
library(dplyr)
library(fpp3)
library(ggplot2)
library(ggeasy)
library(plotly)
library(ggpubr)
library(tidyquant)
library(TTR)
library(scales)
library(lubridate)

pedestrian <- pedestrian

library(shiny)
library(shinyWidgets)

welcometab <- menuItem("Welcome!", tabName = "welcome")
tseriestab <- menuItem("Time Series - Pedestrian Counts", tabName = "plot")
featuretab <- menuItem("Time Series - Weekday Vs Weekend", tabName = "compare")
optionstab <- menuItem("Time Series - Additional Plots", tabName = "additional")
interpttab <- menuItem("Interpretations", tabName = "interpt")

sidebar <- dashboardSidebar(sidebarMenu(
  welcometab,
  tseriestab,
  featuretab,
  optionstab,
  interpttab))

body <- dashboardBody(tabItems(
    tabItem(
      tabName = "welcome", h2("Welcome to my BAS 475 Midterm Project!!! In this app, we are using the pedestrian dataset, which is a daily time series of class 'tsibble' that shows hourly pedestrian counts at four sites near Melbourne, Australia. You will notice four additional tabs: Time Series - Pedestrian Counts, Time Series - Weekend Vs Weekday, Time Series - Additional Plots, and Interpretations."),
    ),
    tabItem(
      tabName = "plot", h2("Shown is the full time series. You can 'deselect' or 'select' a site by clicking on it in the legend. For interpretation, go to tab 'Interpretations'."),
      plotlyOutput("sensors_plot")
    ),
    tabItem(
        tabName = "compare", h2("Select a data range between the two default start and end dates. Then, select a site. Shown is the pedestrian volume on the weekends vs weekdays. For interpretation, go to tab 'Interpretations'."),
        dateInput("start_compare", "Select Start Date", value = "2015-01-01"),
        dateInput("end_compare", "Select End Date", value = "2016-12-31"),
        selectInput("sname1", "Choose A Site", choices = names(table(pedestrian$Sensor)), selected = names(table(pedestrian$Sensor))),
        submitButton(),
        plotlyOutput("week_plot"),
    ),
    tabItem(
          tabName = "additional", h2("Select which plot you would like to view. For interpretation, go to tab 'Interpretations'."),
          selectInput("sname3", "Choose A Plot", choices = (c("Autocorr", "Seasonal", "Decomposition"))),
          submitButton(),
          plotOutput("add_plot"),
    ),
    tabItem(
          tabName = "interpt", h2("The high to low pedestrian counts over time are as follows: Birrarung Marr, Bourke Street Mall (North), Southern Cross Station, and QV Market-Elizabeth St (West).
                                Birrarung Marr has an even distribution of weekend versus weekday pedestrians, Bourke Street Mall (North) has an even distribution of weekend versus weekday pedestrians,
                                  QV Market-Elizabeth Street has a notably higher count of pedestrians later in the evening on the weekday versus the weekend, and Southern Cross Station has a very high amount of pedestrians all day during the week versus a small amount during the weekend.
                                  "),
  )
))

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "BAS 475 Midterm Project"),
    sidebar,
    body
  )
)

  server <- function(input, output) {
    output$sensors_plot <- renderPlotly({
      pedestrian %>%
        autoplot(Count) +
        labs(title = "Pedestrian Count")
    })
    output$week_plot <- renderPlotly({
      start <- input$start_compare
      end <- input$end_compare
      pedestrian %>%
        mutate(
          Day = lubridate::wday(Date, label = TRUE),
          Weekend = (Day %in% c("Sun", "Sat"))
        ) %>%
        filter(Sensor == input$sname1) %>%
        filter(Date >= input$start_compare) %>%
        filter(Date <= input$end_compare) %>%
        ggplot(aes(x = Time, y = Count, group = Date)) +
        geom_line(aes(col = Weekend)) +
        facet_grid(Sensor ~ .)
    })
output$add_plot <- renderPlot({
  switch(
    input$sname3,
    Autocorr = pedestrian %>%
      fill_gaps() %>%
      ACF(Count) %>%
      autoplot() + theme_dark(),
    
    Seasonal = pedestrian %>%
      fill_gaps() %>%
      gg_season(Count),
    
    Decomposition = pedestrian %>%
      fill_gaps(.full = TRUE) %>%
      model(classical_decomposition(Count, type = "additive")) %>%
      components() %>%
      autoplot()
  )
})
}

shinyApp(ui = ui, server = server)

