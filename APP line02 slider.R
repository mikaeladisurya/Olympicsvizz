# ----Combining from the References:---------------------
# Lecture Notes
# https://stackoverflow.com/questions/54816608/customizing-bin-widths-in-plotlys-histogram-function-in-r
# https://plotly.com/r/histograms/
# https://rpubs.com/juanhklopper/control_histogram_color_using_Plotly_for_R
# https://towardsdatascience.com/lollipop-dumbbell-charts-with-plotly-696039d5f85

library(shiny)
library(plotly)
library(dplyr)

# Loading host_medal data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))

# Add a new column with the sum of gold_as_host and gold_b_host
host_country$medal_diff <- host_country$medal_as_host - host_country$medal_b_host

# UI code
ui <- fluidPage(
  titlePanel("Line Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "yearRange",
        label = "Games Range",
        min = min(host_country$Year),
        max = max(host_country$Year),
        value = c(min(host_country$Year), max(host_country$Year)),
        step = 4, sep = ""
      )
    ),
    mainPanel(
      plotlyOutput("line_host"),
      plotlyOutput("chart")
    )
  )
)

# Server code
server <- function(input, output) {
  
  # Create the line chart using Plotly for difference of before and after becoming host
  output$line_host <- renderPlotly({
    filtered_host <- host_country[host_country$Year >= input$yearRange[1] & host_country$Year <= input$yearRange[2], ]
    
    plot_ly(filtered_host, x = ~Year, y = ~medal_diff, type = "scatter", mode = "lines+markers", name = "Gold as Host") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Gold Total"),
        title = "Before After Medal Difference of Hosting Country"
      )
  }) # line chart host country
  
}

# Run the app
shinyApp(ui, server)