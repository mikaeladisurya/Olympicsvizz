library(plotly)
library(shiny)

# Create the dataframe
df <- data.frame(
  Year = c(1896, 1900, 1904, 1906, 1908, 1912),
  City = c("Athina", "Paris", "St. Louis", "Athina", "London", "Stockholm"),
  NOC = c("GRE", "FRA", "USA", "GRE", "GBR", "SWE"),
  gold_as_host = c(42, 89, 554, 42, 202, 108),
  gold_b_host = c(0, 5, 30, 1, 9, 48)
)

# Shiny UI
ui <- fluidPage(
  plotlyOutput("chart"),
  sliderInput(
    inputId = "yearRange",
    label = "Year Range",
    min = min(df$Year),
    max = max(df$Year),
    value = c(min(df$Year), max(df$Year)),
    step = 4
  )
)

# Shiny server
server <- function(input, output) {
  output$chart <- renderPlotly({
    filtered_df <- df[df$Year >= input$yearRange[1] & df$Year <= input$yearRange[2], ]
    
    plot_ly(filtered_df, x = ~Year, y = ~gold_as_host, type = "scatter", mode = "lines", name = "Gold as Host") %>%
      add_trace(data = filtered_df, x = ~Year, y = ~gold_b_host, type = "scatter", mode = "lines", name = "Gold b Host") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Gold Total"),
        title = "Gold Total Over the Years"
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)