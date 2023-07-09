library(plotly)
library(shiny)

# Create the dataframe
df <- data.frame(
  Year = c("Event01", "Event02", "Event03", "Event04", "Event05", "Event06"),
  City = c("Athina", "Paris", "St. Louis", "Athina", "London", "Stockholm"),
  NOC = c("GRE", "FRA", "USA", "GRE", "GBR", "SWE"),
  gold_as_host = c(42, 89, 554, 42, 202, 108),
  gold_b_host = c(0, 5, 30, 1, 9, 48)
)

# Shiny UI
ui <- fluidPage(
  plotlyOutput("chart"),
  selectInput(
    inputId = "eventSelection",
    label = "Select Events",
    choices = df$Year,
    multiple = TRUE
  )
)

# Shiny server
server <- function(input, output) {
  output$chart <- renderPlotly({
    filtered_df <- df[df$Year %in% input$eventSelection, ]
    
    plot_ly(filtered_df, x = ~Year, y = ~gold_as_host, type = "scatter", mode = "lines", name = "Gold as Host") %>%
      add_trace(data = filtered_df, x = ~Year, y = ~gold_b_host, type = "scatter", mode = "lines", name = "Gold b Host") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Gold Total"),
        title = "Gold Total Over the Events"
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
