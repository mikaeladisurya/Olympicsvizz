library(plotly)

# Create the dataframe
df <- data.frame(
  Year = c(1896, 1900, 1904, 1906, 1908, 1912),
  City = c("Athina", "Paris", "St. Louis", "Athina", "London", "Stockholm"),
  NOC = c("GRE", "FRA", "USA", "GRE", "GBR", "SWE"),
  gold_as_host = c(42, 89, 554, 42, 202, 108),
  gold_b_host = c(0, 5, 30, 1, 9, 48)
)

# Create a plotly line chart
chart <- plot_ly() %>%
  add_trace(data = df, x = ~Year, y = ~gold_as_host, type = "scatter", mode = "lines", name = "Gold as Host") %>%
  add_trace(data = df, x = ~Year, y = ~gold_b_host, type = "scatter", mode = "lines", name = "Gold b Host") %>%
  layout(
    xaxis = list(title = "Year"),
    yaxis = list(title = "Gold Total"),
    title = "Gold Total Over the Years",
    sliders = list(
      list(
        active = 1,
        currentvalue = list(prefix = "Year: "),
        pad = list(t = 30),
        steps = lapply(unique(df$Year), function(yr) {
          list(
            label = as.character(yr),
            method = "update",
            args = list(list(visible = list(df$Year == yr)))
          )
        })
      )
    )
  )

# Display the chart
chart
