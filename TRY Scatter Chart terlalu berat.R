library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

age_data <- summer_olympic[complete.cases(summer_olympic$Age), ]

fig <- plot_ly(data = summer_olympic, x = summer_olympic$Year, y = summer_olympic$Age,
               marker = list(size = 10,
                             color = 'rgba(255, 182, 193, .9)',
                             line = list(color = 'rgba(152, 0, 0, .8)',
                                         width = 2)))
fig <- fig %>%
  layout(
    title = 'Styled Scatter',
    yaxis = list(zeroline = FALSE),
    xaxis = list(zeroline = FALSE)
    ) %>%
  style(hoverinfo = "none")

fig
