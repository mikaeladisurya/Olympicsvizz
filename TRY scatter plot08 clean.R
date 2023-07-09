# https://towardsdatascience.com/lollipop-dumbbell-charts-with-plotly-696039d5f85

library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Loading host_medal data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))

# medal per country in each games
medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Calculate the average medal count for each game
medal_avg_country <- medal_per_country %>%
  group_by(Year) %>%
  summarize(avg_medal_count = mean(medal_count)) %>%
  mutate(NOC = "Average")

# Create the scatter plot using Plotly for host country
plot <- plot_ly(data = host_country, color = I("gray80"))

# Create the segment connecting before and become host
plot <- plot %>% add_segments(x = ~Year, xend = ~Year, y = ~medal_as_host, yend = ~medal_b_host, color = I("gray80") , showlegend = FALSE)

plot <- plot %>% add_trace(data = host_country, x = ~Year, y = ~medal_as_host, color = I("red"), type = "scatter", mode = "markers",
                text = ~paste("NOC: ", NOC), name = "Host Country")

# Create the scatter plot for before host country
plot <- plot %>% add_trace(data = host_country, x = ~Year, y = ~medal_b_host, color = I("coral"), type = "scatter", mode = "markers",
                           text = ~paste("NOC: ", NOC), name = "Before Host Country")

# Create the scatter plot for average medal in each games
plot <- plot %>% add_trace(data = medal_avg_country, x = ~Year, y = ~avg_medal_count, color = I("blue"), type = "scatter", mode = "markers",
                           text = ~paste("NOC: ", NOC), name = "Average")

# Add the remaining points/other countries with gray color
plot <- plot %>% add_trace(data = medal_per_country,
                           x = ~Year, y = ~medal_count, color = I("grey"), opacity = 0.2, type = "scatter", mode = "markers",
                           text = ~paste("NOC: ", NOC), name = "Others")

# Customize the scatter plot
plot <- plot %>% layout(
  xaxis = list(title = "Games"),
  yaxis = list(title = "Medal Count"),
  showlegend = TRUE,
  legend = list(title = "Rank"),
  hoverinfo = "text"
  
)

plot
