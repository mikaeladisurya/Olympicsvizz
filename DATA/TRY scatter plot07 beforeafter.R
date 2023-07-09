# https://towardsdatascience.com/lollipop-dumbbell-charts-with-plotly-696039d5f85

library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Loading athlete data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))

# join athlete data with host country codes to add NOC for hosting country
# summer_olympic <-  merge(x = summer_olympic, y = host_country, by = "City", all = FALSE) # country_code data just the one that intersect

medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Calculate the average medal count for each game
df_avg <- medal_per_country %>%
  group_by(Year) %>%
  summarize(avg_medal_count = mean(medal_count)) %>%
  mutate(NOC = "Average")

# Bind the average data to the original dataset
df_combined <- bind_rows(medal_per_country, df_avg)

# Filter the host country for each Games
# host_country_filter <- medal_per_country %>%
#   filter(NOC==Host_IOC)

# Create the scatter plot using Plotly
plot <- plot_ly(data = host_country, x = ~Year, y = ~medal_as_host, color = I("red"), type = "scatter", mode = "markers",
                text = ~paste("NOC: ", NOC), name = "Host Country")

# Create the scatter plot using Plotly
plot <- plot %>% add_trace(data = host_country, x = ~Year, y = ~medal_b_host, color = I("coral"), type = "scatter", mode = "markers",
                           text = ~paste("NOC: ", NOC), name = "Before Host Country")

plot <- plot %>% add_segments(x = ~Year, xend = ~Year, y = ~medal_as_host, yend = ~medal_b_host, color = I("gray80") , showlegend = FALSE)

# Create the scatter plot using Plotly
plot <- plot %>% add_trace(data = df_avg, x = ~Year, y = ~avg_medal_count, color = I("blue"), type = "scatter", mode = "markers",
                           text = ~paste("NOC: ", NOC), name = "Average")

# Add the remaining points with gray color
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
