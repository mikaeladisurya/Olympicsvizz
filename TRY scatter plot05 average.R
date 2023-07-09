library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Loading athlete data
host_country <- read.csv(paste0(getwd(),"/DATA/host_country_code.csv"))

# join athlete data with host country codes to add NOC for hosting country
summer_olympic <-  merge(x = summer_olympic, y = host_country, by = "City", all = FALSE) # country_code data just the one that intersect

medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Games, Host_IOC) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Calculate the average medal count for each game
df_avg <- medal_per_country %>%
  group_by(Games) %>%
  summarize(avg_medal_count = mean(medal_count)) %>%
  mutate(NOC = "Average")

# Bind the average data to the original dataset
df_combined <- bind_rows(medal_per_country, df_avg)

# Filter the host country for each Games
host_country_filter <- medal_per_country %>%
  filter(NOC==Host_IOC)

# Create the scatter plot using Plotly
plot <- plot_ly(data = host_country_filter, x = ~Games, y = ~medal_count, color = "red", type = "scatter", mode = "markers",
                text = ~paste("NOC: ", NOC), name = "Host Country")

# Create the scatter plot using Plotly
plot <- plot %>% add_trace(data = df_avg, x = ~Games, y = ~avg_medal_count, color = "blue", type = "scatter", mode = "markers",
                text = ~paste("NOC: ", NOC), name = "Average")

# Add the remaining points with gray color
plot <- plot %>% add_trace(data = medal_per_country[!medal_per_country$NOC %in% host_country_filter$NOC, ],
                           x = ~Games, y = ~medal_count, color = I("gray"), opacity = 0.5, type = "scatter", mode = "markers",
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
