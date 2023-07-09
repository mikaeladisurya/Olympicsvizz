library(plotly)
library(dplyr)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Loading athlete data
host_country <- read.csv(paste0(getwd(),"/DATA/host_country_code.csv"))

# join athlete data with host country codes to add NOC for hosting country
summer_olympic <-  merge(x = summer_olympic, y = host_country, by = "City", all.x = TRUE) # country_code data just the one that intersect

medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year, Host_IOC) %>%
  summarise(medal_count = sum(!is.na(Medal)))

medal_per_country <- data.frame(medal_per_country)

# Filter the host country for each Year
host_country_filter <- medal_per_country %>%
  filter(NOC==Host_IOC)

sorted_host_country <- host_country_filter[order(host_country_filter$Year), ]

# Calculate the average medal count for each game
df_avg <- medal_per_country %>%
  group_by(Year) %>%
  summarize(avg_medal_count = mean(medal_count)) %>%
  mutate(NOC = "Average")

# Bind the average data to the original dataset
df_combined <- bind_rows(medal_per_country, df_avg)

fig <- plot_ly(sorted_host_country, x = ~Year, y = ~medal_count,
               name = 'Host Country', type = 'scatter', 
               mode = 'markers')

# Add the rank column for each game
country_ranked <- medal_per_country %>%
  group_by(Year) %>%
  mutate(Rank = as.integer(rank(desc(medal_count)))) %>%
  ungroup()

country_ranked <- data.frame(country_ranked)

# Filter the top 3 medal_count for each Games
country_ranked_filtered <- country_ranked %>%
  filter(Rank <= 3)

# Create the scatter plot using Plotly
fig <- fig %>% add_trace(data = country_ranked_filtered, x = ~Year, y = ~medal_count, color = ~as.factor(rank), 
                colors = c("#FF0000", "#00FF00", "#0000FF"), type = "scatter", mode = "markers")

# Add the remaining points with gray color
fig <- fig %>% add_trace(data = medal_per_country[!medal_per_country$NOC %in% host_country_filter$NOC, ],
                         x = ~Year, y = ~medal_count, color = I("gray"), opacity = 0.5, type = "scatter", mode = "markers",
                         text = ~paste("NOC: ", NOC), name = "Others")

fig
