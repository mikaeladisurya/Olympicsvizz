library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))
glimpse(summer_olympic)

# Loading athlete data
host_country <- read.csv(paste0(getwd(),"/DATA/host_country_code.csv"))

# join athlete data with host country codes to add NOC for hosting country
summer_olympic <-  merge(x = summer_olympic, y = host_country, by = "City", all = FALSE) # country_code data just the one that intersect

medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Games, Host_IOC) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# # Add the rank column for each game
# country_ranked <- medal_per_country %>%
#   group_by(Games) %>%
#   mutate(Rank = as.integer(rank(desc(medal_count)))) %>%
#   ungroup()
# 
# # Filter the top 3 medal_count for each Games
# country_ranked_filtered <- country_ranked %>%
#   filter(rank <= 3)

# Filter the host country for each Games
host_country_filter <- medal_per_country %>%
  filter(NOC==Host_IOC)

# Create the scatter plot using Plotly
plot <- plot_ly(data = host_country_filter, x = ~Games, y = ~medal_count, color = "red", type = "scatter", mode = "markers",
                text = ~paste("NOC: ", NOC), name = "Host Country")

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
