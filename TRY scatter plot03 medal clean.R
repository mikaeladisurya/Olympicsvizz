library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))
glimpse(summer_olympic)

medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Games) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Add the rank column for each game
country_ranked <- medal_per_country %>%
  group_by(Games) %>%
  mutate(Rank = as.integer(rank(desc(medal_count)))) %>%
  ungroup()

# Filter the top 3 medal_count for each Games
country_ranked_filtered <- country_ranked %>%
  filter(Rank <= 3)

# Create the scatter plot using Plotly
plot <- plot_ly(data = country_ranked_filtered, x = ~Games, y = ~medal_count, color = ~as.factor(rank), 
                colors = c("#FF0000", "#00FF00", "#0000FF"), type = "scatter", mode = "markers")

# Add the remaining points with gray color
plot <- plot %>% add_trace(data = country_ranked[!country_ranked$NOC %in% country_ranked_filtered$NOC, ],
                           x = ~Games, y = ~medal_count, color = I("gray"), type = "scatter", mode = "markers",
                           text = ~paste("NOC: ", NOC))

# Customize the scatter plot
plot <- plot %>% layout(
  xaxis = list(title = "Games"),
  yaxis = list(title = "Medal Count"),
  showlegend = TRUE,
  legend = list(title = "Rank"),
  text = ~paste("NOC: ", NOC),
  hoverinfo = "text"
  
)

plot
