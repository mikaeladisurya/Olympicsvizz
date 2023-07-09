library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))
glimpse(summer_olympic)

age_data <- summer_olympic[complete.cases(summer_olympic$Age), ]

medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Games) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Add the rank column for each game
medal_rank <- medal_per_country %>%
  group_by(Games) %>%
  mutate(Rank = as.integer(rank(desc(medal_count)))) %>%
  ungroup()

# Filter the data for "1900 Summer" games to check result
df_1900_summer <- medal_rank %>%
  filter(Games == "1900 Summer")

fig <- plot_ly(
  data = medal_per_country, x = medal_per_country$Games, y = medal_per_country$medal_count,
  marker = list(
    size = 10,
    color = 'rgba(255, 182, 193, .9)',
    line = list(color = 'rgba(152, 0, 0, .8)',width = 2)
    )
  )
fig <- fig %>%
  layout(
    title = 'Styled Scatter',
    yaxis = list(zeroline = FALSE),
    xaxis = list(zeroline = FALSE)
  ) %>%
  style(hoverinfo = "none")

fig


# Create the scatter plot using Plotly
plot <- plot_ly(data = medal_per_country, x = ~Games, y = ~medal_count, type = "scatter", mode = "markers")

# Customize the scatter plot
plot <- plot %>% layout(
  xaxis = list(title = "Games"),
  yaxis = list(title = "Medal Count"),
  showlegend = TRUE
)

# Display the scatter plot
plot

# Group the data by Games and rank the medal_count
country_ranked <- medal_per_country %>%
  group_by(Games) %>%
  mutate(rank = dense_rank(desc(medal_count))) %>%
  ungroup()

# Filter the top 3 medal_count for each Games
country_ranked_filtered <- country_ranked %>%
  filter(rank <= 3)

# Create the scatter plot using Plotly
plot <- plot_ly(data = country_ranked_filtered, x = ~Games, y = ~medal_count, color = ~as.factor(rank), 
                colors = c("#FF0000", "#00FF00", "#0000FF"), type = "scatter", mode = "markers")

# Add the remaining points with gray color
plot <- plot %>% add_trace(data = country_ranked[!country_ranked$NOC %in% country_ranked_filtered$NOC, ],
                           x = ~Games, y = ~medal_count, color = I("gray"), type = "scatter", mode = "markers")

# Customize the scatter plot
plot <- plot %>% layout(
  xaxis = list(title = "Games"),
  yaxis = list(title = "Medal Count"),
  showlegend = TRUE,
  legend = list(title = "Rank"),
  hoverinfo = "text",
  text = ~paste("NOC: ", NOC)
)

plot
