library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Loading athlete data
host_country <- read.csv(paste0(getwd(),"/DATA/host_country_code.csv"))

# join athlete data with host country codes to add NOC for hosting country
summer_olympic <-  merge(x = summer_olympic, y = host_country, by = "City", all.x = TRUE) # country_code data just the one that intersect

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
plot <- plot_ly(data = host_country_filter, x = ~Games, y = ~medal_count, type = "scatter", mode = "lines",
                text = ~paste("NOC: ", NOC), name = "Host Country")

plot
