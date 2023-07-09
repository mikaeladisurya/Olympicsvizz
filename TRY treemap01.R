library(plotly)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))
# Calculate BMI
summer_olympic$BMI <- summer_olympic$Weight / ((summer_olympic$Height / 100) ^ 2)

# Loading alphabetic country codes data
country_code <- read.csv(paste0(getwd(),"/DATA/alphabetic_country_codes.csv"))
# join athlete data with country codes to add iso3 country code
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all.x = TRUE)

# Loading host_medal data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))
# Add a new column with the sum of gold_as_host and gold_b_host
host_country$medal_diff <- host_country$medal_as_host - host_country$medal_b_host

# medal per country in each games
medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Calculate the average medal count for each game
medal_avg_country <- medal_per_country %>%
  group_by(Year) %>%
  summarize(avg_medal_count = round(mean(medal_count), 2)) %>%
  mutate(NOC = "Average")

# medal per country in each sport
medal_sport_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Sport) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Create treemap
fig2 <- plot_ly(
  type = "treemap",
  labels = medal_sport_country$Sport,
  parents = medal_sport_country$NOC,
  values = medal_sport_country$medal_count,
  marker = list(
    colors = "blue",  # Color for all sections
    line = list(color = "black", width = 1)
  )
)

# Customize treemap layout
fig2 <- fig2 %>% layout(
  title = "Medal Count by Sport and NOC",
  font = list(size = 14)
)

# Display the treemap
fig2


fig <- plot_ly(
  type="treemap",
  labels=c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
)
fig




fig <- plot_ly(
  type="treemap",
  labels=c("AFG", "AFG", "AFG", "AFG", "AHO", "AHO", "AHO", "AHO", "AHO", "ALB", "ALB", "ALB"),
  parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
)
fig

# Data
data <- data.frame(
  labels=c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
)

# Create treemap
fig <- plot_ly(
  type = "treemap",
  labels = c("AFG", "AFG", "AFG", "AFG", "AHO", "AHO", "AHO", "AHO", "AHO", "ALB", "ALB", "ALB"),
  parents = c("Athletics", "Boxing", "Football", "Hockey", "Judo", "Taekwondo", "Wrestling", "Athletics", "Boxing", "Football", "Hockey", "Judo"),
  values =c(1, 1, 1, 2, 1, 2, 0, 2, 2, 4, 3, 0),
  marker = list(
    colors = "blue",  # Color for all sections
    line = list(color = "black", width = 1)
  )
)

# Customize treemap layout
fig <- fig %>% layout(
  title = "Medal Count by Sport and NOC",
  font = list(size = 14)
)

# Display the treemap
fig

