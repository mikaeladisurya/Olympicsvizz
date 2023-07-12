library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyverse)
library(rgdal)
library(DT)

##----Prepare dataframe-------------------------------------------------------
# world spatial polygon data frame
world_spdf <- readOGR(paste0(getwd(),"\\DATA\\world_shape_file\\"), 
                      "TM_WORLD_BORDERS_SIMPL-0.3",
                      verbose=FALSE
)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))
# Calculate BMI
summer_olympic$BMI <- summer_olympic$Weight / ((summer_olympic$Height / 100) ^ 2)

# Loading alphabetic country codes data
country_code <- read.csv(paste0(getwd(),"/DATA/alphabetic_country_codes.csv"))
# join athlete data with country codes to add iso3 country code
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all.x = TRUE)

# delete NA row in ISO column
summer_olympic <- summer_olympic[!is.na(summer_olympic$ISO),]

# Loading host_medal data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))
# Add a new column with the difference as host and before it
host_country$cbronze_diff <- host_country$bronze_as_host - host_country$bronze_b_host
host_country$csilver_diff <- host_country$silver_as_host - host_country$silver_b_host
host_country$cgold_diff <- host_country$gold_as_host - host_country$gold_b_host
host_country$cmedal_diff <- host_country$medal_as_host - host_country$medal_b_host

host_country$rbronze_diff <- host_country$rbronze_b_host - host_country$rbronze_as_host
host_country$rsilver_diff <- host_country$rsilver_b_host - host_country$rsilver_as_host
host_country$rgold_diff <- host_country$rgold_b_host - host_country$rgold_as_host
host_country$rmedal_diff <- host_country$rmedal_b_host - host_country$rmedal_as_host

##----Prepare dataframe for medal count and rank for each country-------------
# medal per country in each games
medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year) %>%
  summarise(cmedal = sum(!is.na(Medal)),
            cgold = sum(Medal == "Gold", na.rm = TRUE),
            csilver = sum(Medal == "Silver", na.rm = TRUE),
            cbronze = sum(Medal == "Bronze", na.rm = TRUE))

# rank by medal
medal_per_country <- medal_per_country %>%    # Applying group_by & summarise
  group_by(Year) %>%
  mutate(rmedal = ceiling(rank(-cmedal)),
         rgold = ceiling(rank(-cgold)),
         rsilver = ceiling(rank(-csilver)),
         rbronze = ceiling(rank(-cbronze)))

# Calculate the average medal count for each game
medal_avg_country <- medal_per_country %>%
  group_by(Year) %>%
  summarize(cmedal_avg = round(mean(cmedal), 2),
            cgold_avg = round(mean(cgold), 2),
            csilver_avg = round(mean(csilver), 2),
            cbronze_avg = round(mean(cbronze), 2),
            rmedal_avg = round(mean(rmedal), 2),
            rgold_avg = round(mean(rgold), 2),
            rsilver_avg = round(mean(rsilver), 2),
            rbronze_avg = round(mean(rbronze), 2))

host_country <- merge(x = host_country, y = medal_avg_country, by = "Year", all.x = TRUE)


#--------------------------------------------- Cari error kalo ngga ada kolom
filtered_data <- summer_olympic[!is.na(summer_olympic$Medal),]

# Calculate the count of medals by team
medal_counts <- filtered_data %>%
  group_by(ISO, Medal) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Medal, values_from = Count, values_fill = 0)

medal_counts <- medal_counts %>%
  mutate(Total_Medals = Bronze + Gold + Silver)


sport_data <- filtered_data[filtered_data$Sport %in% "Archery", ]

country_data <- sport_data[sport_data$NOC %in% "AUS", ]

# Calculate the count of medals by team
filter_counts <- country_data %>%
  group_by(ISO, Medal) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Medal, values_from = Count, values_fill = 0)

filter_counts <- filter_counts %>%
  mutate(
    Total_Medals = ifelse("Bronze" %in% names(.), Bronze, 0) + 
      ifelse("Gold" %in% names(.), Gold, 0) +
      ifelse("Silver" %in% names(.), Silver, 0)
  )
# ----------------------------------------

sport_data <- filtered_data[filtered_data$Sport %in% "Archery", ]

country_data <- sport_data[sport_data$NOC %in% "EUN", ]

filter_height <- country_data[!is.na(country_data$Height),]
height_per_noc <- aggregate(Height ~ ISO, data = filter_height, FUN = function(x) round(mean(x), 2))

if (nrow(filter_height) > 0) {
  height_per_noc <- aggregate(Height ~ ISO, data = filter_height, FUN = function(x) round(mean(x), 2))
} else {
  height_per_noc <- data.frame(ISO = character(), Height = numeric())
}

# ----------------------------------------------------------------------

age_bar <- data.frame(Age = 10:75)


male_hist <- hist(filtered_data$Age, breaks = 64, plot = FALSE)

filtered_data %>% 
  mutate(population = ifelse(Sex == "M", yes = population*(-1), no = population*1)) %>%
  mutate(abs_pop = abs(population)) %>%
  plot_ly(x= ~population, y=~age, color=~Sex) %>% 
  add_bars(orientation = 'h', hoverinfo = 'text', text = ~abs_pop) %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(tickmode = 'array', tickvals = c(-15000, -10000, -5000, 0, 5000, 10000, 15000),
                      ticktext = c('15000', '10000', '5000', '0', '5000', '10000', '15000')))


# Filter the data for male and female players
male_data <- subset(filtered_data, Sex == "M")
female_data <- subset(filtered_data, Sex == "F")

# Calculate the maximum frequency for setting the range of the y-axis
max_frequency <- max(hist(male_data$Age, plot = FALSE)$counts, hist(female_data$Age, plot = FALSE)$counts)

# Create the bar plots for male and female
plot_female <- plot_ly(female_data, y = ~Age, type = "histogram",
                       histfunc = "count", marker = list(color = "rgba(255, 0, 0, 0.7)"),
                       nbinsy = 15)%>%
  add_trace(male_data, y = ~Age, 
            histfunc = "sum", marker = list(color = "rgba(0, 0, 255, 0.7)"),
            nbinsy = 15) %>%
  layout(
    title = "Age Distribution of Players",
    xaxis = list(title = "Frequency", range = c(0, max_frequency)),
    yaxis = list(title = "Age"),
    bargap = 0.1, barmode = 'overlay',
    legend = list(x = 0.1, y = 0.9)
  )

plot_female
# Customize the layout (optional)

# Combine the bar plots and layout
plot_ly(plot_female, plot_male) 


plot_overlay <- plot_ly(alpha = 0.6)

plot_overlay <- plot_overlay %>% add_histogram(y = male_data$Age, histfunc = "count", nbinsy = 15)

plot_overlay <- plot_overlay %>% add_histogram(y = female_data$Age, histfunc = "count", nbinsy = 15)

plot_overlay <- plot_overlay %>% layout(barmode = "overlay", 
                                        xaxis = list(title = "Frequency", autorange = "reversed")
                                        )

plot_overlay

male_data <- subset(filtered_data, Sex == "M")
attr_data <- filtered_data[, c("Age", "Sex")]
colnames(attr_data)[colnames(attr_data) == "Age"] <- "Property"

temp_data <- filtered_data[filtered_data$Sex == "M", ]
temp_data <- temp_data[rowSums(!is.na(temp_data)) > 0, ]

female_attr_data <- subset(filtered_data, Sex == "F")
female_attr_data <- female_attr_data[, c("Age", "Sex")]
colnames(female_attr_data)[colnames(female_attr_data) == "Age"] <- "Property"

max_frequency <- max(hist(female_attr_data$Property, breaks = 20, plot = FALSE)$counts)

#-------------------------------------------------------------------------------
# Sample data
dataframe1 <- data.frame(
  NOC = c("ARG", "USA", "GBR", "BRA"),
  Gold = c(10, 21, 18, 9),
  Silver = c(24, 32, 33, 12)
)

# Check if Gold, Silver, and Bronze columns are present
if (!"Gold"%in% colnames(dataframe1)) {
  dataframe1$Gold <- 0
}
if (!"Silver"%in% colnames(dataframe1)) {
  dataframe1$Silver <- 0
}
if (!"Bronze"%in% colnames(dataframe1)) {
  dataframe1$Bronze <- 0
}
















