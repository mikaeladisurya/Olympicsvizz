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