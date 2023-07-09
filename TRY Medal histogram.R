# Clean the data object
library(dplyr)
library(tidyverse)

library(ggplot2)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Loading noc_region data
noc_regions <- read.csv(paste0(getwd(),"/DATA/noc_regions.csv"))

# Loading alphabetic country codes data
country_code <- read.csv(paste0(getwd(),"/DATA/alphabetic_country_codes.csv"))

glimpse(summer_olympic)
glimpse(noc_regions)
glimpse(country_code)

# change NOC of Singapore (in noc_region SIN)
# summer_olympic$NOC[summer_olympic$NOC == "SGP"] <- "SIN"

# join athlete data with noc_regions to add region
# summer_olympic <-  merge(x = summer_olympic, y = noc_regions, by = "NOC", all = TRUE)

# join athlete data with country codes to add iso3 country code
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all = FALSE) # country_code data just the one that intersect

# unique(summer_olympic$ISO)
# View(medal_per_country)
# distinct_noc <- summer_olympic[is.na(summer_olympic$region),] %>%    # Applying group_by & summarise
#   group_by(NOC, Team) %>%
#   summarise(count = n_distinct(ID))
# View(distinct_noc)
# min(world_spdf@data$count)
# count_per_noc$region
# world_spdf@data$NAME
# write.csv(count_per_noc, "EXPORT\\count_per_noc.csv", row.names=FALSE)
# write.csv(medal_per_country, "EXPORT\\medal_per_country.csv", row.names=FALSE)

# count based on NOC
count_per_noc <- summer_olympic %>%    # Applying group_by & summarise
  group_by(ISO) %>%
  summarise(count = n_distinct(ID))
# count_per_noc <-  merge(x = count_per_noc, y = noc_regions, by = "NOC", all = TRUE)

# hist count_per_noc
hist(count_per_noc$count)

# Filter the data for bronze medals
bronze_data <- na.omit(subset(summer_olympic, Medal == "Bronze"))
# Calculate BMI
bronze_data$BMI <- bronze_data$Weight / ((bronze_data$Height / 100) ^ 2)

# the histogram bronze by age
ggplot(bronze_data, aes(x = Age)) +
  geom_histogram(binwidth = 1, breaks = seq(min(bronze_data$Age), max(bronze_data$Age), length.out = 11), fill = "steelblue", color = "white") +
  labs(x = "Age", y = "Count", title = "Histogram of Bronze Medals by Age") +
  theme_minimal()

# the histogram bronze by height
ggplot(bronze_data, aes(x = Height)) +
  geom_histogram(binwidth = 1, breaks = seq(min(bronze_data$Height), max(bronze_data$Height), length.out = 21), fill = "salmon", color = "white") +
  labs(x = "Height", y = "Count", title = "Histogram of Bronze Medals by Height") +
  theme_minimal()

# the histogram bronze by weight
ggplot(bronze_data, aes(x = Weight)) +
  geom_histogram(binwidth = 1, breaks = seq(min(bronze_data$Weight), max(bronze_data$Weight), length.out = 21), fill = "salmon", color = "white") +
  labs(x = "Weight", y = "Count", title = "Histogram of Bronze Medals by Weight") +
  theme_minimal()

# the histogram bronze by BMI
ggplot(bronze_data, aes(x = BMI)) +
  geom_histogram(binwidth = 1, breaks = seq(min(bronze_data$BMI), max(bronze_data$BMI), length.out = 21), fill = "salmon", color = "white") +
  labs(x = "BMI", y = "Count", title = "Histogram of Bronze Medals by BMI") +
  theme_minimal()

gold_data <- na.omit(subset(summer_olympic, Medal == "Gold"))
silver_data <- na.omit(subset(summer_olympic, Medal == "Silver"))

gns <- rbind(gold_data, silver_data)
all_medal <- rbind(gns, bronze_data)



