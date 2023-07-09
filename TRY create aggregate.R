# Clean the data object
library(dplyr)
library(tidyverse)

# Loading athlete data
summer_olympic <- read.csv('/DATA/summer_olympics.csv')

# Loading noc_region data
noc_regions <- read.csv('/DATA/noc_regions.csv')

glimpse(summer_olympic)
glimpse(noc_regions)

# join athlete data with noc_regions to add region
summer_olympic <-  merge(x = summer_olympic, y = noc_regions, by = "NOC", all = TRUE)

# aggregate based on NOC
count_per_noc <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC) %>%
  summarise(count = n_distinct(ID))
count_per_noc

# filter distinct name per year
