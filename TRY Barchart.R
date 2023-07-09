# Clean the data object
library(dplyr)
library(tidyverse)

library(ggplot2)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))
# Loading alphabetic country codes data
country_code <- read.csv(paste0(getwd(),"/DATA/alphabetic_country_codes.csv"))
# join athlete data with country codes to add iso3 country code
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all.x = TRUE)

first_20_rows <- head(summer_olympic, 200)

filtered_data <- first_20_rows[first_20_rows$Medal == "Bronze", ]
filtered_data <- filtered_data[rowSums(!is.na(filtered_data)) > 0, ]

# Calculate the count of medals by team
medal_counts <- filtered_data %>%
  group_by(ISO, Medal) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Medal, values_from = Count, values_fill = 0)
medal_counts <- medal_counts %>%
  mutate(Total_Medals = Bronze + Gold + Silver)


filtered_isdata <- first_20_rows[!is.na(first_20_rows$Medal),]
class(filtered_isdata)
filtered_isdata <- filtered_isdata[filtered_isdata$Medal == "Bronze", ]
class(filtered_isdata)

filter_age <- filtered_isdata[!is.na(filtered_isdata$Age),]

#-- check data
unique(summer_olympic$Sex)
summary(summer_olympic$Age)
summary(summer_olympic$Height)
summary(summer_olympic$Weight)
unique(summer_olympic$Year)
unique(summer_olympic$Medal)

selected = c(min(summer_olympic$Year),max(summer_olympic$Year))

young_olympiad <- na.omit(summer_olympic[summer_olympic$Age<15, ])

# Calculate BMI
summer_olympic$BMI <- summer_olympic$Weight / ((summer_olympic$Height / 100) ^ 2)

first_20_rows <- head(summer_olympic, 20)
filter_height_20 <- first_20_rows[!is.na(first_20_rows$Height),]
omit_bronze_20 <- first_20_rows[first_20_rows$Medal == 'Bronze', ]
omit_bronze_20 <- omit_bronze_20[complete.cases(omit_bronze_20), ]
omit_bronze_20 <- omit_bronze_20[rowSums(!is.na(omit_bronze_20)) > 0, ]

filtered_data <- na.omit(summer_olympic[summer_olympic$Medal == 'Bronze', ])

filter_height <- summer_olympic[!is.na(summer_olympic$Height),]
height_per_noc <- aggregate(Height ~ NOC, data = filter_height, FUN = function(x) round(mean(x), 2))

filter_height <- summer_olympic[, "Height"]
filter_height_weight <- summer_olympic[, c("Height", "Weight")]

gold_df <- na.omit(summer_olympic[summer_olympic$Medal == "Gold", ])
filter_height_gold <- gold_df[, "Height"]

filter_height <- na.omit(summer_olympic[summer_olympic$Height, ])

# aggregate based on NOC
count_per_noc <- filter_height %>%    # Applying group_by & summarise
  group_by(ISO) %>%
  summarise(count = n_distinct(ID))

# Loading noc_region data
# noc_regions <- read.csv(paste0(getwd(),"/DATA/noc_regions.csv"))

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
write.csv(medal_per_country, "EXPORT\\medal_per_country.csv", row.names=FALSE)

# count based on NOC
count_per_noc <- summer_olympic %>%    # Applying group_by & summarise
  group_by(ISO) %>%
  summarise(count = n_distinct(ID))
# count_per_noc <-  merge(x = count_per_noc, y = noc_regions, by = "NOC", all = TRUE)

# hist count_per_noc
hist(count_per_noc$count)

barplot(count_per_noc$count, main = "Country Representative", names.arg = count_per_noc$ISO, xlab = "Country", ylab = "revenue", col = "salmon")


medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC) %>%
  summarise(gold_count = n_distinct(NOC,))

medal_per_country <- data.frame(table(summer_olympic$ISO, summer_olympic$Medal))
colnames(medal_per_country) <- c("Team", "Medal", "Count")

medal_type <- unique(medal_per_country$Medal)
# Stacked
# https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
ggplot(medal_per_country, aes(x=medal_per_country$Team, y=medal_per_country$Count, fill=medal_per_country$Medal)) + 
  geom_bar(position="stack", stat="identity")


cek <- "Gold"
cek_color <- "azure3"

cek_color <- case_when(
  cek == "Gold" ~ "darkgoldenrod2",
  cek == "Silver" ~ "azure3",
  cek == "Bronze" ~ "chocolate",
  TRUE ~ "deepskyblue2"
)

# pilih 1 kolom
noc_regions <- summer_olympic$NOC[!duplicated(summer_olympic$NOC)]
noc_unik <- unique(summer_olympic$NOC)

medal_pick <- 'Total_Medals'

df <- data.frame(
  Group = c("A", "A", "C", "B", "C", "A", "B", "B", "C", "C"),
  Record = c(100, 99, 98, 111, 101, 93, 100, 121, 88, 97)
)

df <- df %>%
  group_by(Group) %>%
  mutate(Rank_by_group = rank(-Record))





