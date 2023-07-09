# Clean the data object
library(dplyr)
library(tidyverse)

library(leaflet)
# Create a color palette with handmade bins.
library(RColorBrewer)
# Read shape file with the rgdal library. 
library(rgdal)

# world spatial polygon data frame
world_spdf <- readOGR(paste0(getwd(),"\\data\\world_shape_file\\"), 
                      "TM_WORLD_BORDERS_SIMPL-0.3",
                      verbose=FALSE
)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))
# Calculate BMI
summer_olympic$BMI <- summer_olympic$Weight / ((summer_olympic$Height / 100) ^ 2)

# Loading noc_region data
# noc_regions <- read.csv(paste0(getwd(),"/DATA/noc_regions.csv"))

# Loading alphabetic country codes data
country_code <- read.csv(paste0(getwd(),"/DATA/alphabetic_country_codes.csv"))

# glimpse(summer_olympic)
# glimpse(noc_regions)
# glimpse(country_code)

# change NOC of Singapore
# summer_olympic$NOC[summer_olympic$NOC == "SGP"] <- "SIN"

# join athlete data with noc_regions to add region
# summer_olympic <-  merge(x = summer_olympic, y = noc_regions, by = "NOC", all = TRUE)

# join athlete data with country codes to add iso3 country code
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all = FALSE)

# unique(summer_olympic$region)
# distinct_noc <- summer_olympic[is.na(summer_olympic$region),] %>%    # Applying group_by & summarise
#   group_by(NOC, Team) %>%
#   summarise(count = n_distinct(ID))
# View(distinct_noc)
min(world_spdf@data$count)
# count_per_noc$region
# world_spdf@data$NAME
# write.csv(count_per_noc, "EXPORT\\count_per_noc.csv", row.names=FALSE)
# write.csv(world_spdf@data, "EXPORT\\world_spdf_data.csv", row.names=FALSE)

# aggregate based on NOC
count_per_noc <- summer_olympic %>%    # Applying group_by & summarise
  group_by(ISO) %>%
  summarise(count = n_distinct(ID))
# count_per_noc <-  merge(x = count_per_noc, y = noc_regions, by = "NOC", all = TRUE)

# hist count_per_noc
# hist(count_per_noc$count)
# 
# barplot(count_per_noc$count, main = "Country Representative", names.arg = count_per_noc$ISO, xlab = "Country", ylab = "revenue", col = "orange")

# try putting count data to spatial
world_spdf@data = data.frame(world_spdf@data, count_per_noc[match(world_spdf@data[["ISO3"]], count_per_noc[["ISO"]]),])

# #----Start plotting for Choropleth-------------------
mybins <- c(NA, 0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$count, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "Area: ", world_spdf@data$AREA, "<br/>", 
  "Count: ", world_spdf@data$count, 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(count), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addCircleMarkers(~LON, ~LAT,
                   fillColor = blues9, fillOpacity = 0.7, color="white", radius=~POP2005, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Population (M)", position = "bottomleft" )

View(world_spdf@data)


# filter_height <- summer_olympic[, "Height"]
# filter_height_weight <- summer_olympic[, c("Height", "Weight")]
# 
# gold_df <- na.omit(summer_olympic[summer_olympic$Medal == "Gold", ])
# filter_height_gold <- gold_df[, "Height"]

filter_age <- summer_olympic[!is.na(summer_olympic$Age),]
avg_age <- aggregate(Age ~ ISO, data = filter_age, FUN = mean)
# Calculate the count of medals by team
medal_counts <- aggregate(Medal ~ ISO, data = filter_age, FUN = function(x) table(factor(x, levels = c("Gold", "Silver", "Bronze"))))

# Merge the average age and medal counts dataframes by team
count_per_noc <- merge(avg_age, medal_counts, by = "ISO")
summary(count_per_noc$Age)

world_spdf@data = data.frame(world_spdf@data, avg_age[match(world_spdf@data[["ISO3"]], avg_age[["ISO"]]),])
world_spdf@data = data.frame(world_spdf@data, medal_counts[match(world_spdf@data[["ISO3"]], medal_counts[["ISO"]]),])

# filter_height <- summer_olympic[!is.na(summer_olympic$Height),]
# count_per_noc <- aggregate(Height ~ ISO, data = filter_height, FUN = mean)
# summary(count_per_noc$Height)
# 
# filter_weight <- summer_olympic[!is.na(summer_olympic$Weight),]
# count_per_noc <- aggregate(Weight ~ ISO, data = filter_weight, FUN = mean)
# summary(count_per_noc$Weight)
# 
# filter_bmi <- summer_olympic[!is.na(summer_olympic$BMI),]
# count_per_noc <- aggregate(BMI ~ ISO, data = filter_bmi, FUN = mean)
# summary(count_per_noc$BMI)

# Calculate the count of medals by team
medal_counts_try <- filter_age %>%
  group_by(ISO, Medal) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Medal, values_from = Count, values_fill = 0)

