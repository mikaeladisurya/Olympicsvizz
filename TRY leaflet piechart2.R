# https://www.supplychaindataanalytics.com/map-based-charting-in-r-leaflet-minicharts/
library(knitr)
library(leaflet)
library(leaflet.minicharts)
library(magrittr)
library(dplyr)

basemap = leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>%
  setView(-98,38.5,zoom=2)

basemap

minitest_data = read.csv("DATA/sales_minichart.csv",header=TRUE)
head(data)

colors = c("#FF0000", "#428EF4")

palete <- colors[1]

basemap %>%
  addMinicharts(
    data$longitude, data$latitude,
    chartdata = select(data,sales_sensors,sales_pumps),
    colorPalette = colors,
    width = 45, height = 45
  )


basemap %>%
  addMinicharts(
    minitest_data$longitude, minitest_data$latitude,
    type = "pie",
    chartdata = select(minitest_data,sales_sensors,sales_pumps), 
    fillColor = colors,
    colorPalette = colors, 
    width = 60 * sqrt(minitest_data$sales_total) / sqrt(max(minitest_data$sales_total)), transitionTime = 0
  )
