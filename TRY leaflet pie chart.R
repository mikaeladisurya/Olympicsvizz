# https://cran.r-project.org/web/packages/leaflet.minicharts/vignettes/introduction.html
library(leaflet)
# Load the leaflet.minicharts package
library(leaflet.minicharts)

tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

basemap <- leaflet(width = "100%", height = "400px") %>%
  addTiles(tilesURL)

colors <- c("#4fc13c", "#cccccc")

basemap %>%
  addMinicharts(
    prod2016$lng, prod2016$lat,
    type = "pie",
    chartdata = prod2016[, c("renewable", "non_renewable")], 
    colorPalette = colors, 
    width = 60 * sqrt(prod2016$total) / sqrt(max(prod2016$total)), transitionTime = 0
  )