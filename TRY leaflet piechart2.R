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

# Create a sample dataset
data <- rnorm(100)

# Create the Plotly histogram
plot <- plot_ly(x = ~data, type = "histogram", text = ~paste("Min bin: ", lower.bin.edge, "</br> Max bin: ", upper.bin.edge, "</br> Count: ", count)) %>%
  layout(title = "Histogram with Custom Popup",
         xaxis = list(title = "Value"),
         yaxis = list(title = "Frequency"),
         hovermode = "closest")

# Display the plot
plot

# Create a sample dataset
data <- rnorm(100)

# Calculate the histogram manually to obtain the bin edges
hist_data <- hist(data, plot = FALSE)
bin_edges <- hist_data$breaks

# Create the Plotly histogram with custom popup text
plot <- plot_ly(x = ~data, type = "histogram",
                hovertemplate = ~paste("Min bin: ", bin_edges[.data$bin.x], "</br> Max bin: ", bin_edges[.data$bin.x + 1], "</br> Count: ", .data$count)) %>%
  layout(title = "Histogram with Custom Popup",
         xaxis = list(title = "Value"),
         yaxis = list(title = "Frequency"),
         hovermode = "closest")

# Display the plot
plot


# Create a sample dataset
data <- rnorm(100)

# Set the number of bins for the histogram
num_bins <- 10

# Calculate the bin indices using cut()
bin_indices <- cut(data, breaks = num_bins, labels = FALSE)

# Create the Plotly histogram with custom popup text
plot <- plot_ly(x = ~data, type = "histogram",
                hovertemplate = ~paste("Min bin: ", min(data[bin_indices == .data$x]), "</br> Max bin: ", max(data[bin_indices == .data$x]), "</br> Count: ", sum(bin_indices == .data$x))) %>%
  layout(title = "Histogram with Custom Popup",
         xaxis = list(title = "Value"),
         yaxis = list(title = "Frequency"),
         hovermode = "closest")

# Display the plot
plot
