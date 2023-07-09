# https://appsilon.com/shiny-application-layouts/
# https://sscc.wisc.edu/shiny/users/jstruck2/layouts/
# https://shiny.posit.co/r/reference/shiny/1.7.2/fillpage

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(leaflet)

# Create a color palette with handmade bins.
library(RColorBrewer)
# Read shape polygon file 
library(rgdal)





ui <- fillPage(
  tags$style(type = "text/css",
             ".half-fill { width: 50%; height: 100%; }",
             "#one { float: left; background-color: #ddddff; }",
             "#two { float: right; background-color: #ccffcc; }"
  ),
  div(id = "one", class = "half-fill",
      "Left half",
      
      headerPanel("GOLDEN LEGACIES"),
      h2("Unveiling the Secret of Summer Olympic Greatness")
  ),
  div(id = "two", class = "half-fill",
      "Right half"
  ),
  padding = 10
)


# Server function
server <- function(input, output) {
  # Create the Sales by Region plot
  
}

# Run the Shiny application 
shinyApp(ui = ui, server = server)
