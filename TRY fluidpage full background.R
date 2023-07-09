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





ui <- fluidPage(
  fluidRow(class = "myRow1", 
           column(6,div(style = "height:200px;background-color: yellow;", "Topleft")),
           column(6,div(style = "height:100px;background-color: blue;", "Topright"))),
  fluidRow(class = "myRow2",
           column(6,div(style = "height:100px;background-color: green;", "Bottomleft")),
           column(6,div(style = "height:150px;background-color: red;", "Bottomright")))
  , tags$head(tags$style("
      .myRow1{height:250px;}
      .myRow2{height:350px;background-color: pink;}"
  )
  )
)


# Server function
server <- function(input, output) {
  # Create the Sales by Region plot
  
}

# Run the Shiny application 
shinyApp(ui = ui, server = server)
