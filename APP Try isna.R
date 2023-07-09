# ----Combining from the References:---------------------
# Lecture Notes
# https://stackoverflow.com/questions/54816608/customizing-bin-widths-in-plotlys-histogram-function-in-r
# https://plotly.com/r/histograms/
# https://rpubs.com/juanhklopper/control_histogram_color_using_Plotly_for_R

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)

# Create a color palette with handmade bins.
library(RColorBrewer)
# Read shape polygon file 
library(rgdal)

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
summer_olympic_merge <-  merge(x = country_code, y = summer_olympic, by = "NOC", all = FALSE)

sum(is.na(summer_olympic$NOC))
sum(is.na(summer_olympic_merge$NOC))

filter_na <- summer_olympic[!is.na(summer_olympic$Height),]
filter_com <- summer_olympic[complete.cases(summer_olympic$Height),]

filter_na_merge <- summer_olympic_merge[!is.na(summer_olympic_merge$Height),]
filter_com_merge <- summer_olympic_merge[complete.cases(summer_olympic_merge$Height),]

# UI code
ui <- fluidPage(
  titlePanel("Choropleth Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose Variable:", 
                  choices = c("Age", "Weight", "Height", "BMI"),
                  selected = "Age"),
      selectInput("medal", "Choose Medal Type:",
                  choices = c("Bronze", "Silver", "Gold", "All Medals"),
                  selected = "All Medals"),
      selectInput("gender", "Choose Gender:",
                  choices = c("F", "M", "All"),
                  selected = "All"),
      selectizeInput("region", "Select Region",
                     choices = unique(summer_olympic$NOC),
                     multiple = TRUE,options = list(plugins = list('remove_button')))
    ),
    mainPanel(
      # Table output for filtered data
      tableOutput("filteredTable"),
      tableOutput("nofilteredTable")
    )
  )
)

# Server code
server <- function(input, output) {
  
  output$filteredData <- renderTable({
    # Exclude rows with NA values in the specified column
    filter_height <- summer_olympic[!is.na(summer_olympic$Height),]
    
    # Return the filtered dataframe
    filter_height
  })
  
  output$nofilteredData <- renderTable({
    # Exclude rows with NA values in the specified column
    summer_olympic
  })
  
}

# Run the app
shinyApp(ui, server)