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
library(sp)

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
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all = TRUE)

# aggregate based on NOC
count_per_noc <- summer_olympic %>%    # Applying group_by & summarise
  group_by(ISO) %>%
  summarise(count = n_distinct(ID))

# try putting count data to spatial
world_spdf@data = data.frame(world_spdf@data, count_per_noc[match(world_spdf@data[["ISO3"]], count_per_noc[["ISO"]]),])

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
      # plotlyOutput("histogram44")
      leafletOutput("map")
    )
  )
)

# Server code
server <- function(input, output) {
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    # #----Start plotting for Choropleth-------------------
    mybins <- c(NA, 0, 500, 1000, 1500, 2000, 3000, 4000, Inf)
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
      # addCircleMarkers(~LON, ~LAT, 
      #                  fillColor = blues9, fillOpacity = 0.7, color="white", radius=~POP2005/10, stroke=FALSE,
      #                  label = mytext,
      #                  labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      # ) %>%
      addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Population (M)", position = "bottomleft" )
  })
  
}

# Run the app
shinyApp(ui, server)