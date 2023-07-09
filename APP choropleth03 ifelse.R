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
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all = FALSE)

# #----Preparing world spdf for filtering-----------------
# aggregate based on NOC
filter_age <- summer_olympic[!is.na(summer_olympic$Age),]
age_per_noc <- aggregate(Age ~ ISO, data = filter_age, FUN = mean)

filter_height <- summer_olympic[!is.na(summer_olympic$Height),]
height_per_noc <- aggregate(Height ~ ISO, data = filter_height, FUN = mean)

filter_weight <- summer_olympic[!is.na(summer_olympic$Weight),]
weight_per_noc <- aggregate(Weight ~ ISO, data = filter_weight, FUN = mean)

filter_bmi <- summer_olympic[!is.na(summer_olympic$BMI),]
bmi_per_noc <- aggregate(BMI ~ ISO, data = filter_bmi, FUN = mean)

# try putting count data to spatial
world_spdf@data = data.frame(world_spdf@data, age_per_noc[match(world_spdf@data[["ISO3"]], age_per_noc[["ISO"]]),])
world_spdf@data = data.frame(world_spdf@data, height_per_noc[match(world_spdf@data[["ISO3"]], height_per_noc[["ISO"]]),])
world_spdf@data = data.frame(world_spdf@data, weight_per_noc[match(world_spdf@data[["ISO3"]], weight_per_noc[["ISO"]]),])
world_spdf@data = data.frame(world_spdf@data, bmi_per_noc[match(world_spdf@data[["ISO3"]], bmi_per_noc[["ISO"]]),])

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
    if (input$variable == "Age") {
      mybins <- c(NA, 18, 20, 22, 24, 26, 28, 30, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$Age, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "Country: ", world_spdf@data$NAME,"<br/>", 
        "Area: ", world_spdf@data$AREA, "<br/>", 
        "Count: ", world_spdf@data$Age, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      # Final Map
      leaflet(world_spdf) %>% 
        addTiles()  %>% 
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          fillColor = ~mypalette(Age), 
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
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Age (Years)", position = "bottomleft" )
    } else if (input$variable == "Height") {
      mybins <- c(NA, 160, 165, 170, 175, 180, 185, 190, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$Height, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "Country: ", world_spdf@data$NAME,"<br/>", 
        "Area: ", world_spdf@data$AREA, "<br/>", 
        "Count: ", world_spdf@data$Height, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      # Final Map
      leaflet(world_spdf) %>% 
        addTiles()  %>% 
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          fillColor = ~mypalette(Height), 
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
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Height (cm)", position = "bottomleft" )
    } else if (input$variable == "Weight") {
      mybins <- c(NA, 55, 62, 69, 76, 83, 90, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$Weight, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "Country: ", world_spdf@data$NAME,"<br/>", 
        "Area: ", world_spdf@data$AREA, "<br/>", 
        "Count: ", world_spdf@data$Weight, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      # Final Map
      leaflet(world_spdf) %>% 
        addTiles()  %>% 
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          fillColor = ~mypalette(Weight), 
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
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Weight (Kg)", position = "bottomleft" )
    } else if (input$variable == "BMI") {
      mybins <- c(NA, 20, 21, 22, 23, 24, 25, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$BMI, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "Country: ", world_spdf@data$NAME,"<br/>", 
        "Area: ", world_spdf@data$AREA, "<br/>", 
        "Count: ", world_spdf@data$BMI, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      # Final Map
      leaflet(world_spdf) %>% 
        addTiles()  %>% 
        setView( lat=10, lng=0 , zoom=2) %>%
        addPolygons( 
          fillColor = ~mypalette(BMI), 
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
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "BMI (Kg/M2)", position = "bottomleft" )
    }
    
  })
  
}

# Run the app
shinyApp(ui, server)