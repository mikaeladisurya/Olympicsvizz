# https://appsilon.com/shiny-application-layouts/
# https://sscc.wisc.edu/shiny/users/jstruck2/layouts/
# https://shiny.posit.co/r/reference/shiny/1.7.2/fillpage
# https://www.codingprof.com/how-to-create-a-slider-in-r-shiny/
# https://shiny.posit.co/r/reference/shiny/0.11/reactivevalues

# Install necessary libraries
if(!require(shiny)) install.packages ("shiny")
if(!require(shinydashboard)) install.packages ("shinydashboard")
if(!require(shinythemes)) install.packages ("shinythemes")
if(!require(shinyWidgets)) install.packages ("shinyWidgets")
if(!require(ggplot2)) install.packages ("ggplot2")
if(!require(plotly)) install.packages ("plotly")
if(!require(dplyr)) install.packages ("dplyr")
if(!require(tidyverse)) install.packages ("tidyverse")
if(!require(leaflet)) install.packages ("leaflet")
if(!require(leaflet)) install.packages ("RColorBrewer")

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
library(RColorBrewer)

# Create a color palette with handmade bins.
library(RColorBrewer)
# Read shape polygon file 
library(rgdal)

##----Prepare dataframe-------------------------------------------------------
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
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all.x = TRUE)

##----Create UI---------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  headerPanel(h1("GOLDEN LEGACIES", align = "center")),
  h2("Unveiling the Secret of Summer Olympic Greatness"),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Athlete Characteristic",
      fluidRow(
        column(width = 4,
               wellPanel(
                 selectInput(
                   "variable", 
                   "Choose Attribute:", 
                   choices = c("Age", "Weight", "Height", "BMI"),
                   selected = "Age"),
                 selectInput(
                   "medal", 
                   "Choose Medal Type:",
                   choices = c("Bronze", "Silver", "Gold", "All Medals"),
                   selected = "All Medals"),
                 selectInput(
                   "gender", "Choose Gender:",
                   choices = c("F", "M", "All"),
                   selected = "All"),
                 selectizeInput(
                   "region", 
                   "Select Region",
                   choices = unique(summer_olympic$NOC),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'All Country',
                     plugins = list('remove_button'))
                   )
               ),# /wellPanel
               wellPanel(
                 titlePanel(
                   h2("WHO DOING BETTER?", align = "center")
                 ),
                 h4(em("Click on the x-axis to filter"), align = "center"),
                 plotlyOutput("histogram_medal")
               )# /wellPanel
        ), # fluidrow4
        column(width = 8,
               wellPanel(
                 leafletOutput("map"),
                 titlePanel(
                   h2("REPRESENTATIVE", align = "center")
                 ),
                 h4(em("Click on the country to filter"), align = "center")
               ),# /wellPanel
               wellPanel(
                 h2("Page description:"),
                 HTML("<ul>
                      <li>This page aims to show the density of accidents happened in the greater Victoria region.</li>
                      <li>You can see Melbourne is the highest crashes happened area. The other suburb around Melbourne has lower crashes happened, and the farther from Melbourne, the less crash happened in Victoria.</li>
                      <li>If you selected 2020, there were less number of accidents shows on the map. this may because of the limited data collection during this year with the Covid-19 impacting</li>
                      <li>If you selected 2020, there were less number of accidents shows on the map. this may because of the limited data collection during this year with the Covid-19 impacting</li>
                    </ul>
                    "
                 )
               )# /wellPanel
        ) # col-8
      ) # fluidrow
    ), #tabpanel1
    tabPanel(
      "Host Countries",
      fluidRow(
        sidebarPanel(
          #Sidebar panel
          h3("Injury Level"),
          selectInput("fatality_level","Select Injury",
                      choices = c("Level 1 - Fatality", "Level 2 - Serious injury", "Level 3 - Other injury", "Level 4 - Not injured")),
          p("*Put your cursor on each bubble to see the annotations"),
          br(),
          h2("Page description:"),
          HTML("<ul>
                      <li>This page aims to find how the light condition affect the crashes' injury level</li>
                      <li>When we look at the level 1 - fatality injury level crashes, the percentage of 'Dark with no street light' is the highest</li>
                      <li>When we look at the level 2 - seriouse injury level crashes, the percentage of 'Dark with no street light' is still the highest</li>
                      <li>When we look at the level 3 - other injury level and level 4- No injury crashes, all kinds of light condition have a relativly average percentage value</li>
                      <li>This evidence that the percentage of serious injury crashes is affected by the visual conditions for drivers. The darker the driving environment, the more likely the driver is involved in serious injury crashes.</li>
                    </ul>
                    ")
        ),
        mainPanel(
          h2("This is Tab 2")
        )
      )   
    ) # tab panel
  ) # tabset panel
)


# Server function
server <- function(input, output) {
  
  # medal_pick <- reactive({ input$medal })
  
  output$map <- renderLeaflet({
    
    # #----Preparing world spdf for filtering-----------------
    if (input$medal == "All Medals") {
      medal_pick='Total_Medals'
      filtered_data <- summer_olympic[!is.na(summer_olympic$Medal),]
    } else {
      medal_pick=input$medal
      filtered_data <- summer_olympic[summer_olympic$Medal == input$medal, ]
      filtered_data <- filtered_data[rowSums(!is.na(filtered_data)) > 0, ]
    }
    
    if (input$gender != "All") {
      filtered_data <- filtered_data[filtered_data$Sex == input$gender, ]
    }
    
    # if region is null, compute 
    if (is.null(input$region)){
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data[filtered_data$NOC %in% input$region, ]
    }
    
    # https://statisticsglobe.com/mean-by-group-in-r
    # filter age data from na value, then create dataframe for each noc age mean rounded 2 decimal
    filter_age <- filtered_data[!is.na(filtered_data$Age),]
    age_per_noc <- aggregate(Age ~ ISO, data = filter_age, FUN = function(x) round(mean(x), 2))
    
    # filter height data from na value, then create dataframe for each noc height mean rounded 2 decimal
    filter_height <- filtered_data[!is.na(filtered_data$Height),]
    height_per_noc <- aggregate(Height ~ ISO, data = filter_height, FUN = function(x) round(mean(x), 2))
    
    # filter weight data from na value, then create dataframe for each noc weight mean rounded 2 decimal
    filter_weight <- filtered_data[!is.na(filtered_data$Weight),]
    weight_per_noc <- aggregate(Weight ~ ISO, data = filter_weight, FUN = function(x) round(mean(x), 2))
    
    # filter bmi data from na value, then create dataframe for each noc bmi mean rounded 2 decimal
    filter_bmi <- filtered_data[!is.na(filtered_data$BMI),]
    bmi_per_noc <- aggregate(BMI ~ ISO, data = filter_bmi, FUN = function(x) round(mean(x), 2))
    
    # Calculate the count of medals by team
    medal_counts <- filtered_data %>%
      group_by(ISO, Medal) %>%
      summarize(Count = n()) %>%
      pivot_wider(names_from = Medal, values_from = Count, values_fill = 0)
    if (input$medal == "All Medals") {
      # Add a new column for total medal count
      medal_counts <- medal_counts %>%
        mutate(Total_Medals = Bronze + Gold + Silver)
    }
    
    # try putting count data to spatial
    world_spdf@data = data.frame(world_spdf@data, age_per_noc[match(world_spdf@data[["ISO3"]], age_per_noc[["ISO"]]),])
    world_spdf@data = data.frame(world_spdf@data, height_per_noc[match(world_spdf@data[["ISO3"]], height_per_noc[["ISO"]]),])
    world_spdf@data = data.frame(world_spdf@data, weight_per_noc[match(world_spdf@data[["ISO3"]], weight_per_noc[["ISO"]]),])
    world_spdf@data = data.frame(world_spdf@data, bmi_per_noc[match(world_spdf@data[["ISO3"]], bmi_per_noc[["ISO"]]),])
    world_spdf@data = data.frame(world_spdf@data, medal_counts[match(world_spdf@data[["ISO3"]], medal_counts[["ISO"]]),])
    
    bin_color <- case_when(
      input$medal == "Gold" ~ "darkgoldenrod",
      input$medal == "Silver" ~ "darkgray",
      input$medal == "Bronze" ~ "chocolate",
      TRUE ~ "darkturquoise"
    )
    
    # #----Start plotting for Choropleth-------------------
    if (input$variable == "Age") {
      mybins <- c(NA, 18, 20, 22, 24, 26, 28, 30, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$Age, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "<b>", world_spdf@data$NAME,"</b><br/>", 
        "Avg Age: ", world_spdf@data$Age, "<br/>", 
        input$medal ," Count: ", world_spdf@data[[medal_pick]], 
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
        addCircleMarkers(~LON, ~LAT,
                         fillColor = bin_color, fillOpacity = 0.7, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Age (Years)", position = "bottomleft" )
    } else if (input$variable == "Height") {
      mybins <- c(NA, 160, 165, 170, 175, 180, 185, 190, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$Height, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "<b>", world_spdf@data$NAME,"</b><br/>", 
        "Avg Height: ", world_spdf@data$Height, "<br/>", 
        input$medal ," Count: ", world_spdf@data[[medal_pick]], 
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
        addCircleMarkers(~LON, ~LAT,
                         fillColor = bin_color, fillOpacity = 0.7, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Height (cm)", position = "bottomleft" )
    } else if (input$variable == "Weight") {
      mybins <- c(NA, 55, 62, 69, 76, 83, 90, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$Weight, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "<b>", world_spdf@data$NAME,"</b><br/>", 
        "Avg Weight: ", world_spdf@data$Weight, "<br/>", 
        input$medal ," Count: ", world_spdf@data[[medal_pick]], 
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
        addCircleMarkers(~LON, ~LAT,
                         fillColor = bin_color, fillOpacity = 0.7, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Weight (Kg)", position = "bottomleft" )
    } else if (input$variable == "BMI") {
      mybins <- c(NA, 20, 21, 22, 23, 24, 25, Inf)
      mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$BMI, na.color="transparent", bins=mybins)
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "<b>", world_spdf@data$NAME,"</b><br/>", 
        "Avg BMI: ", world_spdf@data$BMI, "<br/>", 
        input$medal ," Count: ", world_spdf@data[[medal_pick]], 
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
        addCircleMarkers(~LON, ~LAT,
                         fillColor = bin_color, fillOpacity = 0.7, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "BMI (Kg/M2)", position = "bottomleft" )
    }
    
  })# output renderleaflet choropleth
  
  output$histogram_medal <- renderPlotly({
    
    if (input$medal == "All Medals") {
      filtered_data <- summer_olympic[!is.na(summer_olympic$Medal),]
    } else {
      filtered_data <- na.omit(summer_olympic[summer_olympic$Medal == input$medal, ])
    }
    
    if (input$gender != "All") {
      filtered_data <- na.omit(filtered_data[filtered_data$Sex == input$gender, ])
    }
    
    # if region is null, compute 
    if (is.null(input$region)){
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data[filtered_data$NOC %in% input$region, ]
    }
    
    bin_color <- case_when(
      input$medal == "Gold" ~ "darkgoldenrod",
      input$medal == "Silver" ~ "darkgray",
      input$medal == "Bronze" ~ "chocolate",
      TRUE ~ "darkturquoise"
    )
    
    plot_ly(data = filtered_data, x = ~filtered_data[[input$variable]], 
            type = "histogram", nbinsx = 30, 
            marker = list(color = bin_color, line = list(color = "cadetblue",width = 2))) %>%
      layout(title = paste("Histogram of", input$variable, "for", 
                           input$medal), xaxis = list(title = input$variable))
  }) # output renderplotly histogram
  
}

# Run the Shiny application 
shinyApp(ui = ui, server = server)
