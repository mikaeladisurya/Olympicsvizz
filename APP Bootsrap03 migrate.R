# Load necessary libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(leaflet)
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

# Loading host_medal data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))
# Add a new column with the sum of gold_as_host and gold_b_host
host_country$medal_diff <- host_country$medal_as_host - host_country$medal_b_host

# medal per country in each games
medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year) %>%
  summarise(medal_count = sum(!is.na(Medal)))

# Calculate the average medal count for each game
medal_avg_country <- medal_per_country %>%
  group_by(Year) %>%
  summarize(avg_medal_count = round(mean(medal_count), 2)) %>%
  mutate(NOC = "Average")

##----Create UI---------------------------------------------------------------
ui <- bootstrapPage(
  
  navbarPage(
    theme = shinytheme("flatly"),
    tabPanel(
      "About this site",
      tags$div(
        tags$h4("Last update"),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          top = 75, left = 55, width = 250, fixed=TRUE,
          draggable = TRUE, height = "auto",
          tags$i(h6("Test Panel")),
          h6("Test Panel"),
          h6("Test Panel")
        ),
        tags$br(),tags$br(),tags$h4("Background"), 
        "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
        tags$br(),tags$br(),
        "In isolation, these headlines can be hard to interpret. 
                        How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                        This site is updated daily based on data published by Johns Hopkins University. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic."
      )
    )
  ) # navbar
) # / ui

server = function(input, output, session) {
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  #----CHOROPLET----------------------------------------------------------
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
    
    # filter data based on games range selected
    filtered_data <- filtered_data[filtered_data$Year >= input$game_range[1] & filtered_data$Year <= input$game_range[2], ]
    
    # filter data based on gender option selected
    if (input$gender != "All") {
      filtered_data <- filtered_data[filtered_data$Sex == input$gender, ]
      filtered_data <- filtered_data[rowSums(!is.na(filtered_data)) > 0, ]
    }
    
    # filter data based on country selected
    if (is.null(input$region)){
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data[filtered_data$NOC %in% input$region, ]
    }
    
    # filter data based on sport selected 
    if (is.null(input$sport)){
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data[filtered_data$Sport %in% input$sport, ]
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
      input$medal == "Gold" ~ "gold",
      input$medal == "Silver" ~ "darkgrey",
      input$medal == "Bronze" ~ "chocolate",
      TRUE ~ "coral"
    )
    
    # #----Start plotting for Choropleth-----------------------------------
    ## ----AGE-------------------------------------------------------------
    if (input$attribute == "Age") {
      mybins <- c(NA, 18, 20, 22, 24, 26, 28, 30, Inf)
      mypalette <- colorBin( palette="YlGnBu", domain=world_spdf@data$Age, na.color="transparent", bins=mybins)
      
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
                         fillColor = bin_color, fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Age (Years)", position = "bottomleft" )
      
      ## ----WEIGHT-------------------------------------------------------------
    } else if (input$attribute == "Height") {
      mybins <- c(NA, 160, 165, 170, 175, 180, 185, 190, Inf)
      mypalette <- colorBin( palette="YlGnBu", domain=world_spdf@data$Height, na.color="transparent", bins=mybins)
      
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
                         fillColor = bin_color, fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Height (cm)", position = "bottomleft" )
      
      ## ----WEIGHT-------------------------------------------------------------
    } else if (input$attribute == "Weight") {
      mybins <- c(NA, 55, 62, 69, 76, 83, 90, Inf)
      mypalette <- colorBin( palette="YlGnBu", domain=world_spdf@data$Weight, na.color="transparent", bins=mybins)
      
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
                         fillColor = bin_color, fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Weight (Kg)", position = "bottomleft" )
      
      ## ----BMI-------------------------------------------------------------
    } else if (input$attribute == "BMI") {
      mybins <- c(NA, 20, 21, 22, 23, 24, 25, Inf)
      mypalette <- colorBin( palette="YlGnBu", domain=world_spdf@data$BMI, na.color="transparent", bins=mybins)
      
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
                         fillColor = bin_color, fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "BMI (Kg/M2)", position = "bottomleft" )
    }
    
  })# output renderleaflet choropleth
  
  #----HISTOGRAM----------------------------------------------------------
  output$histogram_medal <- renderPlotly({
    
    # filter main data based on medal selected
    if (input$medal == "All Medals") {
      filtered_data <- summer_olympic[!is.na(summer_olympic$Medal),]
    } else {
      filtered_data <- summer_olympic[summer_olympic$Medal == input$medal, ]
      filtered_data <- filtered_data[rowSums(!is.na(filtered_data)) > 0, ]
    }
    
    # filter data based on games range selected
    filtered_data <- filtered_data[filtered_data$Year >= input$game_range[1] & filtered_data$Year <= input$game_range[2], ]
    
    # filter data based on gender option selected
    if (input$gender != "All") {
      filtered_data <- filtered_data[filtered_data$Sex == input$gender, ]
      filtered_data <- filtered_data[rowSums(!is.na(filtered_data)) > 0, ]
    }
    
    # filter data based on country selected
    if (is.null(input$region)){
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data[filtered_data$NOC %in% input$region, ]
    }
    
    # filter data based on sport selected 
    if (is.null(input$sport)){
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data[filtered_data$Sport %in% input$sport, ]
    }
    
    # set color for used in histogram bin
    bin_color <- case_when(
      input$medal == "Gold" ~ "gold",
      input$medal == "Silver" ~ "darkgrey",
      input$medal == "Bronze" ~ "chocolate",
      TRUE ~ "coral"
    )
    
    # produce the histogram
    plot_ly(data = filtered_data, x = ~filtered_data[[input$attribute]], 
            type = "histogram", nbinsx = 30, 
            marker = list(color = bin_color, line = list(color = "cadetblue",width = 2))) %>%
      layout(
        title = paste("Distribution of ", input$medal, " Medal Winners by ", input$attribute),
        xaxis = list(title = input$attribute),
        yaxis = list(title = "Count")
      )
  }) # output renderplotly histogram
  
  #----LINE CHART----------------------------------------------------------
  # Create the line chart using Plotly for difference of before and after becoming host
  output$line_host <- renderPlotly({
    filtered_host <- host_country[host_country$Year >= input$yearRange[1] & host_country$Year <= input$yearRange[2], ]
    
    plot_ly(filtered_host, x = ~Year, y = ~medal_diff, type = "scatter",
            mode = "lines+markers", group = "group", marker = list(color = ~ifelse(medal_diff >= 0, "darkgreen", "red"),width = 2),
            line = list(color ="darkturquoise"),
            name = "Medal as Host",text = ~paste("Host: ", City, NOC)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Medal Total"),
        title = "Before and When Medal Difference of Hosting Country"
      )
  }) # line chart host country
  
  #----SCATTER PLOT----------------------------------------------------------
  output$scatter_host <- renderPlotly({
    
    filtered_host <- host_country[host_country$Year >= input$yearRange[1] & host_country$Year <= input$yearRange[2], ]
    filtered_avg <- medal_avg_country[medal_avg_country$Year >= input$yearRange[1] & medal_avg_country$Year <= input$yearRange[2], ]
    filtered_country <- medal_per_country[medal_per_country$Year >= input$yearRange[1] & medal_per_country$Year <= input$yearRange[2], ]
    
    # Create the scatter plot using Plotly for other countries with gray color
    plot_ly(data = filtered_country,
            x = ~Year, y = ~medal_count, color = I("grey"), opacity = 0.6, 
            type = "scatter", mode = "markers", name = "Others", 
            text = ~paste(
              "Country: ", NOC,
              "Medal Count: "))%>%
      # Create the segment connecting before and become host
      add_segments(data = filtered_host,
                   x = ~Year, xend = ~Year, y = ~medal_as_host, 
                   yend = ~medal_b_host, color = I("darksalmon"),showlegend = FALSE)%>%
      # Create the scatter plot when become host country
      add_trace(
        data = filtered_host, x = ~Year, y = ~medal_as_host, color = I("red"),
        type = "scatter", mode = "markers",text = ~paste("Country: ", NOC), 
        marker = list(size = 10), name = "Host Country")%>% 
      # Create the scatter plot for before become host country
      add_trace(
        data = filtered_host, x = ~Year, y = ~medal_b_host, color = I("yellow"), 
        type = "scatter", mode = "markers",text = ~paste("NOC: ", NOC), 
        marker = list(size = 10), name = "Before Host Country")%>% 
      # Create the scatter plot for average medal in each games
      add_trace(
        data = filtered_avg, x = ~Year, y = ~avg_medal_count, 
        color = I("blue"),  opacity = 0.6, type = "scatter", mode = "markers",
        text = ~paste("Average Medal"), name = "Average", marker = list(size = 10))%>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Medal Count"),
        title = "Medal Count as Host Compared"
      )
  })
}
shinyApp(ui, server)