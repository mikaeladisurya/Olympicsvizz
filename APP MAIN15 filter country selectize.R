library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyverse)
library(rgdal)
library(DT)

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
# Add a new column with the difference as host and before it
host_country$cbronze_diff <- host_country$bronze_as_host - host_country$bronze_b_host
host_country$csilver_diff <- host_country$silver_as_host - host_country$silver_b_host
host_country$cgold_diff <- host_country$gold_as_host - host_country$gold_b_host
host_country$cmedal_diff <- host_country$medal_as_host - host_country$medal_b_host

host_country$rbronze_diff <- host_country$rbronze_b_host - host_country$rbronze_as_host
host_country$rsilver_diff <- host_country$rsilver_b_host - host_country$rsilver_as_host
host_country$rgold_diff <- host_country$rgold_b_host - host_country$rgold_as_host
host_country$rmedal_diff <- host_country$rmedal_b_host - host_country$rmedal_as_host

##----Prepare dataframe for medal count and rank for each country-------------
# medal per country in each games
medal_per_country <- summer_olympic %>%    # Applying group_by & summarise
  group_by(NOC, Year) %>%
  summarise(cmedal = sum(!is.na(Medal)),
            cgold = sum(Medal == "Gold", na.rm = TRUE),
            csilver = sum(Medal == "Silver", na.rm = TRUE),
            cbronze = sum(Medal == "Bronze", na.rm = TRUE))

# rank by medal
medal_per_country <- medal_per_country %>%    # Applying group_by & summarise
  group_by(Year) %>%
  mutate(rmedal = ceiling(rank(-cmedal)),
         rgold = ceiling(rank(-cgold)),
         rsilver = ceiling(rank(-csilver)),
         rbronze = ceiling(rank(-cbronze)))

# Calculate the average medal count for each game
medal_avg_country <- medal_per_country %>%
  group_by(Year) %>%
  summarize(cmedal_avg = round(mean(cmedal), 2),
            cgold_avg = round(mean(cgold), 2),
            csilver_avg = round(mean(csilver), 2),
            cbronze_avg = round(mean(cbronze), 2),
            rmedal_avg = round(mean(rmedal), 2),
            rgold_avg = round(mean(rgold), 2),
            rsilver_avg = round(mean(rsilver), 2),
            rbronze_avg = round(mean(rbronze), 2))

host_country <- merge(x = host_country, y = medal_avg_country, by = "Year", all.x = TRUE)

### SHINY UI ###
ui <- bootstrapPage(
  navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Olympics in Visualization</a>'), id="nav",
    windowTitle = "Olympics History",
    
    tabPanel(
      "Athlete Characteristic",
      div(class="outer",
          tags$head(includeCSS("styles.css")),
          leafletOutput("choro_map", width="100%", height="100%"),
          absolutePanel(
            id = "controls", class = "panel panel-default",
            top = 75, left = 55, width = 300, fixed=TRUE,
            draggable = TRUE, height = "auto",
            
            h4("-- Parameters --", align = "center"),
            h5(em("Adjust the parameters to manipulate the data showcased in each chart"), align = "center"),
            sliderInput(
              inputId = "game_range",
              label = "Games Range",
              min = min(host_country$Year),
              max = max(host_country$Year),
              value = c(min(host_country$Year), max(host_country$Year)),
              post = " Games",
              step = 4,
              sep = "",
              animate = TRUE
            ),
            radioButtons(
              "attribute", 
              "Select Attribute:",
              choices = c("Age", "Height", "Weight", "BMI"),
              selected = "Age",
              inline = TRUE),
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
              "sport", 
              "Select Sport",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = 'All Sport',
                plugins = list('remove_button')
              )
            ),
            selectizeInput(
              "region", 
              "Select Country",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = 'All Country',
                plugins = list('remove_button')
              )
            )
          ),
          
          absolutePanel(
            id = "page_description", class = "panel panel-default",
            top = 75, right = "1%", width = 1000, fixed = TRUE,
            draggable = TRUE, height = "auto",
            h4("-- Page Description --", align = "center"),
            span(tags$i(h6("One way for a country to achieving glory in the Olympics is 
             through the establishment of a formidable and world-class team of athletes.
             Becoming an elite athlete requires more than just physical attributes. 
             However, certain physical characteristics can offer a competitive advantage in specific sports.
             Understanding the distribution and diversity of the atrributes accross country and sport,
               will help us to uncover some success recipe in the Olympics.", align = "justify")), style="color:#045a8d"),
            dataTableOutput("myTable")
          ),
          
          absolutePanel(
            id = "histogram", class = "panel panel-default",
            top = "50%", right = "1%", width = 400, fixed=TRUE,
            draggable = TRUE, height = "auto",
            plotlyOutput("histogram_medal")
          )
          
      ) #/div
    ), # tabpanel
    
    tabPanel(
      "Host Countries",
      tags$div(
        fluidRow(
          sidebarPanel(
            #Sidebar panel
            h3("-- Parameters --", align = "center"),
            h5(em("Adjust the range to manipulate the data showcased in each chart, Click Play button to animate"), align = "center"),
            sliderInput(
              inputId = "yearRange",
              label = "Games Range",
              min = min(host_country$Year),
              max = max(host_country$Year),
              value = c(min(host_country$Year), max(host_country$Year)),
              post = " Games",
              step = 4,
              sep = "",
              animate = TRUE
            ),
            selectInput(
              "rank", 
              "Choose Data Type:",
              choices = c("Medal Count", 
                          "Bronze Count",
                          "Silver Count",
                          "Gold Count", 
                          "Medal Rank", 
                          "Bronze Rank", 
                          "Silver Rank", 
                          "Gold Rank"
              ),
              selected = "Medal Count"),
            br(),
            h3("-- Page Description --", align = "center"),
            tags$i(h6("Capitalizing on a nation's advantage can be achieved by hosting the Olympics on homeground,
            providing a unique opportunity to maximize their strengths and resources.
            We search to see any significant differences when a country become the Host of the Olympics.
            By visualizing and comparing the performance of host countries with other participants in the Olympics,
            we might see the correlation in hosting the Olympics with achieving bigger success."), style="color:#045a8d"),
            h5(strong("Comparation of Host Achievement")),
            p("By visualizing the comparison of Host country with others in each event,
            we can see the difference in success in terms with other country or with their achievement before"),
            HTML("<ul>
                <li>Red dot for the achievement of Host Country</li>
                <li>Yellow dot for the achievement of Host Country before becoming the Host</li>
                <li>The orange line represents the difference between the when and before the Host country is hosting the Olympics</li>
                <li>Blue dot for the average achievement all participant on that Olympic Games</li>
                <li>Grey dot for the other countries achievement</li>
              </ul>"),
            h5(strong("Difference of Host Achievement")),
            p(""),
            HTML("<ul>
                <li>Green and Red dot for the achievement of Host Country</li>
                <li>Blue dot for the achievement of Host Country before becoming the Host</li>
                <li>The line represents the difference between the when and before the Host country is hosting the Olympics</li>
                <li>Blue dot for the average achievement all participant on that Olympic Games</li>
                <li>Grey dot for the other countries achievement</li>
              </ul>")
          ),
          mainPanel(
            wellPanel(
              titlePanel(
                h2("COMPARISON OF HOST ACHIEVEMENT", align = "center"),
                h4(em("Click on the country to filter"), align = "center")
              ),
              plotlyOutput("scatter_host"),
              br(),
              HTML("<ul>
            <li>Hover to see tooltips</li>
            <li>Select area to Zoom in and double click to zoom out</li>
            <li>Click on each Legend to remove or add the series to the chart</li>
                 </ul>")
            ),# /wellPanel
            wellPanel(
              titlePanel(
                h2("DIFFERENCES OF HOST ACHIEVEMENT", align = "center"),
                h4(em("Click on the country to filter"), align = "center")
              ),
              plotlyOutput("line_host"),
              br(),
              HTML("<ul>
            <li>Hover to see tooltips</li>
            <li>Select area to Zoom in and double click to zoom out</li>
                 </ul>")
            )# /wellPanel
          )# /main panel
        ) # /fluidrow
      )
    ) # tabpanel
    
  )# navbarpage
) # bootsrapPage

server = function(input, output, session) {
  
  ##----Reactive filter for athlete page----------------------------------------
  filtered_data <- reactive({
    
    # filter main data based on medal selected
    if (input$medal == "All Medals") {
      temp_data <- summer_olympic[!is.na(summer_olympic$Medal),]
    } else {
      temp_data <- summer_olympic[summer_olympic$Medal == input$medal, ]
      temp_data[rowSums(!is.na(temp_data)) > 0, ]
    }
    
    # filter data based on games range selected
    temp_data <- temp_data[temp_data$Year >= input$game_range[1] & temp_data$Year <= input$game_range[2], ]
    
    # filter data based on gender option selected
    if (input$gender != "All") {
      temp_data <- temp_data[temp_data$Sex == input$gender, ]
      temp_data <- temp_data[rowSums(!is.na(temp_data)) > 0, ]
    }
    
    # filter data based on sport selected 
    if (is.null(input$sport)){
      temp_data <- temp_data
      updateSelectizeInput(session, 'sport', choices = sort(unique(temp_data$Sport)), server = TRUE)
    } else {
      temp_data <- temp_data[temp_data$Sport %in% input$sport, ]
    }
    
    # filter data based on country selected
    if (is.null(input$region)){
      temp_data <- temp_data
      updateSelectizeInput(session, 'region', choices = sort(unique(temp_data$NOC)), server = TRUE)
    } else {
      temp_data <- temp_data[temp_data$NOC %in% input$region, ]
    }
    # filtered data will be equal to temp_data
    temp_data
  })
  
  output$table <- renderDataTable(iris)
  
  output$myTable <- renderDataTable({
    # Select only the "Year" and "NOC" columns from the reactive dataframe
    selected_cols <- c("Year", "NOC", "Medal")
    df_selected <- filtered_data()
    
    # medal per country in each games
    medal_selected_country <- df_selected %>%    # Applying group_by & summarise
      group_by(NOC, Year) %>%
      summarise(cmedal = sum(!is.na(Medal)),
                cgold = sum(Medal == "Gold", na.rm = TRUE),
                csilver = sum(Medal == "Silver", na.rm = TRUE),
                cbronze = sum(Medal == "Bronze", na.rm = TRUE),
                cage = sum(Age > 0, na.rm = TRUE),
                cheight = sum(Height > 0, na.rm = TRUE),
                cweight = sum(Weight > 0, na.rm = TRUE),
                cbmi = sum(BMI > 0, na.rm = TRUE))
    
    # Render the selected columns as a DataTable
    datatable(medal_selected_country)
  })
  
  bin_color <- reactive({
    case_when(
      input$medal == "Gold" ~ "darkgoldenrod",
      input$medal == "Silver" ~ "darkgrey",
      input$medal == "Bronze" ~ "sienna",
      TRUE ~ "steelblue")
  })
  
  
  #----CHOROPLET----------------------------------------------------------------
  output$choro_map <- renderLeaflet({
    
    if (input$medal == "All Medals") {
      medal_pick='Total_Medals'
    } else {
      medal_pick=input$medal
    }
    
    # put reactive to dataframe so we can subset in the next step
    filtered_data <- filtered_data()
    
    ##----Preparing world spdf for filtering------------------------------------
    # https://statisticsglobe.com/mean-by-group-in-r
    # filter age data from na value, then create dataframe for each noc age mean rounded 2 decimal
    filter_age <- filtered_data[!is.na(filtered_data$Age),]
    if (nrow(filter_age) > 0) {
      age_per_noc <- aggregate(Age ~ ISO, data = filter_age, FUN = function(x) round(mean(x), 2))
    } else {
      age_per_noc <- data.frame(ISO = character(), Age = numeric())
    }
    
    # filter height data from na value, then create dataframe for each noc height mean rounded 2 decimal
    filter_height <- filtered_data[!is.na(filtered_data$Height),]
    if (nrow(filter_height) > 0) {
      height_per_noc <- aggregate(Height ~ ISO, data = filter_height, FUN = function(x) round(mean(x), 2))
    } else {
      height_per_noc <- data.frame(ISO = character(), Height = numeric())
    }
    
    # filter weight data from na value, then create dataframe for each noc weight mean rounded 2 decimal
    filter_weight <- filtered_data[!is.na(filtered_data$Weight),]
    if (nrow(filter_weight) > 0) {
      weight_per_noc <- aggregate(Weight ~ ISO, data = filter_weight, FUN = function(x) round(mean(x), 2))
    } else {
      weight_per_noc <- data.frame(ISO = character(), Weight = numeric())
    }
    
    # filter bmi data from na value, then create dataframe for each noc bmi mean rounded 2 decimal
    filter_bmi <- filtered_data[!is.na(filtered_data$BMI),]
    if (nrow(filter_bmi) > 0) {
      bmi_per_noc <- aggregate(BMI ~ ISO, data = filter_bmi, FUN = function(x) round(mean(x), 2))
    } else {
      bmi_per_noc <- data.frame(ISO = character(), BMI = numeric())
    }
    
    # Calculate the count of medals by team
    medal_counts <- filtered_data %>%
      group_by(ISO, Medal) %>%
      summarize(Count = n()) %>%
      pivot_wider(names_from = Medal, values_from = Count, values_fill = 0)
    if (input$medal == "All Medals") {
      # Add a new column for total medal count
      medal_counts <- medal_counts %>%
        mutate(
          # add with if there is no of gold, silver or bronze column then 0
          Total_Medals = ifelse("Bronze" %in% names(.), Bronze, 0) + 
            ifelse("Gold" %in% names(.), Gold, 0) +
            ifelse("Silver" %in% names(.), Silver, 0)
        )
    }
    
    # try putting count data to spatial
    world_spdf@data = data.frame(world_spdf@data, age_per_noc[match(world_spdf@data[["ISO3"]], age_per_noc[["ISO"]]),],
                                 height_per_noc[match(world_spdf@data[["ISO3"]], height_per_noc[["ISO"]]),], 
                                 weight_per_noc[match(world_spdf@data[["ISO3"]], weight_per_noc[["ISO"]]),],
                                 bmi_per_noc[match(world_spdf@data[["ISO3"]], bmi_per_noc[["ISO"]]),],
                                 medal_counts[match(world_spdf@data[["ISO3"]], medal_counts[["ISO"]]),])
    
    # #----Start plotting for Choropleth-----------------------------------
    ## ----AGE-------------------------------------------------------------
    if (input$attribute == "Age") {
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
                         fillColor = bin_color(), fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Age (Years)", position = "bottomleft" )
      
      ## ----WEIGHT-------------------------------------------------------------
    } else if (input$attribute == "Height") {
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
                         fillColor = bin_color(), fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Height (cm)", position = "bottomleft" )
      
      ## ----WEIGHT-------------------------------------------------------------
    } else if (input$attribute == "Weight") {
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
                         fillColor = bin_color(), fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "Weight (Kg)", position = "bottomleft" )
      
      ## ----BMI-------------------------------------------------------------
    } else if (input$attribute == "BMI") {
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
                         fillColor = bin_color(), fillOpacity = 0.8, color="white", radius=~log2(world_spdf@data[[medal_pick]]), stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~count, opacity=0.9, title = "BMI (Kg/M2)", position = "bottomleft" )
    }
    
  })# output renderleaflet choropleth
  
  #----HISTOGRAM----------------------------------------------------------------
  output$histogram_medal <- renderPlotly({
    
    # produce the histogram
    plot_ly(data = filtered_data(), x = ~filtered_data()[[input$attribute]], 
            type = "histogram", nbinsx = 30, 
            marker = list(color = bin_color(), line = list(color = "cadetblue",width = 2))) %>%
      layout(
        title = list(text = paste("Distribution of ", input$medal, " Winners by ", input$attribute),
                     y = 0.9),
        xaxis = list(title = input$attribute),
        yaxis = list(title = "Count")
      )
    
    
  }) # output renderplotly histogram
  ##---/Histogram---------------------------------------------------------------
  
  ##----Reactive filter for host page-------------------------------------------
  # filter host based on game slider
  filtered_host <- reactive({
    host_country[host_country$Year >= input$yearRange[1] & host_country$Year <= input$yearRange[2], ]
  })
  
  # filter host based on data type
  chosen_data_host <- reactive({
    data.frame(Year = filtered_host()$Year,
               City = filtered_host()$City,
               NOC = filtered_host()$NOC,
               data_as_host = case_when(
                 input$rank == "Bronze Count" ~ filtered_host()$bronze_as_host,
                 input$rank == "Silver Count" ~ filtered_host()$silver_as_host,
                 input$rank == "Gold Count" ~ filtered_host()$gold_as_host,
                 input$rank == "Medal Rank" ~ filtered_host()$rmedal_as_host,
                 input$rank == "Bronze Rank" ~ filtered_host()$rbronze_as_host,
                 input$rank == "Silver Rank" ~ filtered_host()$rsilver_as_host,
                 input$rank == "Gold Rank" ~ filtered_host()$rgold_as_host,
                 TRUE ~ filtered_host()$medal_as_host
               ),
               data_b_host = case_when(
                 input$rank == "Bronze Count" ~ filtered_host()$bronze_b_host,
                 input$rank == "Silver Count" ~ filtered_host()$silver_b_host,
                 input$rank == "Gold Count" ~ filtered_host()$gold_b_host,
                 input$rank == "Medal Rank" ~ filtered_host()$rmedal_b_host,
                 input$rank == "Bronze Rank" ~ filtered_host()$rbronze_b_host,
                 input$rank == "Silver Rank" ~ filtered_host()$rsilver_b_host,
                 input$rank == "Gold Rank" ~ filtered_host()$rgold_b_host,
                 TRUE ~ filtered_host()$medal_b_host
               ),
               data_avg = case_when(
                 input$rank == "Bronze Count" ~ filtered_host()$cbronze_avg,
                 input$rank == "Silver Count" ~ filtered_host()$csilver_avg,
                 input$rank == "Gold Count" ~ filtered_host()$cgold_avg,
                 input$rank == "Medal Rank" ~ filtered_host()$rmedal_avg,
                 input$rank == "Bronze Rank" ~ filtered_host()$rbronze_avg,
                 input$rank == "Silver Rank" ~ filtered_host()$rsilver_avg,
                 input$rank == "Gold Rank" ~ filtered_host()$rgold_avg,
                 TRUE ~ filtered_host()$cmedal_avg
               ),
               diff_data = case_when(
                 input$rank == "Bronze Count" ~ filtered_host()$cbronze_diff,
                 input$rank == "Silver Count" ~ filtered_host()$csilver_diff,
                 input$rank == "Gold Count" ~ filtered_host()$cgold_diff,
                 input$rank == "Medal Rank" ~ filtered_host()$rmedal_diff,
                 input$rank == "Bronze Rank" ~ filtered_host()$rbronze_diff,
                 input$rank == "Silver Rank" ~ filtered_host()$rsilver_diff,
                 input$rank == "Gold Rank" ~ filtered_host()$rgold_diff,
                 TRUE ~ filtered_host()$cmedal_diff)
    )
  })
  
  # filter countries based on game slider
  filtered_country <- reactive({
    medal_per_country[medal_per_country$Year >= input$yearRange[1] & medal_per_country$Year <= input$yearRange[2], ]
  })
  
  # filter countries data based on data type
  chosen_data_country <- reactive({
    data.frame(NOC = filtered_country()$NOC,
               Year = filtered_country()$Year,
               data_country = case_when(
                 input$rank == "Bronze Count" ~ filtered_country()$cbronze,
                 input$rank == "Silver Count" ~ filtered_country()$csilver,
                 input$rank == "Gold Count" ~ filtered_country()$cgold,
                 input$rank == "Medal Rank" ~ filtered_country()$rmedal,
                 input$rank == "Bronze Rank" ~ filtered_country()$rbronze,
                 input$rank == "Silver Rank" ~ filtered_country()$rsilver,
                 input$rank == "Gold Rank" ~ filtered_country()$rgold,
                 TRUE ~ filtered_country()$cmedal)
    )
  })
  
  # set color to be shown as selected by selectinput
  color_line <- reactive({
    case_when(
      input$rank == "Bronze Count" ~ "chocolate",
      input$rank == "Silver Count" ~ "darkgrey",
      input$rank == "Gold Count" ~ "darkgoldenrod",
      input$rank == "Medal Rank" ~ "darkturquoise",
      input$rank == "Bronze Rank" ~ "chocolate",
      input$rank == "Silver Rank" ~ "darkgrey",
      input$rank == "Gold Rank" ~ "darkgoldenrod",
      TRUE ~ "darkturquoise")
  })
  
  #----LINE CHART---------------------------------------------------------------
  # Create the line chart using Plotly for difference of before and after becoming host
  output$line_host <- renderPlotly({
    
    plot_ly(chosen_data_host(), x = ~Year, y = ~diff_data, type = "scatter",
            mode = "lines+markers", group = "group", marker = list(color = ~ifelse(diff_data >= 0, "darkgreen", "red"),size = 10),
            line = list(color =color_line()),
            name = "Medal as Host",text = ~paste("Host:", City, NOC)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = paste(input$rank, "Difference")),
        title = paste(input$rank, "Difference of Before and As Hosting Country")
      )
  }) # line chart host country
  
  #----SCATTER PLOT----------------------------------------------------------
  output$scatter_host <- renderPlotly({
    
    # Create the scatter plot using Plotly for other countries with gray color
    plot_ly(data = chosen_data_country(),
            x = ~Year, y = ~data_country, color = I("grey"), opacity = 0.6, 
            type = "scatter", mode = "markers", name = "Others", 
            text = ~paste(
              "Country: ", NOC,
              "Medal Count: "))%>%
      # Create the scatter plot for average medal in each games
      add_trace(
        data = chosen_data_host(), x = ~Year, y = ~data_avg, 
        color = I("slategrey"),  opacity = 0.6, type = "scatter", mode = "markers",
        text = ~paste("Average Medal"), name = "Average", marker = list(size = 10))%>%
      # Create the segment connecting before and become host
      add_segments(data = chosen_data_host(),
                   x = ~Year, xend = ~Year, y = ~data_as_host, 
                   yend = ~data_b_host, color = I(color_line()) ,showlegend = FALSE)%>%
      # Create the scatter plot when become host country
      add_trace(
        data = chosen_data_host(), x = ~Year, y = ~data_as_host,
        type = "scatter", mode = "markers", text = ~paste("Country: ", diff_data), 
        marker = list(color = ~ifelse(diff_data >= 0, "darkgreen", "red"), size = 10),
        name = "Host Country")%>% 
      # Create the scatter plot for before become host country
      add_trace(
        data = chosen_data_host(), x = ~Year, y = ~data_b_host, color = I("blue"), 
        type = "scatter", mode = "markers",text = ~paste("NOC: ", NOC), 
        marker = list(size = 10), name = "Before Host Country")%>% 
      layout(
        xaxis = list(title = "<br>Year"),
        yaxis = list(title = paste(input$rank, "Comparison")),
        title = paste(input$rank, "as Host Compared"),
        legend = list(orientation = 'h')
      )
  })
}
shinyApp(ui, server)