# ----Combining from the References:---------------------
# Lecture Notes
# https://stackoverflow.com/questions/54816608/customizing-bin-widths-in-plotlys-histogram-function-in-r
# https://plotly.com/r/histograms/
# https://rpubs.com/juanhklopper/control_histogram_color_using_Plotly_for_R
# https://towardsdatascience.com/lollipop-dumbbell-charts-with-plotly-696039d5f85
# https://epirhandbook.com/en/dashboards-with-shiny.html

library(shiny)
library(plotly)
library(dplyr)
library(shinyWidgets)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

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
  summarize(avg_medal_count = mean(medal_count)) %>%
  mutate(NOC = "Average")

# UI code
ui <- fluidPage(
  titlePanel("Line Explorer"),
  sidebarLayout(
    sidebarPanel(
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
      # sliderTextInput(
      #   inputId = "mySliderText",
      #   label = "Month range slider:",
      #   choices = host_country$Year,
      #   post = "Games",
      #   selected = c(min(host_country$Year),max(host_country$Year)),
      #   grid = TRUE,
      #   animate = TRUE
      # ),
      # verbatimTextOutput(outputId = "result")
    ),
    mainPanel(
      plotlyOutput("line_host"),
      plotlyOutput("scatter_host")
    )
  )
)

# Server code
server <- function(input, output) {
  # output$result <- renderPrint(str(input$yearRange))
  
  # Create the line chart using Plotly for difference of before and after becoming host
  output$line_host <- renderPlotly({
    filtered_host <- host_country[host_country$Year >= input$yearRange[1] & host_country$Year <= input$yearRange[2], ]
    
    plot_ly(filtered_host, x = ~Year, y = ~medal_diff, type = "scatter", mode = "lines+markers", name = "Gold as Host") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Gold Total"),
        title = "Before After Medal Difference of Hosting Country"
      )
  }) # line chart host country
  
  output$scatter_host <- renderPlotly({
    # Create the scatter plot using Plotly for host country
    plot_ly(data = host_country, color = I("gray80"))%>%
      # Create the segment connecting before and become host
      add_segments(
        x = ~Year, xend = ~Year, y = ~medal_as_host, 
        yend = ~medal_b_host, color = I("gray80"),
        showlegend = FALSE)%>%
      # Create the scatter plot when become host country
      add_trace(
        data = host_country, x = ~Year, y = ~medal_as_host, color = I("red"),
        type = "scatter", mode = "markers",text = ~paste("NOC: ", NOC), 
        name = "Host Country")%>% 
      # Create the scatter plot for before become host country
      add_trace(
        data = host_country, x = ~Year, y = ~medal_b_host, color = I("coral"), 
        type = "scatter", mode = "markers",text = ~paste("NOC: ", NOC), 
        name = "Before Host Country")%>% 
      # Create the scatter plot for average medal in each games
      add_trace(data = medal_avg_country, x = ~Year, y = ~avg_medal_count, 
                color = I("blue"),  opacity = 0.5, type = "scatter", mode = "markers",
                text = ~paste("NOC: ", NOC), name = "Average")%>%
      # Add the remaining points/other countries with gray color
      add_trace(data = medal_per_country,
                x = ~Year, y = ~medal_count, color = I("grey"), opacity = 0.4, type = "scatter", mode = "markers",
                text = ~paste("NOC: ", NOC), name = "Others")
  })
  
}

# Run the app
shinyApp(ui, server)