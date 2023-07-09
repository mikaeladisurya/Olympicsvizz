# ----Combining from the References:---------------------
# Lecture Notes
# https://stackoverflow.com/questions/54816608/customizing-bin-widths-in-plotlys-histogram-function-in-r
# https://plotly.com/r/histograms/
# https://rpubs.com/juanhklopper/control_histogram_color_using_Plotly_for_R
# https://towardsdatascience.com/lollipop-dumbbell-charts-with-plotly-696039d5f85

library(shiny)
library(plotly)

# Loading host_medal data
host_country <- read.csv(paste0(getwd(),"/DATA/host_medal.csv"))

# Add a new column with the sum of gold_as_host and gold_b_host
host_country$medal_diff <- host_country$medal_as_host - host_country$medal_b_host

# UI code
ui <- fluidPage(
  titlePanel("Line Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose Variable:", 
                  choices = c("Age", "Weight", "Height", "BMI"),
                  selected = "Age"),
    ),
    mainPanel(
      plotlyOutput("line_host")
    )
  )
)

# Server code
server <- function(input, output) {
  
  output$line_host <- renderPlotly({
    # Create the line chart using Plotly for difference of before and after becoming host
    plot_ly(host_country, x = ~Year, y = ~medal_diff,
            name = 'Difference of Medals', type = 'scatter', 
            mode = 'lines+markers', text = ~paste("NOC: ", NOC))
  })
  
}

# Run the app
shinyApp(ui, server)