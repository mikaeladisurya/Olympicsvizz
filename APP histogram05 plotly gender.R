# ----Combining from the References:---------------------
# Lecture Notes
# https://stackoverflow.com/questions/54816608/customizing-bin-widths-in-plotlys-histogram-function-in-r
# https://plotly.com/r/histograms/
# https://rpubs.com/juanhklopper/control_histogram_color_using_Plotly_for_R

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Loading athlete data
summer_olympic <- read.csv(paste0(getwd(),"/DATA/summer_olympics.csv"))

# Filter the data for bronze medals and remove NA values
# bronze_data <- na.omit(subset(summer_olympic, Medal == "Bronze"))

# Calculate BMI
summer_olympic$BMI <- summer_olympic$Weight / ((summer_olympic$Height / 100) ^ 2)

# UI code
ui <- fluidPage(
  titlePanel("Histogram Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose Variable:", 
                  choices = c("Age", "Weight", "Height", "BMI"),
                  selected = "BMI"),
      selectInput("medal", "Choose Medal Type:",
                  choices = c("Bronze", "Silver", "Gold", "All Medals"),
                  selected = "Gold"),
      selectInput("gender", "Choose Gender:",
                  choices = c("F", "M", "All"),
                  selected = "All")
    ),
    mainPanel(
      plotlyOutput("histogram44")
    )
  )
)

# Server code
server <- function(input, output) {
  
  output$histogram44 <- renderPlotly({
    
    if (input$medal == "All Medals") {
      filtered_data <- summer_olympic[!is.na(summer_olympic$Medal),]
    } else {
      filtered_data <- na.omit(summer_olympic[summer_olympic$Medal == input$medal, ])
    }
    
    if (input$gender != "All") {
      filtered_data <- na.omit(filtered_data[summer_olympic$Sex == input$gender, ])
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
  })
  
}

# Run the app
shinyApp(ui, server)