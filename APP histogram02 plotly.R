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

gold_data <- na.omit(subset(summer_olympic, Medal == "Gold"))
silver_data <- na.omit(subset(summer_olympic, Medal == "Silver"))
bronze_data <- na.omit(subset(summer_olympic, Medal == "Bronze"))

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
                  selected = "All Medals")
    ),
    mainPanel(
      plotlyOutput("histplotly_gold"),
      plotlyOutput("histplotly_silver"),
      plotlyOutput("histplotly_bronze"),
      plotlyOutput("histplotly_all")
    )
  )
)

# Server code
server <- function(input, output) {
  
  # Reactive data
  gold_reactive_data <- reactive({
    switch(input$variable,
           "Age" = gold_data$Age,
           "Weight" = gold_data$Weight,
           "Height" = gold_data$Height,
           "BMI" = gold_data$BMI)
  })
  silver_reactive_data <- reactive({
    switch(input$variable,
           "Age" = silver_data$Age,
           "Weight" = silver_data$Weight,
           "Height" = silver_data$Height,
           "BMI" = silver_data$BMI)
  })
  bronze_reactive_data <- reactive({
    switch(input$variable,
           "Age" = bronze_data$Age,
           "Weight" = bronze_data$Weight,
           "Height" = bronze_data$Height,
           "BMI" = bronze_data$BMI)
  })
  allmedal_reactive_data <- reactive({
    switch(input$variable,
           "Age" = summer_olympic$Age,
           "Weight" = summer_olympic$Weight,
           "Height" = summer_olympic$Height,
           "BMI" = summer_olympic$BMI)
  })
  change_reactive_data <- reactive({
    if (input$medal == "All Medals") {
      medal_filter <- summer_olympic
    } else {
      medal_filter <- na.omit(subset(summer_olympic, Medal == input$medal))
    }
    
    switch(input$variable,
           "Age" = medal_filter$Age,
           "Weight" = medal_filter$Weight,
           "Height" = medal_filter$Height,
           "BMI" = medal_filter$BMI)
  })
  
  # Reactive title
  reactive_title <- reactive({
    paste("Histogram of", input$variable, "for", input$medal)
  })
  
  # plotly histogram gold
  output$histplotly_gold <- renderPlotly({
    p <- ggplot(gold_data, aes(x = gold_reactive_data())) +
      geom_histogram(binwidth = 1, fill = "darkgoldenrod2", color = "white") +
      theme_minimal() +
      xlab(input$variable) +
      ylab("Count")
    
    ggplotly(p)
  })
  
  # plotly histogram gold
  output$histplotly_silver <- renderPlotly({
    p <- ggplot(silver_data, aes(x = silver_reactive_data())) +
      geom_histogram(binwidth = 1, fill = "azure4", color = "white") +
      theme_minimal() +
      xlab(input$variable) +
      ylab("Count")
    
    ggplotly(p)
  })
  
  # plotly histogram gold
  output$histplotly_bronze <- renderPlotly({
    p <- ggplot(bronze_data, aes(x = bronze_reactive_data())) +
      geom_histogram(binwidth = 1, fill = "chocolate", color = "white") +
      theme_minimal() +
      xlab(input$variable) +
      ylab("Count")
    
    ggplotly(p)
  })
  
  # plotly histogram gold
  output$histplotly_all <- renderPlotly({
    p <- ggplot(summer_olympic, aes(x = allmedal_reactive_data())) +
      geom_histogram(binwidth = 1, fill = "deepskyblue2", color = "white") +
      theme_minimal() +
      xlab(input$variable) +
      ylab("Count")
    
    ggplotly(p)
  })
  
}

# Run the app
shinyApp(ui, server)