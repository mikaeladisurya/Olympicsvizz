library(shiny)
library(ggplot2)
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
                  selected = "All Medals")
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Server code
server <- function(input, output) {
  
  # Reactive data
  medal_filter <- reactive({
    if (input$medal == "All Medals") {
      medal_filter <- summer_olympic
    } else {
      medal_filter <- subset(summer_olympic, Medal == input$medal)
    }
  })
  
  reactive_data <- reactive({
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
  
  # Reactive fill
  reactive_fill <- reactive({
    Color = case_when(
      input$medal == "Gold" ~ "yellow",
      input$medal == "Silver" ~ "grey",
      input$medal == "Bronze" ~ "salmon",
      TRUE ~ "blue"
    )
  })
  
  # Histogram plot
  output$histogram <- renderPlot({
    ggplot(medal_filter, aes(x = reactive_data())) +
      geom_histogram(binwidth = 1, fill = reactive_fill(), color = "white") +
      labs(x = input$variable, y = "Count", title = reactive_title()) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)