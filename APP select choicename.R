library(shiny)

# Assuming 'df' is your dataframe containing the NOC and Country columns
df <- data.frame(
  NOC = c("AFG", "ALB", "ARG", "ARM", "ARU", "AUS", "AUT", "AZE", "BAH", "BRN", "BAN", "BAR"),
  Country = c("Afghanistan", "Albania", "Argentina", "Armenia", "Aruba", "Australia", "Austria",
              "Azerbaijan", "The Bahamas", "Bahrain", "Bangladesh", "Barbados")
)


ui <- fluidPage(
  selectInput(
    inputId = "myInput",
    label = "Select an option",
    choices = setNames(df$NOC, df$Country)
  ),
  
  verbatimTextOutput("selectedValue")
)

server <- function(input, output) {
  output$selectedValue <- renderPrint({
    input$myInput
  })
}

shinyApp(ui, server)