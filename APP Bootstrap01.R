ui <- bootstrapPage(
  tags$style("
        #controls {
          background-color: #ddd;
          opacity: 0.5;
        }
        #controls:hover{
          opacity: 1;
        }
               "),
  absolutePanel(
    id = "controls", class = "panel panel-default",
    top = 75, left = 55, width = 250, fixed=TRUE,
    draggable = TRUE, height = "auto",
    tags$i(h6("Test Panel")),
    h6("Test Panel"),
    h6("Test Panel")
  ),
  plotOutput("plot1")
)

server = function(input, output, session) {
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
}
shinyApp(ui, server)