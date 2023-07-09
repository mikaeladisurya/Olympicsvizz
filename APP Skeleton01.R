# https://appsilon.com/shiny-application-layouts/
# https://sscc.wisc.edu/shiny/users/jstruck2/layouts/
# https://shiny.posit.co/r/reference/shiny/1.7.2/fillpage

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

# Create a color palette with handmade bins.
library(RColorBrewer)
# Read shape polygon file 
library(rgdal)

ui <- fluidPage(
  # id = 'main_page',
  # tags$style('#main_page {background-color: #007BA7;}'),
  theme = shinytheme("lumen"),
  
  headerPanel("GOLDEN LEGACIES"),
  h2("Unveiling the Secret of Summer Olympic Greatness"),
  fluidRow(
    column(width = 6,
           wellPanel(
             h2("Page description:"),
             HTML("<ul>
                      <li>This page aims to show the density of accidents happened in the greater Victoria region.</li>
                      <li>You can see Melbourne is the highest crashes happened area. The other suburb around Melbourne has lower crashes happened, and the farther from Melbourne, the less crash happened in Victoria.</li>
                      
                    </ul>
                    "
             )
           ),
           wellPanel(
             h2("Page description:"),
             HTML("<ul>
                      <li>This page aims to show the density of accidents happened in the greater Victoria region.</li>
                      <li>You can see Melbourne is the highest crashes happened area. The other suburb around Melbourne has lower crashes happened, and the farther from Melbourne, the less crash happened in Victoria.</li>
                      <li>If you selected 2020, there were less number of accidents shows on the map. this may because of the limited data collection during this year with the Covid-19 impacting</li>
                      <li>If you selected 2020, there were less number of accidents shows on the map. this may because of the limited data collection during this year with the Covid-19 impacting</li>
                      <li>If you selected 2020, there were less number of accidents shows on the map. this may because of the limited data collection during this year with the Covid-19 impacting</li>
                      <li>If you selected 2020, there were less number of accidents shows on the map. this may because of the limited data collection during this year with the Covid-19 impacting</li>
                      <li>If you selected 2020, there were less number of accidents shows on the map. this may because of the limited data collection during this year with the Covid-19 impacting</li>
                    </ul>
                    "
             )
           )
    ),
    column(width = 6,
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
           ),
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
           )
    ) # col-6
  ), # fluidrow
  fluidRow(
    sidebarPanel(
      #Sidebar panel
      h3("Gender and Age Group details"),
      radioButtons("gender", "Gender",
                   choices = c("Male", "Female"),
                   selected = "Female"),
      p("*Put your cursor on the bar chart to see the annotations"),
      br(),
      h2("Page description:"),
      HTML("<ul>
                      <li>This page aims to compare the accident case numbers in Victoria during 2006-2020 of female and male but also provides the accident rate for age group.</li>
                      <li>The Gender Camparison chart shows the accident caused by female is 117863 cases, the accident casued by male is 170164 cases. Female made the less accidents than male regardless by any age groups.</li>
                      <li>Both the Gende Bar charts show the The young drivers who at age 20-24 may have the highest risk to cause traffic crashes than the other age groups.</li>
                      <li>The accident rates that shown on bar charts represent: 'Accident number between 2006-2020 in Vic/Vic population in 2015'.</li>
                    </ul>
                    ")
    ),
    mainPanel(
      h2("Gender Comparison"),
      fluidRow(
        # Box for the plot output with full width (12/12)
        box(plotOutput("plot_donut"), width = 12)
      ),
      plotlyOutput("bar_gender")
    )
  )
)


# Server function
server <- function(input, output) {
  # Create the Sales by Region plot
  
}

# Run the Shiny application 
shinyApp(ui = ui, server = server)
