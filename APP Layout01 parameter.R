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
summer_olympic <-  merge(x = summer_olympic, y = country_code, by = "NOC", all = FALSE)

##----Create UI---------------------------------------------------------------
ui <- fluidPage(
  # id = 'main_page',
  # tags$style('#main_page {background-color: #007BA7;}'),
  theme = shinytheme("lumen"),
  
  # headerPanel("GOLDEN LEGACIES"),
  # h2("Unveiling the Secret of Summer Olympic Greatness"),
  fluidRow(
    column(width = 6,
           wellPanel(
             titlePanel(
               h1("GOLDEN LEGACIES", align = "center")
               ),
             h4("Unveiling the Secret of Summer Olympic Greatness", align = "center"),
             selectInput("variable", "Choose Variable:", 
                         choices = c("Age", "Weight", "Height", "BMI"),
                         selected = "Age"),
             selectInput("medal", "Choose Medal Type:",
                         choices = c("Bronze", "Silver", "Gold", "All Medals"),
                         selected = "All Medals"),
             selectInput("gender", "Choose Gender:",
                         choices = c("F", "M", "All"),
                         selected = "All"),
             selectizeInput("region", "Select Region",
                            choices = unique(summer_olympic$NOC),
                            multiple = TRUE,options = list(plugins = list('remove_button')))
           ),# /wellPanel
           wellPanel(
             
           )# /wellPanel
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
           ),# /wellPanel
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
           )# /wellPanel
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
