#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(leaflet)
library(leafdown)
library(echarts4r)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(rgdal)

load("NC_ACS .Rda")
us1<-readRDS("Shapes/us1.RDS")
us2<-readRDS("Shapes/us2.RDS")
nc.map10=readOGR(dsn='Shapes',layer='tl_2015_37_tract')


##https://cran.r-project.org/web/packages/leafdown/vignettes/Introduction.html

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yearselected",
        label = "Select year",
        choices = 2007:2018
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
