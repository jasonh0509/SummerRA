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
library(echarts4r)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(rgdal)
library(raster)
library(leafdown)

load("NC_ACS .Rda")
us1<-readRDS("Shapes/us1.RDS")
us2<-readRDS("Shapes/us2.RDS")
nc.map10=readOGR(dsn='Shapes',layer='tl_2015_37_tract')
nc1<-raster::getData(country="USA",level="1")
nc2<-raster::getData(country="USA",level="2")
spdf.list<-list(us1,us2)
my_leafdown$add_data(countydata5)

##https://cran.r-project.org/web/packages/leafdown/vignettes/Introduction.html

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  tags$style(HTML(".leaflet-container {background: #ffffff;}")),
  useShinyjs(),
  actionButton("drill_down", "Drill Down"),
  actionButton("drill_up", "Drill Up"),
  leafletOutput("leafdown", height = 600),
)


##Label

create_labels <- function(countydata5, map_level) {
  labels <- sprintf(
    "<strong>%s</strong><br/>%g € per capita</sup>",
    data[, paste0("NAME_", map_level)], countydata5$TotPop
  )
  labels %>% lapply(htmltools::HTML)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  leafdown.try<-Leafdown$new(spdf.list, map_output_id = "leafdown", input = input)
  update_leafdown<-reactiveVal(0)
  
  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    update_leafdown(update_leafdown() + 1)
  })
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
