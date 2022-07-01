#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library('rgdal')
library('rgeos')
library('spdep')
library('rgdal')
library('maptools')
library('shapefiles')
library('ggmap')
library('ggpubr')
library('ggplot2')
library('gridExtra')
library('sf')
library('broom')
library(maps)

NYdata=read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/NY.csv",header = TRUE)
NYdata$CrudeRate = NYdata$Deaths/NYdata$Population*100
NYdata$FIPS = NYdata$countyFIPS-36000
MapTotal=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')

Map.NY=MapTotal[MapTotal$STATEFP=='36',]

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(p("Opioid Relatead Death Rate", style = "color:#3474A7")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yearselected",
        label = "Select year",
        choices = 2007:2018
        
      ),
      p("Made with", a("Shiny",
                       href = "http://shiny.rstudio.com"
      ), "."),
      img(
        src = "https://www.analyticsvidhya.com/wp-content/uploads/2016/10/shiny.png",
        width = "70px", height = "70px"
      )
      
    ),
      
      # Show a plot of the generated distribution
     
    mainPanel(
      leafletOutput(outputId = "Map.data")
      #dygraphOutput(outputId = "timetrend"),
      #DTOutput(outputId = "table")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Map.NY<-renderLeaflet({
    dataFiltered<-NYdata[which(NYdata$Year == input$yearselected),]
    Counties<-match(Map.NY@NYdata$NAME,dataFiltered$County)
    Map.NY@NYdata<-dataFiltered[Counties,]
    
    pal <- colorBin("YlOrRd", domain = Map.NY$CrudeRate, bins = 7)
    
    labels <- sprintf("%s: %g", Map.NY$County, Map.NY$CrudeRate) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(Map.NY) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(CrudeRate),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~CrudeRate,
        opacity = 0.7, title = NULL
      )
  })
  
  
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
