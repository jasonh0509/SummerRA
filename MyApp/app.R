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


data=read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/DataForJason.csv?token=GHSAT0AAAAAABVDRYPINSL5AVYH7HFCNTCEYVFEODA",header = TRUE)
data$death.rate = data$Deaths/data$Population*10000
data$FIPS = data$countyFIPS-39000
data("county.fips")

Map=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')

Map.data=Map[Map$STATEFP=='39',]
#Map.data$id2=as.numeric(Map.data$id)
#Map.data.merge=merge(Map.data,data,by.x="id2",by.y="FIPS")
#mappingdata = Map.data.merge[which(Map.data.merge$Year==2018),]

pngfile<-"https://raw.githubusercontent.com/jasonh0509/SummerRA/blob/main/images.png"
toload<-readPNG("images.png")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(p("Spatial app", style = "color:#3474A7")),
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
    mainPanel(
      leafletOutput(outputId = "Map.data")
      #dygraphOutput(outputId = "timetrend"),
      #DTOutput(outputId = "table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$img<-renderUI({
    tags$img(scr = "https://www.analyticsvidhya.com/wp-content/uploads/2016/10/shiny.png")
    
  })
  
  #output$distPlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  # x    <- faithful[, 2]
  # bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #})
  #output$table<-renderDT(AppData)
  output$Map.data<-renderLeaflet({
    dataFiltered<-data[which(data$Year == input$yearselected),]
    Counties<-match(Map.data@data$NAME,dataFiltered$County)
    Map.data@data<-dataFiltered[Counties,]
    
    
    
    pal <- colorBin("YlOrRd", domain = Map.data$death.rate, bins = 7)
    
    labels <- sprintf("%s: %g", Map.data$County, Map.data$death.rate) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(Map.data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(death.rate),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~death.rate,
        opacity = 0.7, title = NULL
      )
  })
  
  
  
  
  
  
  
}

# Run the application 

shinyApp(ui = ui, server = server)