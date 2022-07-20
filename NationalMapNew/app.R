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


data=read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Death%20Rate%20All%20counties.csv",header = TRUE)
data<-subset(data,select = c(County,Year,County.Code,Population,Deaths))
data<-data%>%
  rename(FIPS=County.Code)

data$death.rate = data$Deaths/data$Population*100
#write.csv(data,"C:/Users/24597/Desktop/National Processed Data.csv", row.names = FALSE)
#data$FIPS = data$countyFIPS-36000

Map=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')
map.data = fortify(Map,region='COUNTYFP')


#oh.map.data=fortify(,region='COUNTYFP')

map.data$id2=as.numeric(map.data$id)
Map.data.merge=merge(data,map.data,by.x="id2",by.y="County.Code")

#mappingdata = Map.data.merge[which(Map.data.merge$Year==2018),]

##If errror occurs, for some states zips code changed over time , possible to have NAs



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(p("Opioid Relatead Death Rate", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yearselected",
        label = "Select year",
        choices = 2007:2018
      ),
      #selectInput(
      #inputId = "Stateselected",
      # label  = "Select Sate",
      # choices = c("Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Minor Outlying Islands", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "U.S. Virgin Islands", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
      #),
      p("Made with", a("Shiny",
                       href = "http://shiny.rstudio.com"
      ), "."),
      img(
        src = "https://www.analyticsvidhya.com/wp-content/uploads/2016/10/shiny.png",
        width = "70px", height = "70px"
      )
      
    ),
    mainPanel(
      leafletOutput(outputId = "Map")
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
  


  output$Map<-renderLeaflet({
    dataFiltered<-data[which(data$Year == input$yearselected),]
    Counties<-match(Map@data$NAME,dataFiltered$CountyName)
    Map@data<-dataFiltered[Counties,]
    
    pal <- colorBin("YlOrRd", domain = Map$death.rate, bins = 7)
    
    labels <- sprintf("%s: %g", Map$CountyName, Map$death.rate) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(Map) %>%
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
