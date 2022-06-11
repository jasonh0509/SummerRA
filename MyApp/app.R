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

data = read.csv('E:/RGithub/SummerRA/DataForJason.csv',header=TRUE)
data$death.rate = data$Deaths/data$Population*10000
data$FIPS = data$countyFIPS-39000

Map=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')

Map.data=fortify(Map[Map$STATEFP=='39',],region='COUNTYFP')
Map.data$id2=as.numeric(Map.data$id)
Map.data.merge=merge(Map.data,data,by.x="id2",by.y="FIPS")
mappingdata = Map.data.merge[which(Map.data.merge$Year==2018),]

#Appmappingdata = oh.map.data.merge[which(oh.map.data.merge$Year==2018),]


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(p("Spatial app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      p("Made with", a("Shiny",
                       href = "http://shiny.rstudio.com"
      ), "."),
      img(
        src = "imageShiny.png",
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

    #output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
       # x    <- faithful[, 2]
       # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #})
  #output$table<-renderDT(AppData)
  output$Map<-renderLeaflet({
    dataFiltered<-data[which(data$Year == 2018),]
    Counties<-match(Map@data$NAME,dataFiltered$County)
    Map@data<-dataFiltered[Counties,]
    
    pal <- colorBin("YlOrRd", domain = Map$death.rate, bins = 7)
    
    labels <- sprintf("%s: %g", Map$County, Map$death.rate) %>%
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
