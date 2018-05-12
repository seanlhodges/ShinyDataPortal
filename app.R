## app.R ##

## dashboard-mimic-alpha
## A mimic of the dashboard available through https://envdata.boprc.govt.nz
## UI based on shinydashboard template
## Script developed by: Sean Hodges
## Updated on: 13 May 2018

## Purpose: To deliver an interactive dashboard that allows
##          access to the publically accessible natural
##          resource data collected as part of various
##          automated monitoring networks in the Horizons region


## Load libraries required
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(XML)


## ui -----------

#A dashboard has three parts: a header, a sidebar, and a body. 
ui <- dashboardPage(skin="black",
  dashboardHeader(title = "Natural Resource Data Portal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboards", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Maps", tabName = "maps", icon = icon("globe")),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Location", tabName = "location", icon = icon("map-marker")),
      menuItem("Reports", tabName = "reports", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              tags$style(".highlight {color:#E87722;font-size:1.5em}"),
              
              h3("Welcome to the Environmental data portal"),
              p("The Webportal system provides access to a range of environmental monitoring locations from across the Horizons region and their collected data. The data presented was collected to support Councils active and historical routine environmental monitoring programmes. There are many more environmental datasets held by Council that are more minor or discrete in nature that cannot easily be presented through the portal. Enquiries related to the Environmental Data Portal should be directed to the Catchment Data team of Council."),
              
              tags$b("Start your search in the  Map section by first selecting the parameter of choice, and then select the appropriate Interval and Statistic"),
              br(),
              p(),icon("dashboard",class="highlight"),span("Dashboards access informative dashboards including this homepage and rainfall maps."),br(),
              p(),icon("globe",class="highlight"),span("Map shows locations and their values overlaid on a map of the Horizons region. A choice of the latest value or a selection of basic statistics at different intervals are available."),br(),
              p(),icon("th",class="highlight"),span("Data Set provides a summary of each dataset available and is further broken up into sub-tabs of Summary, Chart, Grid and Statistics."),br(),
              p(),icon("map-marker",class="highlight"),span("Location enables you to search for a particular location. Once a location is selected it will display a list of data sets available."),br(),
              p(),icon("book",class="highlight"),span("Reports displays a list of published reports that can be downloaded as a PDF."),br(),
              
              h3("Quality Coding"),
              p("Horizons Regional Council undertakes a quality assurance process on all data that adheres to the National Environmental Monitoring Standards (NEMS) including the application of quality codes to data. For information on our quality assurance and grading process refer to  in the top right corner of the website.")
              
      ),
      
      # Second tab content
      tabItem(tabName = "maps",
              #h2("Map content"),
              
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
              
              uiOutput("choose_collection"),
              leafletOutput("map",width = "100%")
              
      ),
      tabItem(tabName = "data",
              h2("Data lists and summaries")
      ),
      tabItem(tabName = "location",
              h2("Monitoring site attributes")
      ),
      tabItem(tabName = "reports",
              h2("Printable / downloadable reports")
      )
    )
  )
)

## Code supporting interface and reactive functions --------

getCollection <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/boo.hts?service=Hilltop&request=CollectionList",sep=""))
collections<-sapply(getNodeSet(getCollection,"//Collection/@Name"),as.character)

getSites <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=SiteList&location=LatLong",sep=""))
site<-getNodeSet(getSites,"//Latitude/../@Name")
site<-sapply(site, as.character)
lat <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Latitude"), xmlValue))
lon <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Longitude"), xmlValue))
sites <-data.frame(site,lat,lon, stringsAsFactors=FALSE)


## server -----------

server <- function(input, output, session) {
  
  
  #Drop-down selection box for which data set
  output$choose_collection <- renderUI({
    selectInput("collection", "Collection", as.list(collections),selected = "Rainfall")
  })
  
  dataInput <- reactive({
    getSites <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=SiteList&location=LatLong&collection=",input$collection,sep=""))
    site<-getNodeSet(getSites,"//Latitude/../@Name")
    site<-sapply(site, as.character)
    lat <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Latitude"), xmlValue))
    lon <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Longitude"), xmlValue))
    df <-data.frame(site,lat,lon, stringsAsFactors=FALSE)
  })
   
  output$map <- renderLeaflet({
    leaflet(dataInput()) %>%  
      setView(lng = 175, lat = -39.6, zoom = 7) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon,
        ~lat,
        color = "green",
        opacity = 0.8,
        radius = 5
      )  
      #addMarkers(~lon, ~lat, popup = ~as.character(site), label = ~as.character(site))
      #addTiles(options = tileOptions(maxZoom = 28, maxNativeZoom = 19), group = 'OSM')
  })
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  
  output$busmap <- renderLeaflet({
    locations <- routeVehicleLocations()
    if (length(locations) == 0)
      return(NULL)
    
    # Show only selected directions
    locations <- filter(locations, Direction %in% as.numeric(input$directions))
    
    # Four possible directions for bus routes
    dirPal <- colorFactor(dirColors, names(dirColors))
    
    map <- leaflet(locations) %>%
      addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
      addCircleMarkers(
        ~VehicleLongitude,
        ~VehicleLatitude,
        color = ~dirPal(Direction),
        opacity = 0.8,
        radius = 8
      )
    
    if (as.numeric(input$routeNum) != 0) {
      route_shape <- get_route_shape(input$routeNum)
      
      map <- addPolylines(map,
                          route_shape$shape_pt_lon,
                          route_shape$shape_pt_lat,
                          fill = FALSE
      )
    }
    
    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }
    
    map <- map %>% mapOptions(zoomToLimits = rezoom)
    
    map
  })
}

shinyApp(ui, server)
