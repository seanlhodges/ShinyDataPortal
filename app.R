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
library(tidyr)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(XML)
library(highcharter)

## Code supporting interface and reactive functions --------

# * Deliver list of collections *
getCollection <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/boo.hts?service=Hilltop&request=CollectionList",sep=""))
collections<-sapply(getNodeSet(getCollection,"//Collection/@Name"),as.character)

# * Deliver list of all sites   *
getSites <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=SiteList&location=LatLong",sep=""))
site<-getNodeSet(getSites,"//Latitude/../@Name")
site<-sapply(site, as.character) # this vector can populate a site dropdown box
lat <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Latitude"), xmlValue))
lon <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Longitude"), xmlValue))
# Construct dataframe that can be mapped
sites <-data.frame(site,lat,lon, stringsAsFactors=FALSE)

# Reactive functions needed
# 1. Ability to dynamically get list of sites for a single collection
# 2. Ability to dynamically get list of available measurements for a single site
# 3. Ability to dynamically get time series data from site/meausurement combo

## ui -----------

# -----------------------------------
# * Overall dashboard configuration *

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
      # -------------------------
      #     First tab content
      # * Description of portal *
      # -------------------------
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
      
      # -------------------------
      #    Second tab content
      # *  Monitoring site map  *
      # -------------------------
      tabItem(tabName = "maps",
              #h2("Map content"),
              
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
              
              uiOutput("choose_collection"),
              leafletOutput("map",width = "100%")
              
      ),

      # -------------------------
      #     Third tab content
      # *  Chart of Time series *
      # -------------------------
    
      # Link chart to site and measurement selected on map
      # Also have dropdowns for:
       #         - Monitoring site
       #         - Measurement
       #         - Time interval to chart
    
       # For rainfall measurement show cumulative rainfall
       # totals over the period requested (day, week, month, yr)

      tabItem(tabName = "data",
              h2("Data lists and summaries"),
              fillRow(
                uiOutput("data_choose_site"),
                uiOutput("data_choose_measurement"),
                selectInput("interval",label = "Interval",choices = list("Daily" = 1,"Weekly"=2, "Monthly"=3,"Yearly"=4),selected=2),
                flex = 1, height = "70px"
              ),
              fillRow(
                mainPanel(
                tabsetPanel(
                  id="mySiteData",
                  tabPanel("Info"),
                  tabPanel("Chart",
                           highchartOutput("tsChart")),
                  tabPanel("Data", 
                           DT::dataTableOutput('SiteMeasurementData')),
                  tabPanel("Statistics")
                ))
              )
      ),

      # -------------------------
      #    Fourth tab content
      # *  Location attributes  *
      # -------------------------
      tabItem(tabName = "location",
              h2("Monitoring site attributes"),
              uiOutput("location_choose_site")
 
      ),


      # -------------------------
      #     Fifth tab content
      # * Available PDF reports *
      # -------------------------
      tabItem(tabName = "reports",
              h2("Printable / downloadable reports")
      )
    )
  )
)


## server -----------

server <- function(input, output, session) {
  
  # Reactive functions defined
  # 1. Ability to dynamically get list of sites for a single collection
  CollectionList <- reactive({
    
    var1 <- input$collection
    getSites <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=SiteList&location=LatLong&collection=",var1,sep=""))
    site<-getNodeSet(getSites,"//Latitude/../@Name")
    site<-sapply(site, as.character)
    lat <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Latitude"), xmlValue))
    lon <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Longitude"), xmlValue))
    df <-data.frame(site,lat,lon, stringsAsFactors=FALSE)
    
    if(var1=="Rainfall"){
    getData <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Collection=",var1,"&method=Total&interval=1%20day/timeInterval=P1D/now",sep=""))
    } else {
    getData <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Collection=",var1,sep=""))
    }
    site<-getNodeSet(getData,"//Measurement/@SiteName")
    site<-sapply(site, as.character)
    sensor<-getNodeSet(getData,"//Measurement/DataSource/@Name")
    sensor<-sapply(sensor, as.character)
    LastValue<-sapply(getNodeSet(getData,"//Measurement/Data/E[last()]/I1"),xmlValue)
    if(var1=="River Level"|var1=="Flow"){
      LastValue<-as.character(round(as.numeric(LastValue)/1000,1))
    } else if (var1=="Rainfall"){
      LastValue<-as.character(round(as.numeric(LastValue),0))
    } else {
      LastValue<-as.character(round(as.numeric(LastValue),1))
    }
    df1 <-data.frame(site,sensor,LastValue, stringsAsFactors=FALSE)
    
    df <- merge(x=df,y=df1,by="site",all.x=TRUE)
    
    
  })
  
  
  # Reactive functions defined
  # 2. Ability to dynamically get list of available measurements for a single site
  SiteMeasurementList <- reactive({
    getMeas <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=MeasurementList&Site=",input$data_siteName,sep=""))
    # Need to construct a flat table of:
    #  DataSourceName, MeasurmentName, DataType, Interpolation, Format, Units, Available Start Date, Available End Date
    #Vector of DataSource Names
    ds <- sapply(getNodeSet(getMeas,"//DataSource[TSType='StdSeries']/@Name"),as.character)
    
    #for each datasource, get the available measurement names
    for(i in 1:length(ds)){
      if(i==1){
        dm <- sapply(getNodeSet(getMeas,paste("//DataSource[TSType='StdSeries' and @Name='",ds[i],"']/Measurement/@Name",sep="")),as.character)
        dm <- as.data.frame(dm, stringsAsFactors=FALSE)
        dm$DataSourceName <- ds[i]
        names(dm) <- c("MeasurementName","DataSourceName")
      } else {
        bb <- sapply(getNodeSet(getMeas,paste("//DataSource[TSType='StdSeries' and @Name='",ds[i],"']/Measurement/@Name",sep="")),as.character)
        bb <- as.data.frame(bb, stringsAsFactors=FALSE)
        bb$DataSourceName <- ds[i]
        names(bb) <- c("MeasurementName","DataSourceName")
        dm <- rbind(dm,bb)
        rm(bb)
      } 
    }
    
    # For this constructed dataframe, it will now be possible to extract all the other necessary
    # element and attribute values through one further for-loop
    
    MeasurementName <- paste(dm$MeasurementName," [",dm$DataSourceName,"]",sep="")
  })
  
  # Reactive functions defined
  # 3. Ability to dynamically get time series data from site/meausurement combo
  GetData <- reactive({
    #Get name of datasource in order to pull measurements correctly
    #Need to use regex to extract text between [ and ]
    str  <- input$data_measurement
    expr <- "(?<=\\[)(.*)(?=\\])"
    ds   <- regmatches(x=str,regexpr(expr,str,perl=TRUE))
    
    if(ds=="SCADA Rainfall"|ds=="Rainfall"){
      chartType <- "Bar"
      url <- paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Site=",input$data_siteName,"&Measurement=",input$data_measurement,"&TimeInterval=P7D/now&method=Total&interval=1 hour&alignment=00:00:00",sep="")
    } else {
      chartType <- "Continuous"
      url <- paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Site=",input$data_siteName,"&Measurement=",input$data_measurement,"&TimeInterval=P7D/now",sep="")
    }
    
    getData <- xmlInternalTreeParse(url)
    DateTime <- sapply(getNodeSet(getData, "//Hilltop/Measurement/Data/E/T"), xmlValue)
    Value <- sapply(getNodeSet(getData, "//Hilltop/Measurement/Data/E/I1"), xmlValue)
    
    df <- data.frame(DateTime,Value,stringsAsFactors = FALSE)
    #df$chart <- chartType
    
    
  })
  
  
  output$SiteMeasurementData <- DT::renderDataTable({
    df<-GetData()
    DT::datatable(df, options = list(pageLength = 15))
  })
  
  ## Outputting reactive function values to ui controls
  
  #Drop-down selection box to contain Collection List
  output$choose_collection <- renderUI({
    selectInput("collection", "Collection", as.list(collections),selected = "Rainfall")
  })
  
  #Drop-down selection box to contain Site List
  output$data_choose_site <- renderUI({
    selectInput("data_siteName", "Site", as.list(site),selected = "Manawatu at Teachers College")
  })
  #Drop-down selection box to contain Site List
  output$location_choose_site <- renderUI({
    selectInput("location_siteName", "Site", as.list(site),selected = "Manawatu at Teachers College")
  })
  
  #Drop-down selection box to contain Measurement List at a Site
  output$data_choose_measurement <- renderUI({
    selectInput("data_measurement", "Measurements", as.list(SiteMeasurementList()))
  })
  
  # Store last zoom button value so we can detect when it's clicked
  #lastZoomButtonValue <- NULL
  
  output$map <- renderLeaflet({
    map <- leaflet(CollectionList()) %>%  
      setView(lng = 175.1, lat = -39.8, zoom = 8) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon,
        ~lat,
        color = rgb(0.4, 0.4, 0.6),
        fillOpacity = 0.8,
        opacity = 1,
        radius = 12,
        stroke = 0
      ) %>%
      addLabelOnlyMarkers(
        ~lon,
        ~lat,
        label = ~LastValue,
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          textOnly = TRUE,
          opacity = 1,
          direction = "top",
          style=list(
            'color'='white',
            'font-size' = '10px',
            'position' = 'absolute',
            'top' = '20px'
            
          )
        )
        
        
      )
    #addMarkers(~lon, ~lat, popup = ~as.character(site), label = ~as.character(site))
    #addTiles(options = tileOptions(maxZoom = 28, maxNativeZoom = 19), group = 'OSM')

    map
    
  })
  
  # getID <- reactive({
  #   #invalidateLater(60000)
  #   y <- getDataIntraDay(input$text, input$radio)
  #   return(y)
  # })
  
  output$tsChart <- renderHighchart({
    #Get name of datasource in order to pull measurements correctly
    #Need to use regex to extract text between [ and ]
    str  <- input$data_measurement
    expr <- "(?<=\\[)(.*)(?=\\])"
    ds   <- regmatches(x=str,regexpr(expr,str,perl=TRUE))
    
    if(ds=="SCADA Rainfall"|ds=="Rainfall"){
      chartType <- "column"
      url <- paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Site=",input$data_siteName,"&Measurement=",input$data_measurement,"&TimeInterval=P7D/now&method=Total&interval=1 hour&alignment=00:00:00",sep="")
    } else {
      chartType <- "line"
      url <- paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Site=",input$data_siteName,"&Measurement=",input$data_measurement,"&TimeInterval=P7D/now",sep="")
    }
    
    getData <- xmlInternalTreeParse(url)
    DateTime <- sapply(getNodeSet(getData, "//Hilltop/Measurement/Data/E/T"), xmlValue)
    DateTime <- strptime(DateTime,format = "%Y-%m-%dT%H:%M:%S",tz="NZ")
    
    Value <- as.numeric(sapply(getNodeSet(getData, "//Hilltop/Measurement/Data/E/I1"), xmlValue))
    
    df <- data_frame(as.POSIXct(DateTime),Value)
    names(df) <- c("x","y")
    
    # converting datetime to a timestamp - needed by highcharter library
    df$x <- datetime_to_timestamp(df$x)
    
    # Creating a simple timeseries chart for continuous data
    hchart(df, type = chartType,
           hcaes(x = x, y=y)) %>% 
      hc_title(text = input$data_siteName) %>% 
      hc_subtitle(text = input$data_measurement) %>% 
      hc_xAxis(type="datetime",
               title = list(text="Date")) %>%
      hc_yAxis(title = list(text=input$data_measurement))

  })

}

shinyApp(ui, server)
