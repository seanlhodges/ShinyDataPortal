## app.R ##

## dashboard-mimic-alpha
## A mimic of the dashboard available through https://envdata.boprc.govt.nz
## UI based on shinydashboard template
## Script developed by: Sean Hodges
## Created on: 13 May 2018
## Updated on: 30 May 2018
version.number <- "2018.0.40"   # reflects Year.Version.GitCommits

## Purpose: To deliver an interactive dashboard that allows
##          access to the publically accessible natural
##          resource data collected as part of various
##          automated monitoring networks in the Horizons region


## Load libraries required
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(tidyr)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(XML)
library(highcharter)
library(RColorBrewer)

#iframe links
ivrLink <- "http://horizonsrc.maps.arcgis.com/apps/View/index.html?appid=d6dc8d35cfaa44fcb9dbada7de2cf40b"

cat("Get list of collections\n")
## Code supporting interface and reactive functions --------
# * Deliver list of collections *
getCollection <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/boo.hts?service=Hilltop&request=CollectionList",sep=""))
collections<-sapply(getNodeSet(getCollection,"//Item/../../Collection/@Name"),as.character)

cat("Get site and measurement names from collections\n")
# * Build dataframe to hold collection, sitename and measurement rows
s <- sapply(getNodeSet(getCollection,"//Item/SiteName"),xmlValue)
m <- sapply(getNodeSet(getCollection,"//Item/Measurement"),xmlValue)

for(i in 1:length(s)){
  cn <- sapply(getNodeSet(getCollection,paste("//Collection[Item/SiteName='",s[i],"' and Item/Measurement='",m[1],"']/@Name",sep="")),as.character)
}

cat("Get list of sites and lat/lon's\n")
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
cat("Create ui\n")
ui <- dashboardPage(skin="black",
  dashboardHeader(title = "Sensor Data",disable=FALSE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboards", tabName = "dashboard", icon = icon("dashboard"),
               menuSubItem("Welcome", tabName = "dashWelcome"),
               menuSubItem("Climate Summary", tabName = "dashClimate"),
               menuSubItem("River height warnings", tabName = "dashRiverHeights"),
               menuSubItem("Overall data", tabName = "dashOverall")
      ),
      menuItem("Maps", tabName = "maps", icon = icon("globe"),
               menuSubItem("Horizons Envirodata", tabName = "dashEnvirodata"),
               menuSubItem("Map Analytics", tabName = "dashAnalytics")
      ),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Stations", tabName = "stations", icon = icon("compass")),
      menuItem("Reports", tabName = "reports", icon = icon("book"))
    ),
    div(class = "version-class", 
        hr(),
        date(),br(),
        HTML(paste("Sensor Data Portal",br(),
                   "Version:",version.number,br(),
                   "Horizons Regional Council",br(),
                   R.Version()$version.string,sep="")),
        HTML('<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.')
    )
  ),
  
  dashboardBody(
    tags$head(tags$script(src="chat.js")),
    tags$head(tags$style(HTML('
                .version-class {
                                position: absolute;
                                top: calc(100% - 220px);
                                font-family: Calabri,Helvetica,Arial,sans-serif;
                                \\font-weight: bold;
                                font-size: 11px;
                                line-height: normal;
                                text-align: center;
                                padding-left: 10px;
                                padding-right: 10px;
                }
                h1             {
                                font-family: Calabri,Open Sans,Arial,Helvetica Neue,Helvetica,sans-serif;
                                font-size: 35px;
                                font-weight: 300;
                                line-height: 45px;
                                letter-spacing:0px;
                                color:white;
                                padding-left: 22px;
                                padding-top: 75px;
                                padding-bottom: 30px;
                                width: 400px
                }
                .bgimg         {
                                background-image: url("http://www.horizons.govt.nz/HRC/media/Media/Banner/Lower-Pohangina-Valley.jpg");
                                background-size: cover;
                                width:100%;
                                position:relative;
                                top:0px;
                                
                }
                .main-header .logo .skin-black .navbar .navbar-static-top{
                                background-color: #000;
                }

                .logo          {
                                background-image: url("http://www.horizons.govt.nz/HRC/images/logo-compact.png");
                                background-size: 75%;
                                background-repeat:no-repeat;
                                align: center
                              }

              '))),
    tabItems(
      # -------------------------
      #     First tab content
      #      First sub menu
      # * Description of portal *
      # -------------------------
      tabItem(tabName = "dashWelcome",
              tags$style(".highlight {color:#E87722;font-size:1.5em} h2 {text-align:center}"),
              div(class="bgimg",h1("Welcome to the Sensor Data Portal")),
              p("The Webportal system provides access to a range of environmental monitoring locations from across the Horizons region and their collected data. The data presented was collected to support Councils active and historical routine environmental monitoring programmes. There are many more environmental datasets held by Council that are more minor or discrete in nature that cannot easily be presented through the portal. Enquiries related to the Natural Resources Data Portal should be directed to the Catchment Data team of Council."),
              
              tags$b("Start your search in the  Map section by first selecting the parameter of choice, and then select the appropriate Interval and Statistic"),
              br(),
              p(),icon("dashboard",class="highlight"),span("Dashboards access informative screens including this homepage and rainfall maps."),br(),
              p(),icon("globe",class="highlight"),span("Map shows locations and their values overlaid on a map of the Horizons region. A choice of the latest value or a selection of basic statistics at different intervals are available."),br(),
              p(),icon("th",class="highlight"),span("Data Set provides a summary of each dataset available and is further broken up into sub-tabs of Summary, Chart, Grid and Statistics."),br(),
              p(),icon("compass",class="highlight"),span("Stations enables you to search for a particular station name. Once a name is selected it will display a list of data sets available."),br(),
              p(),icon("book",class="highlight"),span("Reports displays a list of published reports that can be downloaded as a PDF."),br(),
              
              h3("Quality Coding"),
              p(),span("Horizons Regional Council undertakes a quality assurance process on all data that adheres to the National Environmental Monitoring Standards ("),a(href='http://www.nems.org.nz/', target='_blank', 'NEMS'),span(") including the application of quality codes to data.")
              
      ),
      # -------------------------
      #     First tab content
      #      Second sub menu
      # * Description of portal *
      # -------------------------
      tabItem(tabName = "dashClimate",
              tags$style(".highlight {color:#E87722;font-size:1.5em}"),
              
              h3("Climate Summary"),
              p("Real time data for such measurements as rainfall, soil moisture and wind speed and direction, can be aggregated over time (on a daily, weekly, monthly or annual basis) in order to present and compare conditions during the year, or from year to year."),
              p("Long term signals in the data around climate change ...")
      ),
      
      # -------------------------
      #     First tab content
      #      Third sub menu
      # * Description of portal *
      # -------------------------
      tabItem(tabName = "dashRiverHeights",
              tags$style(".highlight {color:#E87722;font-size:1.5em}"),
              
              h3("Rising River Levels"),
              tags$iframe(src=ivrLink, height=700,width="100%")
              #htmlOutput("IVR")
      ),
      
      # -------------------------
      #     First tab content
      #      Fourth sub menu
      # * Description of portal *
      # -------------------------
      tabItem(tabName = "dashOverall",
              tags$style(".highlight {color:#E87722;font-size:1.5em}"),
              
              h3("Data delivery statistics"),
              p("How much data has been delivered today, last week, last month, this year, compared to last year"),
              p("Opportunity for a simple dashboard for statistics on data acquisition.")
      ),
  
        # -------------------------
        #     Second tab content
        #      First sub menu
        # * Horizons Envirodata Page *
        # -------------------------
        tabItem(tabName = "dashAnalytics",
              cat("Map content\n"),
              
              tags$style(type = "text/css", "#map {height: calc(100vh - 240px) !important;}"),
              
              uiOutput("choose_collection"),
              leafletOutput("map",width = "100%")
        ),
        # -------------------------
        #     Second tab content
        #      First sub menu
        # * Horizons Envirodata Page *
        # -------------------------
        tabItem(tabName = "dashEnvirodata",
                tags$style(".highlight {color:#E87722;font-size:1.5em}"),
                
                h3("Horizons Envirodata"),
                tags$iframe(src="https://envirodata.horizons.govt.nz/", height=700,width="100%")
    
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
              fillRow(width="100%",
                uiOutput("data_choose_site"),
                uiOutput("data_choose_measurement"),
                selectInput("interval",label = "Interval",choices = list("Daily" = 1,"Weekly"=2, "Monthly"=3,"Yearly"=4),selected=2),
                flex = 1, height = "100px"
              ),
              fillRow(width="100%",
                mainPanel(width=12,   # makes the mainPanel fill the entire width of the screen, otherwise defaults to 8
                tabsetPanel(
                  id="mySiteData",
                  tabPanel("Info",
                           h3("Site information"),
                           p("Place holder for site metadata. This can be populated by the hilltop call to 'SiteInfo'"),
                           htmlOutput("siteinfo")

                           ),
                  tabPanel("Chart & Data",
                           fluidRow(
                             column(width=8,
                               highchartOutput("tsChart")
                             ),
                             column(width=4,
                                DT::dataTableOutput('SiteMeasurementData')
                             )
                           )
                          ),
                  #tabPanel("Data", 
                  #         DT::dataTableOutput('SiteMeasurementData')
                  #         ),
                  tabPanel("Statistics")
                ))
              )
      ),

      # -------------------------
      #    Fourth tab content
      # *  Location attributes  *
      # -------------------------
      tabItem(tabName = "stations",
              h2("Monitoring Stations & available timeseries"),
              uiOutput("location_choose_site"),
              DT::dataTableOutput('measurementList')
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
cat("Define server code")
server <- function(input, output, session) {

  ## modal intro dialog
  showModal(modalDialog(
    title = "Data Portal Disclaimer",
    "Horizons Regional Council endeavours to provide useful and accurate information. Horizons Regional Council shall not, however, be liable whether in contract, tort, equity or otherwise, for any loss or damage of any type (including consequential losses) arising directly or indirectly from the inadequacy or any other deficiency in information supplied irrespective of the cause. Use of information supplied is entirely at the risk of the recipient and shall be deemed to be acceptance of this liability exclusion.",
    easyClose = TRUE,
    footer = modalButton("Accept")
  ))
  
  
  # Reactive functions defined
  # 1. Ability to dynamically get list of sites for a single collection
  CollectionList <- reactive({
    #cat("input$collection",input$collection,"\n")
    
    var1 <- input$collection
    cat("var1",var1,"\n")
    if(is.null(var1)){
      return(NULL)
    } else {
    getSites <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=SiteList&location=LatLong&collection=",var1,sep=""))
    site<-getNodeSet(getSites,"//Latitude/../@Name")
    site<-sapply(site, as.character)
    lat <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Latitude"), xmlValue))
    lon <- as.numeric(sapply(getNodeSet(getSites, "//HilltopServer/Site/Longitude"), xmlValue))
    df <-data.frame(site,lat,lon, stringsAsFactors=FALSE)
    
    if(var1=="Rainfall"){
      getData <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Collection=",var1,"&method=Total&interval=1%20day",sep=""))
    } else {
      getData <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=GetData&Collection=",var1,sep=""))
    }
    site<-getNodeSet(getData,"//Measurement/@SiteName")
    site<-sapply(site, as.character)
    sensor<-getNodeSet(getData,"//Measurement/DataSource/@Name")
    sensor<-sapply(sensor, as.character)
    observedProperty<-sapply(getNodeSet(getData,"//Measurement/DataSource/ItemInfo/ItemName"),xmlValue)
    LastValue<-sapply(getNodeSet(getData,"//Measurement/Data/E[last()]/I1"),xmlValue)
    numValue <- as.numeric(LastValue)
    descValue <- "Last recorded value"
    dateValue <- sapply(getNodeSet(getData,"//Measurement/Data/E[last()]/T"),xmlValue)
    if(var1=="River Level"|var1=="Flow"|var1=="Groundwater"){
      LastValue<-as.character(round(as.numeric(LastValue)/1000,1))
    } else if (var1=="Rainfall"){
      LastValue<-as.character(round(as.numeric(LastValue),0))
      descValue<-"Last 24 hours"
    } else {
      LastValue<-as.character(round(as.numeric(LastValue),1))
    }
    #cat("length(df1)",length(df),"\n")
    df1 <-data.frame(site,sensor,observedProperty,dateValue,LastValue,numValue,descValue, stringsAsFactors=FALSE)
    
    df <- merge(x=df,y=df1,by="site",all.x=TRUE)
    #cat("length(df)",length(df),"\n")
    }
  })
  
  
  # Reactive functions defined
  # 2. Ability to dynamically get list of available measurements for a single site
  SiteMeasurementList <- reactive({
    if(is.null(input$data_siteName)){
      return()
    } else {
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
    }
  })
  
  # Reactive functions defined
  # 2a. Ability to dynamically get list of available measurements for a single site
  measurementList <- reactive({
    if(is.null(input$location_siteName)){
      return()
    } else {
      getMeas <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=MeasurementList&Site=",input$location_siteName,sep=""))
      #getMeas <- xmlInternalTreeParse(paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=MeasurementList&Site=Lake Horowhenua at Buoy",sep=""))
      # Need to construct a flat table of:
      #  DataSourceName, MeasurmentName, DataType, Interpolation, Format, Units, Available Start Date, Available End Date
      #Vector of DataSource Names
      dname <- sapply(getNodeSet(getMeas,"//DataSource[TSType='StdSeries']/@Name"),as.character)
      fromDate <- sapply(getNodeSet(getMeas,"//DataSource[TSType='StdSeries']/From"),xmlValue)
      toDate <- sapply(getNodeSet(getMeas,"//DataSource[TSType='StdSeries']/To"),xmlValue)
      dataType  <- sapply(getNodeSet(getMeas,"//DataSource[TSType='StdSeries']/DataType"),xmlValue)
      interp  <- sapply(getNodeSet(getMeas,"//DataSource[TSType='StdSeries']/Interpolation"),xmlValue)
      #build data.frame
      ds <- data.frame(dname,fromDate,toDate,dataType,interp, stringsAsFactors=FALSE)
      ds <- ds[ds$dname!="Campbell Signature",]
      #for each datasource, get the available measurement names
      for(i in 1:nrow(ds)){
        if(i==1){
          dm <- sapply(getNodeSet(getMeas,paste("//DataSource[TSType='StdSeries' and @Name='",ds$dname[i],"']/Measurement/Units/../@Name",sep="")),as.character)
          du <- sapply(getNodeSet(getMeas,paste("//DataSource[TSType='StdSeries' and @Name='",ds$dname[i],"']/Measurement/Units",sep="")),xmlValue)
          df <- data.frame(dm,du, stringsAsFactors=FALSE)
          df$dname        <-ds$dname[i]
          df$interp       <-ds$interp[i]
          df$fromDate     <-ds$fromDate[i]
          df$toDate       <-ds$toDate[i]
          names(df) <- c("MeasurementName","Units","DataSourceName","Interpolation","FromDate","ToDate")
          rm(du,dm)
        } else {
          bb <- sapply(getNodeSet(getMeas,paste("//DataSource[TSType='StdSeries' and @Name='",ds$dname[i],"']/Measurement/Units/../@Name",sep="")),as.character)
          if(length(bb)!=0){
            bu <- sapply(getNodeSet(getMeas,paste("//DataSource[TSType='StdSeries' and @Name='",ds$dname[i],"']/Measurement/Units",sep="")),xmlValue)
            bf <- data.frame(bb,bu, stringsAsFactors=FALSE)
            bf$dname        <-ds$dname[i]
            bf$interp       <-ds$interp[i]
            bf$fromDate     <-ds$fromDate[i]
            bf$toDate       <-ds$toDate[i]
            names(bf) <- c("MeasurementName","Units","DataSourceName","Interpolation","FromDate","ToDate")
            df <- rbind(df,bf)
            rm(bb,bu,bf)
          }
        }
      
      }
      df
      
      # For this constructed dataframe, it will now be possible to extract all the other necessary
      # element and attribute values through one further for-loop
      
    }
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
  
  # Reactive functions defined
  # 4. Ability to dynamically get SiteInfo from Hilltop
  GetSiteInfo <- reactive({
    #Get name of datasource in order to pull measurements correctly
    #Need to use regex to extract text between [ and ]
    #str  <- "Manawatu at Teachers College"
    #str <- input$map_marker_mouseover$id
    str <- input$map_marker_click$id
    url <- paste("http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&request=SiteInfo&Site=",str,sep="")

    getSiteInfo <- xmlInternalTreeParse(url)
    getSiteDF <- xmlToDataFrame(getSiteInfo)
    df <- getSiteDF[2,]

    
  })
  
  

  # Output TVP
  output$SiteMeasurementData <- DT::renderDataTable({
    df<-GetData()
    DT::datatable(df,
                  extensions = "Buttons",
                  options = list(dom = "Bfrtip", 
                                 buttons = list('copy','csv','excel','print'))
                )
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
  
  ## Make the map
  output$map <- renderLeaflet({
    obj<-CollectionList()
    if(is.null(obj)){
      return()
    } else {
    # Configuring map
    map <- leaflet(obj) %>%  
      setView(lng = 175.1, lat = -39.8, zoom = 8) %>%
      addTiles(group="Topo") %>%
      #Add two tile sources
      addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="Default") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      addCircleMarkers(
        ~lon,
        ~lat,
        group = "Stations",
        color = rgb(0.4, 0.4, 0.6),
        fillOpacity = 0.8,
        opacity = 1,
        radius = 11,
        stroke = 0,layerId=~site
      ) %>%
      addLabelOnlyMarkers(
        ~lon,
        ~lat,
        label = ~LastValue,
        group = "Stations",
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          textOnly = TRUE,
          opacity = 1,
          direction = "top",
          style=list(
            'color'='white',
            'font-size' = '10px',
            'position' = 'absolute',
            'top' = '20px')
        ) #%>%
        # Add  the control widget
        #addLayersControl(baseGroups = c("ESRI","Default"))
      ) %>%
      addHeatmap(
        lng = ~lon,
        lat = ~lat,
        intensity = ~numValue,
        radius = 20,
        blur=15,
        max=30,
        minOpacity = 0,
        gradient = "BuPu",
        group = "Heatmap"
      ) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Topo", "Imagery"),
        overlayGroups = c("Stations", "Heatmap"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    #addMarkers(~lon, ~lat, popup = ~as.character(site), label = ~as.character(site))
    #addTiles(options = tileOptions(maxZoom = 28, maxNativeZoom = 19), group = 'OSM')

    map
    }
  })
  
  # Event marker_click
  observeEvent(input$map_marker_click$id, {
    leafletProxy("map") %>% clearPopups()
    pointId <- input$map_marker_click$id
    df <- GetSiteInfo()
    popupText <- paste("<b>",as.character(pointId),"</b><br />",
                       df$Comment[2],
                       "<p style='text-align:center'>[ <a onclick=openTab('Chart')>Chart</a> ]   [ Data ]",
                       tags$script(HTML("
                             var openTab = function(tabName){
                                        $('a', $('.sidebar')).each(function() {
                                          if(this.getAttribute('data-value') == tabName) {
                                            this.click()
                                          };
                                        });
                              }"))
                       )
    cat(pointId,"\n")
    lat = sites[sites$site == pointId, 2]
    lng = sites[sites$site == pointId, 3]
    leafletProxy("map") %>% addPopups(lat = lat, lng = lng, popupText,
                                         layerId = "hoverPopup")
    })

  output$siteinfo <- renderText({
    df <- GetSiteInfo()
    tabHTMLContent <- paste("<table>",
                              "<tr><td width='20%'>LAWA Site ID</td><td>",df$LawaSiteID,"</td></tr>",
                              "<tr><td width='20%'>LAWA Site Name</td><td>",df$LawaSiteName,"</td></tr>",
                              "<tr><td width='20%'>First Synonym</td><td>",df$FirstSynonym,"</td></tr>",
                              "<tr><td width='20%'>Second Synonyn</td><td>",df$SecondSynonym,"</td></tr>",
                              "<tr><td width='20%'>Comment</td><td>",df$Comment,"</td></tr>",
                              "<tr><td width='20%'>Data license</td><td>",df$DataLicense,"</td></tr>",
                            "</table>",sep="")
    
    })


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
    # - Turn off UTC time
    hcGopts <- getOption("highcharter.global")
    hcGopts$useUTC <- FALSE
    options(highcharter.global = hcGopts)
    
    hchart(df, type = chartType,
           hcaes(x = x, y=y)) %>%
      hc_title(text = input$data_siteName) %>% 
      hc_subtitle(text = input$data_measurement) %>% 
      hc_xAxis(type="datetime",
               title = list(text="Date")) %>%
      hc_yAxis(title = list(text=input$data_measurement))%>% 
      hc_exporting(enabled = TRUE)

  })

  output$measurementList <- DT::renderDataTable({
    df <- measurementList()
    DT::datatable(df,
                  extensions = "Buttons",
                  options = list(dom = "Bfrtip", 
                                 buttons = list('copy','csv','excel','print')
                  )
    )
  })
  
  
}

shinyApp(ui, server)
