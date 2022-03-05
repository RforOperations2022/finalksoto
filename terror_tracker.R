library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(fmsb)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)
library(fontawesome)


# Load and clean data ----------------------------------------------

terr_data <- read.csv("./data/2019_Terrorism_Incidents.csv", header=TRUE, stringsAsFactors=FALSE)

icons <- awesomeIconList(
  `Military` = makeAwesomeIcon(icon = "fighter-jet", library = "fa", markerColor = "darkgreen"),
  `Airports & Aircraft` = makeAwesomeIcon(icon = "plane", library = "fa", markerColor = "lightblue"),
  `Business` = makeAwesomeIcon(icon = "briefcase", library = "fa", markerColor = "gray"),
  `Educational Institution` = makeAwesomeIcon(icon = "graduation-cap", library = "fa", markerColor = "red"),
  `Food or Water Supply` = makeAwesomeIcon(icon = "droplet", library = "fa", markerColor = "blue"),
  `Government (Diplomatic)` = makeAwesomeIcon(icon = "globe", library = "fa", markerColor = "lightgray"),
  `Government (General)` = makeAwesomeIcon(icon = "balance-scale", library = "fa", markerColor = "lightgray"),
  `Journalists & Media` = makeAwesomeIcon(icon = "newspaper", library = "fa", markerColor = "pink"),
  `NGO` = makeAwesomeIcon(icon = "atlas", library = "fa", markerColor = "lightgray"),
  `Religious Figures/Institutions` = makeAwesomeIcon(icon = "praying-hands", library = "fa", markerColor = "white"),
  `Private Citizens & Property` = makeAwesomeIcon(icon = "user", library = "fa", markerColor = "lightred"),
  `Police` = makeAwesomeIcon(icon = "bullhorn", library = "fa", markerColor = "cadetblue"),
  `Other` = makeAwesomeIcon(icon = "circle", library = "fa", markerColor = "gray"),
  `Telecommunication` = makeAwesomeIcon(icon = "sattelite-dish", library = "fa", markerColor = "purple"),
  `Maritime` = makeAwesomeIcon(icon = "anchor", library = "fa", markerColor = "cadetblue"),
  `Terrorists/Non-State Militia` = makeAwesomeIcon(icon = "skull-crossbones", library = "fa", markerColor = "black"),
  `Tourists` = makeAwesomeIcon(icon = "map-marked", library = "fa", markerColor = "lightred"),
  `Utilities` = makeAwesomeIcon(icon = "bolt", library = "fa", markerColor = "blue"),
  `Transportation` = makeAwesomeIcon(icon = "train", library = "fa", markerColor = "blue"),
  `Violent Political Party` = makeAwesomeIcon(icon = "book-skull", library = "fa", markerColor = "black"),
  `Unknown` = makeAwesomeIcon(icon = "question-circle", library = "fa", markerColor = "gray")
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Terrorism Dashboard")
                          
                          # Drop down menu with hard coded values ------------------------------

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("2019 Incident Map", icon = icon("fire"), tabName = "t_map"),
    menuItem("Analytics", icon = icon("chart-pie"), tabName = "analysis"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    

 
    
    
    # Inputs: select variables to plot ----------------------------------------------
 
    
    
    conditionalPanel(
      'input.tabs== "t_map"',
      
      
      
      radioButtons("view", label = "View Type",
                   choices = list("Regional" = 'region', "Spotlight a Country" = "country"), 
                   selected = "country"),
      
      conditionalPanel(
        'input.view== "region"',
        
        checkboxGroupInput("region_select", "Region", choices = c(sort(unique(terr_data$region_txt))),
                           selected = c("South America"))
        ,
     
        
        
        selectInput("country_select",
                    "Country:",
                    choices = sort(unique(terr_data$country_txt)),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("United States"))
        
        
     
      ),
      
     conditionalPanel(
        'input.view== "country"',
        
        
        selectInput("country_spotlight",
                    "Country:",
                    choices = sort(unique(terr_data$country_txt)),
                    multiple = FALSE,
                    selectize = TRUE,
                    selected = c("United States")),
     ),
        
        
     
     selectInput("target",
                 "Target:",
                 choices = sort(unique(terr_data$targtype1_txt)),
                 multiple = TRUE,
                 selectize = TRUE,
                 selected = c("Military")),
     
     
      ),
      
        
    
    conditionalPanel(
      'input.tabs== "analysis"',
      
      
      
      selectInput("assailant",
                  "Assailant:",
                  choices = sort(unique(terr_data$gname)),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "Taliban"),
      # selected = c("")),
      
   
        
        
      ),
    
    conditionalPanel(
      'input.tabs== "table"',
      
      
      
      downloadButton('downloadData', 'Download raw data')
      
      
      
      
      
    )
    
   
  )
)


# Dashboard body ----------------------------------------------
body <- dashboardBody(
  
  
  # Map Output
 
  
  tabItems(
  
    
    
    tabItem("t_map",
            
            
            
          
            fluidRow(leafletOutput("leaflet", height = "100vh"))
            
    ),
            
    tabItem("analysis",       # breed explore Page ----------------------------------------------
            
            fluidRow(
              plotlyOutput("plot_assailants"),
              plotlyOutput("terrplot"))
             
             
    ),
    
    tabItem("table",
    
    fluidPage(
      box(title = "Selected Information", DT::dataTableOutput("terr_tab_out"), width = 12))
    
    )
  
  # Other page ----------------------------------------------

  

  )
)


ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output, session) {
  
  values <- reactiveValues(removed = c())
  
  # Basic Map
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-44.0060, 40.7128, 2) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  

  
  
  # Region Filtered data
  terrInputs <- reactive({
    reg_terr <- terr_data
    
    req(input$region_select)
    # Regions
    reg_terr <- subset(terr_data, region_txt %in% input$region_select)
    
    # Target type
    if (length(input$target) > 0) {
      # clearGroup(group = "reg_terr")
      reg_terr <- subset(reg_terr, targtype1_txt %in% input$target & region_txt  %in%  input$region_select )
      
    }
  
    return(reg_terr)
  })
  
  
  
  
  
  
  spotInputs <- reactive({
    spot_terr <- terr_data
    
    req(input$country_spotlight)
    # Regions
    spot_terr <- subset(terr_data, country_txt %in% input$country_spotlight)
    
    # Target type
    if (length(input$target) > 0) {
      # clearGroup(group = "reg_terr")
      spot_terr <- subset(spot_terr, targtype1_txt %in% input$target &  country_txt %in% input$country_spotlight )
      
    }
    
    return(spot_terr)
  })

  
  
  observe({
    reg_terr <- terrInputs()
    spot_terr<- spotInputs()
    
    if(input$view == "region"){
      
      maxLong = max(reg_terr$longitude)
      maxLat = max(reg_terr$latitude)
      minLong = min(reg_terr$longitude)
      minLat = min(reg_terr$latitude)
    
    # Data is greenInf
      leafletProxy("leaflet", data = reg_terr) %>%
        # In this case either lines 92 or 93 will work
        clearMarkers() %>%
         
       # clearGroup(group = "reg_terr") %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = ~icons[targtype1_txt], popup = ~paste0("<b>", summary, "</b>: ", targtype1_txt), group = "reg_terr") %>%
      
      fitBounds(minLong,minLat,maxLong,maxLat)
      
    }
    
    
    if(input$view == "country"){
      
      maxLong = max(spot_terr$longitude)
      maxLat = max(spot_terr$latitude)
      minLong = min(spot_terr$longitude)
      minLat = min(spot_terr$latitude)
    
      # Data is greenInf
      leafletProxy("leaflet", data = spot_terr) %>%
        # In this case either lines 92 or 93 will work
        clearMarkers() %>%
        # clearGroup(group = "reg_terr") %>%
        addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = ~icons[targtype1_txt], popup = ~paste0("<b>", summary, "</b>: ", targtype1_txt), group = "spot_terr") %>% 
      
        #setView(lng = spot_terr$longitude[1], lat = spot_terr$latitude[1], zoom = 6)
        
        fitBounds(minLong,minLat,maxLong,maxLat)
      #setView(
       # lng = spot_terr$longitude[1],
        #lat = spot_terr$longitude[1],
        #zoom = 8)
    }
    
    
    
      })
  
  
  
  

  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    this_shape <- reference_table[match(click$id, reference_table$id),]
    
    leafletProxy("leaflet") %>% 
      setView(
        lng = this_shape$lng,
        lat = this_shape$lat,
        zoom = 8)
  })
  
  

 
  # Reactive data function -------------------------------------------

  
  
  

    
    # Return dataframe ----------------------------------------------

  
  
  
  assailInput <- reactive({
    
      # Homeworld Filter ----------------------------------------------
    
    assail_sub <- subset(terr_data, gname %in% input$assailant)
    
    
    # Return dataframe ----------------------------------------------
    return(assail_sub)
  })

  
  
  
  
  
  
  observe({
  
    if(length(terrInputs()) > 0 ){
      updateSelectInput(session, "assailant",
                        #label = paste("Select", x),
                        choices = terrInputs()$gname)
    }
    
    else{ 
      
      updateSelectInput(session, "assailant",
                        #label = paste("Select", x),
                        choices = terr_data$gname)
    }
  })
  

  
  output$plot_assailants <- renderPlotly({
    datz <- subset(assailInput())
    
   
    
    # Generate Plot ----------------------------------------------
    ggplot(data = datz, aes_string(x = "country_txt", y = "nkill", color = "gname")) + geom_bar(stat = "sum")
  } )
  
  
  output$title_panel <- renderText({
    paste0(assailInput()$gname)
  })
  
  

  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('terrorism data', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(terr_data, con)
    })
  

  

  
  
  output$terrplot <- renderPlotly({

    
    
    ggplot(assailInput(), aes(y = gname, x=targtype1_txt, color =targtype1_txt, size=nkill )) + ggtitle("Casualties by Target")  + geom_point(stat = "sum") +
      #geom_segment( aes(x=Affectionate_Family, xend=Dog_Friendly, y=0, yend=Kid_Friendly), color="skyblue") +

      theme(
        panel.border = element_blank(),
      )
    
  })
  
  
  


 
  
  output$terr_tab_out <- DT::renderDataTable({
    
    tab_dat <- subset(terr_data, select = c(country_txt, targtype1_txt, nkill, gname, summary))
    
    DT::datatable(tab_dat, escape = FALSE) # HERE
  })
  
  # Picture test----------------------------------------------
  

  

}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)