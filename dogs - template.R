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

dog_data <- read.csv("./data/dog_breed_data.csv", header=TRUE, stringsAsFactors=FALSE)
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
        
        selectInput("assailant",
                    "Assailant:",
                    choices = sort(unique(terr_data$gname)),
                    multiple = TRUE,
                    selectize = TRUE),
                   # selected = c("")),
        
        
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
      'input.tabs== "other"',
      
      radioButtons(
        "advanced",
        "Filtering:",
        c("Show" = "basic",
          "Hide" = "adv"),
        selected = "basic",
        inline = TRUE)
      ,
      
      conditionalPanel(
        'input.advanced== "basic"',
        
        sliderInput("weight",
                    "Average Weight (lbs):",
                    min = min(dog_data$Avg_Weight, na.rm = T),
                    max = max(dog_data$Avg_Weight, na.rm = T),
                    value = c(min(dog_data$Avg_Weight, na.rm = T), max(dog_data$Avg_Weight, na.rm = T)),
                    step = 5),
        
        sliderInput("adaptability",
                    "Overall Adaptability:",
                    min = min(dog_data$Adaptability, na.rm = T),
                    max = max(dog_data$Adaptability, na.rm = T),
                    value = c(min(dog_data$Adaptability, na.rm = T), max(dog_data$Adaptability, na.rm = T)),
                    step = .2),
        
        sliderInput("friendliness",
                    "Overall Friendliness:",
                    min = min(dog_data$Overall_Friendliness, na.rm = T),
                    max = max(dog_data$Overall_Friendliness, na.rm = T),
                    value = c(min(dog_data$Overall_Friendliness, na.rm = T), max(dog_data$Overall_Friendliness, na.rm = T)),
                    step = .2),
        
        sliderInput("health_grooming",
                    "Health and Grooming Needs:",
                    min = min(dog_data$Health_Grooming_Needs, na.rm = T),
                    max = max(dog_data$Health_Grooming_Needs, na.rm = T),
                    value = c(min(dog_data$Health_Grooming_Needs, na.rm = T), max(dog_data$Health_Grooming_Needs, na.rm = T)),
                    step = .2),
        
        sliderInput("trainability",
                    "Trainability:",
                    min = min(dog_data$Trainability, na.rm = T),
                    max = max(dog_data$Trainability, na.rm = T),
                    value = c(min(dog_data$Trainability, na.rm = T), max(dog_data$Trainability, na.rm = T)),
                    step = .2),
        
        sliderInput("physical",
                    "Physical Needs:",
                    min = min(dog_data$Physical_Needs, na.rm = T),
                    max = max(dog_data$Physical_Needs, na.rm = T),
                    value = c(min(dog_data$Physical_Needs, na.rm = T), max(dog_data$Physical_Needs, na.rm = T)),
                    step = .2)
        
        )
        
        
        
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
            
    tabItem("doggy",       # breed explore Page ----------------------------------------------
            
            fluidRow(
              titlePanel(textOutput("title_panel")),
              htmlOutput("picture"),
              fluidRow(
                infoBoxOutput("info")
              ),
              
              tabBox(title = "", width = 12,
                     
                     tabPanel("Trainability", 
                              
                              fluidRow(
                                valueBoxOutput("train_score")
                              ),
                              
                              
                              plotlyOutput("train")),
                     
                     tabPanel("Friendliness",
                              
                              fluidRow(
                                valueBoxOutput("friend_score")
                              ),
                              
                              
                              plotlyOutput("friendly")),
                     
                     
                     tabPanel("Grooming and Care",
                              fluidRow(
                                valueBoxOutput("health_score")
                              ),
                              
                              plotlyOutput("health")),
                     
                     tabPanel("Adaptability",
                              
                              fluidRow(
                                valueBoxOutput("adapt_score")
                              ),
                              
                              
                              plotlyOutput("adapt")),
                     
                     tabPanel("Physical Activity",
                              
                              fluidRow(
                                valueBoxOutput("physical_score")
                              ),
                              
                              plotlyOutput("phys"))
              )   
            )
    ),
  
  
  # Other page ----------------------------------------------
  
  tabItem("other",
          
          # Input and Value Boxes ----------------------------------------------
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Compare Dogs",
                   width = 13,
                   tabPanel("Visualize Comparisons",
                            
                            selectInput("x",
                                        "Plot on x-axis:",
                                        choices = sort(colnames(dog_data)),
                                        multiple = FALSE,
                                        selectize = TRUE,
                                        selected = c("Adaptability")),
                   
                   
                           selectInput("y",
                                     "Plot on y-axis:",
                                     choices = sort(colnames(dog_data)),
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     selected = c("Adaptability")),
         
                            plotlyOutput("plot_breeds")),
                   
                   tabPanel("Browse based on Criteria",
                            
                            fluidPage(
                              box(title = "Selected Dog Stats", DT::dataTableOutput("dog_tab_out"), width = 12))
                        
                            
                            
                        )
                   )
          )
  )
  

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
  beInput <- reactive({
    explore_dogs <- dog_data %>%
      
      # Slider Filter ----------------------------------------------
    filter(Avg_Weight >= input$weight[1] & Avg_Weight <= input$weight[2]
           & Adaptability >= input$adaptability[1] & Adaptability <= input$adaptability[2]
           & Overall_Friendliness >= input$friendliness[1] & Overall_Friendliness <= input$friendliness[2]
           & Health_Grooming_Needs >= input$health_grooming[1] & Health_Grooming_Needs <= input$health_grooming[2]
           & Trainability >= input$trainability[1] & Trainability <= input$trainability[2]
           & Physical_Needs >= input$physical[1] & Physical_Needs<= input$physical[2]
           )
    
    # Return dataframe ----------------------------------------------
    return(explore_dogs)
  })
  
  
  
  
  sub_assail <- reactive({
    
    # Homeworld Filter ----------------------------------------------
    if (length(input$size_select) > 0 ) {
      checked  <- subset(dog_data, Dog_Size %in% input$size_select)
      
      return(checked)
    }
    
    else{
      return(dog_data)
      
    }
    
    # Return dataframe ----------------------------------------------
  })
  
  
  
  ddInput <- reactive({
    
      # Homeworld Filter ----------------------------------------------
    if (length(input$breeds) > 0 ) {
      dog_dat_sub <- subset(dog_data, Breed %in% input$breeds)
    }
    
    # Return dataframe ----------------------------------------------
    return(dog_dat_sub)
  })

  
  
  personality <- reactive({
    
    # personality Filter ----------------------------------------------
    if (length(ddInput()) > 0 ) {
      dog_pers_traits <- subset(ddInput(), select = c("Affectionate_Family", "Kid_Friendly", 
                                                      "Dog_Friendly", "Friendly_To_Strangers"))
      pers_traits = rbind(pers_max_min, dog_pers_traits)
      
      return(pers_traits)
       
    }

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
  

  
  output$plot_breeds <- renderPlotly({
    datz <- subset(beInput())
    
    x=input$x
    y=input$y
    
    # Generate Plot ----------------------------------------------
    ggplot(data = datz, aes_string(x = input$x, y = input$y, color = "Breed")) + geom_point(stat = "identity")
  } )
  
  
  output$title_panel <- renderText({
    paste0(ddInput()$Breed)
  })
  
  
  output$friendly <- renderPlotly({
    
    dat <- subset(ddInput(), select = c( "Affectionate_Family", "Kid_Friendly", 
                                        "Dog_Friendly", "Friendly_To_Strangers"))
    
    lol_dat=melt(dat)
    
    ggplot(lol_dat, aes(y = variable, x=value)) + ggtitle("Friendliness and Disposition") +
      geom_segment( aes(x=value, xend=value, y=variable, yend=variable), color="red") +
      #geom_segment( aes(x=Affectionate_Family, xend=Dog_Friendly, y=0, yend=Kid_Friendly), color="skyblue") +
      geom_point( color="red", size=4, alpha=0.6) +
      scale_x_continuous(limits = c(0, 5))+
      theme_light() +
      ylab("") +
      theme(
        panel.border = element_blank(),
      )
    
  })
  
  
  
  output$train <- renderPlotly({
    
    train_data <- subset(ddInput(), select = c( "Intelligence", "Easy_To_Train", "Mouthiness", 
                                         "Prey_Drive", "Bark_Tendency","Wanderlust"))
    
    train_dat=melt(train_data)
    
    ggplot(train_dat, aes(y = variable, x=value)) + ggtitle("Intelligence and Trainability") +
      geom_segment( aes(x=value, xend=value, y=variable, yend=variable), color="orange") +
      #geom_segment( aes(x=Affectionate_Family, xend=Dog_Friendly, y=0, yend=Kid_Friendly), color="skyblue") +
      geom_point( color="orange", size=4, alpha=0.6) +
      scale_x_continuous(limits = c(0, 5))+
      theme_light() +
      ylab("") +
      theme(
        panel.border = element_blank(),
      )
    
  })
  
  
  
  output$adapt <- renderPlotly({
    
    adapt_data <- subset(ddInput(), select = c( "Hot_Weather", "Cold_Weather", "Tolerates_Being_Alone", 
                                                "Sensitivity", "Novice_Owners","Apartment_Living"))
    
    ad_dat=melt(adapt_data)
    
    ggplot(ad_dat, aes(y = variable, x=value)) + ggtitle("Adaptability and Living Environment") +
      geom_segment( aes(x=value, xend=value, y=variable, yend=variable), color="green") +
      #geom_segment( aes(x=Affectionate_Family, xend=Dog_Friendly, y=0, yend=Kid_Friendly), color="skyblue") +
      geom_point( color="green", size=4, alpha=0.6) +
      scale_x_continuous(limits = c(0, 5))+
      theme_light() +
      ylab("") +
      theme(
        panel.border = element_blank(),
      )
    
  })
  
  
  output$health <- renderPlotly({

    
    health_data <- subset(ddInput(), select = c(  "Shedding", 
                                                 "Drooling", "Easy_To_Groom","Health", "Weight_Gain"))
    
    hel_dat=melt(health_data)
    
    ggplot(hel_dat, aes(y = variable, x=value)) + ggtitle("Grooming and Care") +
      geom_segment( aes(x=value, xend=value, y=variable, yend=variable), color="skyblue") +
      #geom_segment( aes(x=Affectionate_Family, xend=Dog_Friendly, y=0, yend=Kid_Friendly), color="skyblue") +
      geom_point( color="skyblue", size=4, alpha=0.6) +
      scale_x_continuous(limits = c(0, 5))+
      theme_light() +
      ylab("") +
      theme(
        panel.border = element_blank(),
      )
    
  })
  
  
  
  output$phys<- renderPlotly({
    
    
    phys_data <- subset(ddInput(), select = c("Energy_Level", 
                                                  "Intensity", "Easy_To_Groom","Exercise", "Playfulness"))
    
    p_dat=melt(phys_data)
    
    ggplot(p_dat, aes(y = variable, x=value)) + ggtitle("Exercise, Energy, and Activity Level") +
      geom_segment( aes(x=value, xend=value, y=variable, yend=variable), color="purple") +
      #geom_segment( aes(x=Affectionate_Family, xend=Dog_Friendly, y=0, yend=Kid_Friendly), color="skyblue") +
      geom_point( color="purple", size=4, alpha=0.6) +
      scale_x_continuous(limits = c(0, 5))+
      theme_light() +
      ylab("") +
      theme(
        panel.border = element_blank(),
      )
    
  })

  
  output$dogtable <- DT::renderDataTable({
    subset(ddInput(), select = c(Breed, Image, Dog_Size))
  })
  
  output$dog_tab_out <- DT::renderDataTable({
    browse_dogs <- subset(beInput())
    browse_dogs$Picture = paste('<img src="', browse_dogs$Image, '" width="100" height="68" style="float: left; padding: 2px 2px 2px 2px;"', '>')
    
    tab_dat <- subset(browse_dogs, select = c(Breed, Picture, Dog_Size, Dog_Breed_Group))
    
    DT::datatable(tab_dat, escape = FALSE) # HERE
  })
  
  # Picture test----------------------------------------------
  
  output$picture<-renderText({c('<img src="',ddInput()$Image, '" width="400" height="250" style="float: left; padding: 5px 10px 10px 15px;"', '>' )})
  
  
  # Mass mean info box ----------------------------------------------
  output$info<- renderInfoBox({
    ddx <- ddInput()
    num <- ddx$Health
  
    
    infoBox("Health Score", value = num , subtitle = paste("Life expectancy: ", ddx$Life_Span), icon = icon("comment-medical"), color = "navy")
  })
  
  # Health Score value box ----------------------------------------------
  output$health_score <- renderValueBox({
    dd <- ddInput()
    num <- dd$Health_Grooming_Needs
    
    valueBox(subtitle = "Grooming and Care Score", value = num, icon = icon("sort-numeric-asc"), color = "aqua")
  })
  
  output$train_score <- renderValueBox({
    dd <- ddInput()
    num <- dd$Trainability
    
    valueBox(subtitle = "Trainability Score", value = num, icon = icon("sort-numeric-asc"), color = "orange")
  })
  
  output$friend_score <- renderValueBox({
    dd <- ddInput()
    num <- dd$Overall_Friendliness
    
    valueBox(subtitle = "Friendliness Score", value = num, icon = icon("sort-numeric-asc"), color = "red")
  })
  
  
  output$adapt_score <- renderValueBox({
    dd <- ddInput()
    num <- dd$Adaptability
    
    valueBox(subtitle = "Adaptability Score", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
  
  output$physical_score <- renderValueBox({
    dd <- ddInput()
    num <- dd$Physical_Needs
    
    valueBox(subtitle = "Activity Score", value = num, icon = icon("sort-numeric-asc"), color = "purple")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)