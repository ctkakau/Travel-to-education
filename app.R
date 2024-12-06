#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(osmdata)
library(mapview)
library(leaflet)
library(leaflet.providers)
library(bslib)
library(tmap)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(sf)

# big files that hold all the data, 
# update location if necessary

# function to read in shapefiles
convert_shp_leaf <- function(path, keepCols= c(2, 6)){
  df <- st_read(path) %>% 
    select(all_of(keepCols)) %>%
    st_transform("+proj=longlat +datum = WGS84")
  df$geometry <- st_geometry(df)+c(360, 90) %% c(360) - c(0, 90)
  return(df)
}

if (!exists("census")){
  print('loading data, this may take a minute or two')
  # from here once the csv file has been created
  census <- read.csv('census_2018_2023.csv', row.names = 'X')
  #  create object to hold the TS (total stated) values
  totals <- census %>%
    # filter in  the TS (Total Stated) rows
    dplyr::filter(stringr::str_ends(Var_Code,"(?i)TS"))
  # create a TS (total stated) column to hold the TS for each SA1, year, and topic
  census <- census %>%
    # filter in  the Total and Total Stated rows
    dplyr::filter(!stringr::str_ends(Var_Code,"(?i)TS|Total")) %>%
    # join census and totals table
    left_join(y = totals[,c("SA1", "shortCode", "Year", "Value")],
              by = c("SA1", "Year", "shortCode"),
              relationship = 'many-to-one') %>%
    # rename to useful column names
    rename(c(Value = "Value.x", TS = "Value.y"))
  
  
  
  # shapefile locations
  shp_folder <-  c("C:/Users/chad/OneDrive - Victoria University of Wellington - STUDENT/DATA581 - waka kotahi/StatsNZShapeFiles/")
  shp_sa1 <- paste0(shp_folder, "statistical-area-1-2023-generalised.shp", sep = "")
  shp_sa2 <- paste0(shp_folder, "statistical-area-2-2023-generalised.shp", sep = "")
  shp_ta <- paste0(shp_folder, "territorial-authority-2023-clipped-generalised.shp", sep = "")
  shp_rc <- paste0(shp_folder, "regional-council-2023-clipped-generalised.shp", sep = "")
  

  # # read in shapefiles
  # regional councils
  rc <- convert_shp_leaf(shp_rc)
  names(rc)[1] <- "Regional.Council"
  # territorial authorities
  ta <- convert_shp_leaf(shp_ta)
  names(ta)[1] <- "Territorial.Authority"  
  # suburbs
  sa2 <- convert_shp_leaf(shp_sa2)
  names(sa2)[1] <- "SA2_chx)"
  sa1 <- convert_shp_leaf(shp_sa1, c(1, 6))
  names(sa1)[1] <- "SA1"
  sa1$SA1 <- as.integer(sa1$SA1)
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How people get to their place of education"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          selectizeInput(inputId = 'selected_topic',
                      label = "Census Topic",
                      choices = unique(census$shortCode)),
          selectizeInput(inputId = 'selected_var',
                      label = "Variable",
                      choices = NULL),
          selectizeInput(inputId = 'selected_reg',
                      "Region",
                      choices = unique(census$Regional.Council)),
          selectizeInput(inputId = 'selected_ta',
                      "District",
                      choices = NULL),
          selectizeInput(inputId = 'selected_town',
                      "Town or City",
                      choices = NULL),
          selectizeInput(inputId = 'selected_sub',
                      label = "Suburb",
                      choices = NULL)
        ),

        
        
        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
           leafletOutput('leafmap')
        )
    )
)


#### SERVER FUNCTION
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    # reactively update Territorial Authority selector
    var_subset <- reactive({
      req(input$selected_topic)
      filter(census, shortCode %in% input$selected_topic)
    })
    observeEvent(var_subset(), {
      updateSelectizeInput(session,
                           "selected_var",
                           choices = var_subset()$Variable,
                           server = TRUE)
    }) 
    
    
    
    
    # reactively update Territorial Authority selector
    ta_subset <- reactive({
      req(input$selected_reg)
      filter(census, Regional.Council %in% input$selected_reg)
      })
    observeEvent(ta_subset(), {
      updateSelectizeInput(session,
                           "selected_ta",
                           choices = ta_subset()$Territorial.Authority,
                           server = TRUE)
    })
    
    
    # reactively update town selector
    town_subset <- reactive({
      req(input$selected_ta)
      filter(census, Territorial.Authority %in% input$selected_ta)
    })
    observeEvent(town_subset(), {
      updateSelectizeInput(session,
                           "selected_town",
                           choices = town_subset()$SA3_chx,
                           server = TRUE)
    })
    
    # reactively update suburb selector
    suburb_subset <- reactive({
      req(input$selected_town)
      filter(census, SA3_chx %in% input$selected_town)
    })
    observeEvent(suburb_subset(), {
      updateSelectizeInput(session,
                           "selected_sub",
                           choices = suburb_subset()$SA2_chx,
                           server = TRUE)
    })
    
    
    # filter census data based on region selected
    df <- reactive({
      req(inpu$selected_reg)
      
      regional_dataset <- census %>%
        filter(Regional.Council %in% input$selected_reg)
      
    })
    
    # generate output leaflet plot
    output$leafmap <- renderLeaflet({
      
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = sa2, 
                    weight = 4,
                    color = 'black') %>%
        addPolygons(data = ta,
                    weight = 3,
                    color = 'blue') %>%
        addPolygons(data = rc, 
                    weight = 1,
                    color = 'green'
                    ) %>%
        setView(lng = 175, 
                lat=-40, zoom=6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
