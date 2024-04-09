library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)      
library(leaflet) 
library(lubridate)
library(DT)

# Load the dataset
load("C:/Users/NADER/Downloads/AirBnB (1) (1).Rdata")


# Data preparation and transformations
L$price <- as.numeric(gsub("[$,]", "", L$price))

# Ensure 'amenities' and 'description' are treated as character vectors
L$amenities <- as.character(L$amenities)


# Then proceed with the original transformations
L$number_amenities <- sapply(strsplit(L$amenities, "[{}\",]"), function(x) length(x[x != ""]))


# Handle missing values
L$number_amenities[is.na(L$number_amenities)] <- 0



# Convert dates 
R$date <- as.Date(as.character(R$date))

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "AirBnB Analysis", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Price vs Features", tabName = "price_features", icon = icon("chart-line")),
      menuItem("Apartments per Owner", tabName = "apartments_owner", icon = icon("building")),
      menuItem("Price per Quarter", tabName = "price_quarter", icon = icon("calendar-alt")),
      menuItem("Visit Frequency", tabName = "visit_frequency", icon = icon("clock")),
      menuItem("Interactive Map", tabName = "interactive_map", icon = icon("map-marked-alt"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        
        .content-wrapper {
          background-color: #FFF0F0 !important; 
        }
        
        
        .main-header .navbar, .main-header .logo {
          background-color: #FF5A5F !important; 
        }
        
        /* Sidebar styling */
        .main-sidebar {
          background-color: #D50000 !important; 
        }
        
        
        .sidebar-menu > li > a {
          color: #FFFFFF !important; 
        }
        
        
        .sidebar-menu > li:hover > a, .sidebar-menu > li.active > a {
          background-color: #FF5A5F !important;
          color: #FFFFFF !important;
        }
        
        
        .sidebar-menu .header {
          color: #FFD3D3 !important; 
        }
        
        
        .box-body {
          background-color: #FFEBEB !important; 
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "price_features", fluidRow(titlePanel("Dynamic Correlation Analysis: Price vs Features"), selectInput("featureSelect", "Select Feature for Analysis:", choices = c("Number of Amenities" = "number_amenities", "Accommodates" = "accommodates", "Bedrooms" = "bedrooms", "Bathrooms" = "bathrooms"), selected = "accommodates"), plotlyOutput("scatterPlot"))),
      tabItem(tabName = "apartments_owner", fluidRow(titlePanel("Distribution of Apartments per Owner"), sliderInput("apartmentFilter", "Number of Apartments:", min = 1, max = 100, value = c(1, 5)), DTOutput("ownerTable"))),
      tabItem(tabName = "price_quarter", fluidRow(titlePanel("Average Renting Price per City Quarter"), selectInput("quarterSelect", "Select Quarters:", choices = unique(L$neighbourhood_cleansed), multiple = TRUE), plotlyOutput("avgPricePlot"))),
      tabItem(tabName = "visit_frequency", fluidRow(titlePanel("Visit Frequency of Different Quarters Over Time"), dateRangeInput("dateRange", "Select Date Range:", start = min(R$date), end = max(R$date)), selectInput("quarterFrequencySelect", "Select Quarters:", choices = unique(L$neighbourhood_cleansed), multiple = TRUE), plotlyOutput("visitFrequencyPlot"))),
      tabItem(tabName = "interactive_map", fluidRow(titlePanel("Airbnb Listings on Interactive Map"), leafletOutput("airbnbMap")))
    )
  )
)

# Server logic
server <- function(input, output, session) {
  output$scatterPlot <- renderPlotly({
    # Generate scatter plot based on selected feature
    feature_selected <- input$featureSelect
    plot_data <- L %>% select(price, !!sym(feature_selected)) %>% na.omit()
    p <- ggplot(plot_data, aes_string(x = feature_selected, y = "price")) +
      geom_point(aes(color = price), alpha = 0.5) +
      labs(x = feature_selected, y = "Price in Euros") +
      theme_minimal()
    ggplotly(p)
  })
  # Removed the correlation plot generation section
  apartments_per_owner <- reactive({
    data <- L %>%
      group_by(host_id, host_name) %>%
      summarise(number_of_apartments = n_distinct(id), .groups = 'drop') %>%
      arrange(desc(number_of_apartments))
    data <- data %>% 
      filter(number_of_apartments >= input$apartmentFilter[1] & 
               number_of_apartments <= input$apartmentFilter[2])
    data
  })
  output$ownerTable <- renderDataTable({
    apartments_per_owner()
  }, options = list(pageLength = 10))
  output$avgPricePlot <- renderPlotly({
    # Generate average price per quarter plot
    filtered_data <- if (length(input$quarterSelect) > 0) {
      L %>% filter(neighbourhood_cleansed %in% input$quarterSelect)
    } else {
      L
    }
    avg_prices <- filtered_data %>%
      group_by(neighbourhood_cleansed) %>%
      summarise(Average_Price = mean(price, na.rm = TRUE)) %>%
      arrange(desc(Average_Price))
    p <- ggplot(avg_prices, aes(x = reorder(neighbourhood_cleansed, Average_Price), y = Average_Price)) +
      geom_bar(stat = "identity", fill = "lightpink") +
      labs(title = "Average Renting Price per City Quarter", x = "City Quarter", y = "Average Price (€)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$visitFrequencyPlot <- renderPlotly({
    # Generate visit frequency over time plot
    quarters_data <- if (length(input$quarterFrequencySelect) > 0) {
      L %>% filter(neighbourhood_cleansed %in% input$quarterFrequencySelect)
    } else {
      L
    }
    merged_data <- merge(quarters_data, R, by.x = "id", by.y = "listing_id")
    date_filtered_data <- merged_data %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    visit_frequency_data <- date_filtered_data %>%
      group_by(neighbourhood_cleansed, month = floor_date(date, "month")) %>%
      summarise(Visits = n()) %>%
      arrange(neighbourhood_cleansed, month)
    p <- ggplot(visit_frequency_data, aes(x = month, y = Visits, color = neighbourhood_cleansed, group = neighbourhood_cleansed)) +
      geom_line() + geom_point() +
      labs(title = "Visit Frequency Over Time by City Quarter", x = "Month", y = "Number of Visits") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$airbnbMap <- renderLeaflet({
    # Generate interactive map
    data <- L %>% filter(!is.na(latitude) & !is.na(longitude)) %>%
      mutate(price = as.numeric(gsub("[$,]", "", price)))
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~paste("Price: $", price, "<br>", "Neighbourhood: ", neighbourhood_cleansed, "<br>", "Accommodates: ", accommodates), clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui, server)