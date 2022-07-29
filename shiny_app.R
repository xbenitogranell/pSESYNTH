

library(shiny)
library(gsheet) #import data into R from Google Spreadsheet 
library(leaflet)
library(tidyverse)

data_dir <- "~/R/pSESYNTH"

# Import metadata from Google Spreadsheet 
url <- c('https://docs.google.com/spreadsheets/d/1eKxnmnvP9lcxW7IOB7Qwi4XK1KRbq6BELw4jD2N4zJ4/edit?usp=sharing')
sites <- gsheet2tbl(url)

# I need to create a vector of names of all data contained in /data to use it as choices in search
all_files <- paste0("~/R/pSESYNTH/data/", list.files("~/R/pSESYNTH/data/", recursive = TRUE))


## I need to put the metadata into a global header
pollen_subset <- dplyr::filter(pollen_data, Taxon %in% good_taxa) %>% 
  select(SiteName, Latitude, Longitude, Age, Taxon, Pct) %>% 
  mutate_each(funs(as.numeric), Latitude, Longitude, Age, Pct) %>% na.omit

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("pSESYNTH database"),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      selectInput("id",
                  "Site ID:",
                  choices = sort(sites$id)),
      uiOutput("dynamicui"),
      width = 5,
      
      # sliderInput(inputID = "time", label = "Years before present:",
      #             min = -50,
      #             max = 15000,
      #             value = 0,
      #             step = 500),
      
      # tags$div(title = "This input has a tool tip",
      #          selectInput(inputId = "taxon", 
      #                      label = "Taxon of Interest", 
      #                      choices = sort(unique(global_data$Taxon)))),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map",width="100%",height=600)),
        tabPanel("site", tableOutput("site_info")),
        tabPanel("data", DT::dataTableOutput("site_data_df")),
        tabPanel("plots", plotOutput("ts_plots", height = "2000px")))
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  site_data <- reactive({
    readr::read_csv(file = glue::glue("{data_dir}/sites/{input$id} .csv"))
  })
  
  
  output$dynamicui <- renderUI({
    selectInput(inputId = "proxy", label = "Select proxy", choices =
                  site_data$proxy %in% input$id)
      })
  
  
  output$site_info <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/sites/{input$id} .csv"))
  }, striped = TRUE, width="auto")
  
  

  
  # Create map
  output$map <- renderLeaflet({
    leaflet(site_data()) %>%
      addProviderTiles("Esri.WorldImagery") %>% 
      setView(lng = -12.56, lat = 2.10, zoom = 1.2) %>%
      addCircleMarkers(
                lng = ~longitude, # note the tildes before values, required
                lat = ~latitude,
                stroke = TRUE,
                color = "black",
                fillOpacity = 0.7,
                fillColor = "orange",
          popup = ~paste(
            theme,
            "<br>",
            "<strong>Archive:</strong>",
            archive,
            "<br>",
            "<strong>Proxy:</strong>",
            proxy,
            "<br>",
            "<strong>Indicator:</strong>",
            indicator
            )
          )
  })


  output$site_data_df <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/data/{input$id}.csv"))
  }, striped = TRUE, width="auto")


  
  proxy_data <- reactive({
    readr::read_csv(file = glue::glue("{data_dir}/data/{input$id}.csv"))
  })
  
  # ideally here we could do search and plot desired proxies

  output$ts_plots <- renderPlot({
     site_plot_data <- proxy_data() 

     site_plot_data %>%
       #arrange(proxy) %>%
       ggplot(aes(x = upper_age, y = count)) +
       geom_line(aes(fill = proxy), pch = 21, size = 2) +
       facet_wrap(vars(proxy), scales = "free_y", ncol = 1) +
       theme(legend.position = "none")
   })

}

# Run the application 
shinyApp(ui = ui, server = server)
