

library(shiny)
library(gsheet) #import data into R from Google Spreadsheet 
library(leaflet)
library(tidyverse)

data_dir <- "~/R/pSESYNTH"
#data_dir <- "/Users/xavier/pSESYNTH"

# Import metadata from Google Spreadsheet 
url <- c('https://docs.google.com/spreadsheets/d/1eKxnmnvP9lcxW7IOB7Qwi4XK1KRbq6BELw4jD2N4zJ4/edit?usp=sharing')
sites <- gsheet2tbl(url)

# I need to create a vector of names of all data contained in /data to use it as choices in search
#all_files <- paste0("~/R/pSESYNTH/data/", list.files("~/R/pSESYNTH/data/", recursive = TRUE))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("pSESYNTH database"),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "id", label = "Site ID:",
                  choices = sort(sites$id)),
      uiOutput("dynamicui"),
      width = 2,
      
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
        tabPanel("Site", tableOutput("site_info")),
        tabPanel("Data", tableOutput("site_data_df")),
        tabPanel("Time series (by age)", plotOutput("ts_plots_age", width = "100%", height = 600)),
        tabPanel("Time series (by depth)", plotOutput("ts_plots_depth", width = "100%", height = 600)),
        tabPanel("Other", plotOutput("hist_14C", width = "100%", height = 600)))
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # output$dynamicui <- renderUI({
  #   selectInput(inputId = "proxy", label = "Select proxy", choices = readr::read_csv(file = glue::glue("{data_dir}/data/{input$id} .csv"))[input$proxy])
  # })
  
  site_data <- reactive({
    readr::read_csv(file = glue::glue("{data_dir}/sites/{input$id} .csv"))
  })
  
  output$site_info <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/sites/{input$id} .csv"))
  }, striped = TRUE, width="100%")
  
 

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
            "<br>",
            "<strong>Theme:</strong>",
            theme,
            "<br>",
            "<strong>Archive:</strong>",
            archive,
            "<br>",
            "<strong>Proxy:</strong>",
            proxy,
            "<br>",
            "<strong>DOI_paper:</strong>",
            DOI_paper,
            "<br>",
            "<strong>Topic:</strong>",
            topic,
            "<br>",
            "<strong>Indicator(s):</strong>",
            indicator)
          )
  })


  output$site_data_df <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/data/{input$id}.csv")) %>%
    group_by(proxy, variable) %>%
    summarise(Min=min(count), Max=max(count))
  }, striped = TRUE, width="auto")


  proxy_data <- reactive({
    readr::read_csv(file = glue::glue("{data_dir}/data/{input$id}.csv"))
  })
  
  
  # ideally here we could do search and plot desired proxies

  output$ts_plots_age <- renderPlot({
     site_plot_data <- proxy_data() %>%
       filter(!variable=="age") %>%
       group_by(variable) %>%
       filter(count > 50) #put here some sort of criteria abundant taxa

     site_plot_data %>%
       #arrange(proxy) %>%
       ggplot(aes(x = upper_age, y = count, colour= variable)) +
       geom_line(aes(fill = proxy), pch = 21, size = 1) +
       facet_wrap(vars(proxy), scales = "free_y", ncol = 1) +
       theme(legend.position = "top") +
       xlab("Age (cal yrs BP)") +
       theme_bw()
   })
  
  output$ts_plots_depth <- renderPlot({
    site_plot_data <- proxy_data() %>%
      filter(is.na(upper_age)) %>%
      filter(!variable=="age")

    site_plot_data %>%
      #arrange(proxy) %>%
      ggplot(aes(x = depth, y = count, colour= variable)) +
      geom_line(aes(fill = proxy), pch = 21, size = 1) +
      facet_wrap(vars(proxy), scales = "free_y", ncol = 1) +
      theme(legend.position = "top") +
      theme_bw()
  })
  
  output$hist_14C <- renderPlot({
    site_plot_data <- proxy_data() %>%
      filter(str_detect(proxy, "14C")) %>%
      filter(!variable=="age")
    
    site_plot_data %>%
      ggplot(aes(y = proxy)) +
      geom_bar(stat="identity", group=1) +
      theme(legend.position = "top") +
      theme_bw()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
