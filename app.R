#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(pdftools)
library(leaflet)
library(jsonlite)
library(sf)
library(DT)

source("process_input.R")


# Define UI for application
ui <- fluidPage(

  # Custom CSS to centralize and make inputs bigger
  tags$head(
    tags$style(HTML("
      .header {
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 10px 0;
        background-color: #f8f9fa;
        border-bottom: 1px solid #e0e0e0; 
        margin-bottom: 20px; 
      }
      
      .logo {
        height: 50px;
        margin-left: auto; 
      }
      
      .center-content {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
        min-height: 60vh;
      }
      .wide-input {
        width: 80%;
        max-width: 600px;
        margin-top: 10px;
      }
      .submit-button {
        margin-top: 20px;
      }
      #map {
        height: 600px;
        margin-top: 20px;
      }

      
    "))
  ),

  # Header with logo and title
  div(class = "header",
      span(class = "title-text", "Team Lookal"), # Title text
      img(src = "https://nbisweden.github.io/raukr-2024/assets/logos/raukr.png", class = "logo") # Logo image
  ),
  
  # Sidebar layout for potential future navigation or controls
  sidebarLayout(
    sidebarPanel(width = 3,
      # Placeholder for sidebar content
      p("This is the sidebar content: ....")
    ),
    
    # Main panel for the primary content
    mainPanel(width= 9,
      div(style = "max-width: 800px; margin: auto;",
          # Text for project description
          div(style = "text-align: justify; margin-bottom: 20px;",
              p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
          ),
          
          # Select between text and PDF
          div(style = "width: 200px; margin-bottom: 10px;",
              selectInput("input_type", "Choose Input Type:", 
                          choices = c("Text" = "text", "PDF" = "pdf"),
                          selected = "text")
          ),
          
          # Conditional panel for text input
          conditionalPanel(
            condition = "input.input_type == 'text'",
            div(style = "width: 200%; max-width: 800px; margin-bottom: 10px;",
                textAreaInput("text", "Your text:", "", rows = 3, resize = "vertical", placeholder = "Please enter your text here!")
            )
          ),
          
          # Conditional panel for PDF upload
          conditionalPanel(
            condition = "input.input_type == 'pdf'",
            div(style = "width: 100%; max-width: 600px; margin-bottom: 10px;",
                fileInput("pdf", "Upload a PDF file:", accept = c(".pdf"))
            )
          ),
          
          # Button to submit the input
          div(class = "submit-button",
              actionButton("submit", "Submit")
          ),
          
          # Map output
          leafletOutput("map"),
          
          # Text output
          h3("Output:"),
          textOutput("displayText"),
          
          # Data frame output
          h3("Your text is mainly connected to the following languages:"),
          DT::dataTableOutput("data_table")  # Placeholder for the data table
      )
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  # Reactive value to store the uploaded or entered text
  reactiveText <- reactiveVal("")
  
  # Observe the submit button
  observeEvent(input$submit, {
    if (input$input_type == "pdf" && !is.null(input$pdf)) {
      # Read the uploaded PDF
      pdf_text <- pdf_text(input$pdf$datapath)
      # Update reactiveText with the content of the PDF
      reactiveText(paste(pdf_text, collapse = "\n"))
    } else if (input$input_type == "text") {
      # Update reactiveText with the entered text
      reactiveText(input$text)
    } else {
      reactiveText("No input provided.")
    }
  })
  
  # Output the processed text
  output$displayText <- renderPrint({
    process_text(reactiveText())
  })
  
  # Load GeoJSON data from URL
  geojson_url <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  geojson_data <- st_read(geojson_url) # Read GeoJSON directly from the URL
  
  
  # Render the map with the GeoJSON data
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = geojson_data, fill = TRUE, fillColor = "red", color = "white")
  })
  
  #Render the sample table
  output$data_table <- DT::renderDataTable({
    datatable(sample_df, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

