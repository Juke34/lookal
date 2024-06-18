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
        background-color: #f8f9fa; /* Light background for the header */
        border-bottom: 1px solid #e0e0e0; /* Optional: Add a subtle border below the header */
        margin-bottom: 20px; /* Space below the header */
      }
      
      .logo {
        height: 50px; /* Adjust the logo size */
        margin-left: auto; /* Space between the logo and the title */
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
  # Application title
  #titlePanel("Team Lookal"),
  
  # Header with logo and title
  div(class = "header",
      span(class = "title-text", "Team Lookal"), # Title text
      img(src = "https://nbisweden.github.io/raukr-2024/assets/logos/raukr.png", class = "logo") # Logo image
      
  ),

  # Centralized layout for inputs
  fluidRow(
    column(12,
           div(class = "center-content",
              
               # Text for project description
                div(style = "max-width: 600px; margin: auto; text-align: justify;",
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                ),
           
               # Select between text and PDF
               div(style = "width: 200%; max-width: 300px; margin-top: 10px;",
                   selectInput("input_type", "Choose Input Type:", 
                               choices = c("Text" = "text", "PDF" = "pdf"),
                               selected = "text")
               ),
               
               
              
               # Text area for text input
               conditionalPanel(
                 condition = "input.input_type == 'text'",
                 div(style = "width: 150%; max-width: 600px; margin-top: 10px;",
                     textAreaInput("text", "Your text:", "", rows = 3, resize = "vertical", placeholder = "Please enter your text here!")
                 )
               ),
               
               # File upload for PDF
               conditionalPanel(
                 condition = "input.input_type == 'pdf'",
                 div(style = "width: 80%; max-width: 600px; margin-top: 10px;",
                     fileInput("pdf", "Upload a PDF file:", accept = c(".pdf"))
                 )
               ),
               
               # Button to submit the input
               div(class = "submit-button",
                   actionButton("submit", "Submit")
               )
           )
    )
  ),

  # Map panel to display the map
  fluidRow(
    column(12,
           leafletOutput("map", height = "300px") # Adjust the height as needed
    )
  ),
  
  # Output panel to display text results below the map
  fluidRow(
    column(12,
           h3("Output:"),
           textOutput("displayText")
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
  output$displayText <- renderText({
    reactiveText()
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)


