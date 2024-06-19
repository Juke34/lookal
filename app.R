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
library(dplyr)
library(stringr)
library(purrr)
library(arrow)
library(furrr)
library(networkD3)
library(shinycssloaders)

# Increase the future.globals.maxSize limit
#options(future.globals.maxSize = 1024 * 1024 * 1024)  # Set limit to 1 GB

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
      .footer {
        text-align: center;
        margin-top: 20px;
        color: #6c757d;
        font-size: 10px; /* Adjust font size here */
        line-height: 1.2; /* Adjust line height if needed */
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
      
      .wide-input-container {
        width: 100%;
        margin-top: 10px;
      }
      
      .submit-button {
        margin-top: 20px;
      }
      
      #map {
        height: 600px;
        margin-top: 20px;
      }
      
      .wide-input-container .form-group {
        width: 100%;
      }
      
      #text {
        width: 100%;
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
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Input Data",
              fluidPage(
                column(width = 12,
                       div(style = "max-width: 2000px; margin: auto;",
                           # Text for project description
                           div(style = "text-align: justify; margin-bottom: 20px;",
                               p("Welcome to Lookal, an interactive Shiny application designed to transform your textual input into insightful visualizations. This app offers a comprehensive platform to analyze and explore connections within your text through various dynamic visual tools.")
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
                              div(
                                 textAreaInput("text", "Your text:", "", width = "100%", rows = 3, resize = "vertical", placeholder = "Please enter your text here!")
                            
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
                           
                          
                           # Text output for user's raw input
                           h3("Your Input:"),
                           textOutput("user_input"),
                           
                           # Processed text output (if applicable)
                           #h3("Output:"),
                           #textOutput("reactive_text"),
                           
                           textOutput("display_text"),
                           

                           
                      )
              )
            )),
          
    
    tabPanel("Output World Map",
             # Map output
             leafletOutput("map") %>% withSpinner(type = 1, color = "#0dc5c1", size = 1.5),
             
             # Data frame output
             h5("Your text is mainly connected to the following countries:"),
             DT::dataTableOutput("data_table"), # Placeholder for the data table
             
             #Download Button
             downloadButton("button_download","Download Table"),
    ),
    
    tabPanel("Sankey Plot",
            fluidPage(
               # Sankey plot output
               h3("Language Connections Sankey Plot"),
               sankeyNetworkOutput("sankey_plot") %>% withSpinner(type = 1, color = "#0dc5c1", size = 1.5),
            ),
            
              
    )
    ),
    # Footer section
    div(class = "footer",
        p("© 2024 Team Lookal. (Authors: Ine Bonthius, Jacques Dainat, Tobias Fietze)")
    )
  )
)
)



# Define server logic
server <- function(input, output, session) {
  #options(shiny.maxRequestSize = 800*1024^2)
  
  # Reactive value to store the processed text, empty from start
  reactive_text <- reactiveVal("")

  # Reactive value to store the user input
  user_input <- reactiveVal("")
  

  # Observe the submit button
  observeEvent(input$submit, {
    if (input$input_type == "pdf" && !is.null(input$pdf)) {
      pdf_text <- pdf_text(input$pdf$datapath)
      text_to_process <- paste(pdf_text, collapse = "\n")
    } else if (input$input_type == "text") {
      text_to_process <- input$text
      #Checks if string is empty
    } else { 
      text_to_process <- "No input provided."
    }
    
    # Store user input to be displayed
    user_input(text_to_process)
    

    #More than 0 character and it process the text with process_text() function
    if (nchar(text_to_process) > 0) {
      result <- process_text(text_to_process)
      reactive_text(result)
      
      # Update the map
      updated_sf <- world_sf %>% left_join(result, by = "ISO3")
     
      #Replacing NA's with 0
      updated_sf <- updated_sf %>%
        mutate(Count = replace(count, is.na(count), 0))
      
      
      output$map <- renderLeaflet({
        mybins <- c(0, 1, 2, 5, 10, 20, 50, Inf)
        mypalette <- colorBin(palette = "Blues", domain = updated_sf$Count, na.color = "transparent", bins = mybins)
        
        mytext <- paste(
          "Country: ", updated_sf$NAME, "<br/>",
          "Count: ", updated_sf$Count, "<br/>",
          "ISO3: ", updated_sf$ISO3, "<br/>",
          sep = ""
        ) %>% lapply(htmltools::HTML)
        
        leaflet(updated_sf) %>% 
          addTiles() %>% 
          setView(lat = 10, lng = 0, zoom = 2) %>%
          addPolygons(
            fillColor = ~mypalette(Count),
            stroke = TRUE,
            fillOpacity = 0.9,
            color = "white",
            weight = 0.3,
            label = mytext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(pal = mypalette, values = ~Count, opacity = 0.9, title = "Count", position = "bottomleft")
      })
      
      # Update the data table
      output$data_table <- DT::renderDataTable({
        datatable(reactive_text(), options = list(pageLength = 5, autoWidth = TRUE))
      })
      
      #Output from user's input
      output$user_input <- renderText({
        user_input()
      })

      
      #Output Button for Download
      output$button_download <- downloadHandler(
        filename = function() {
          "processed_data.csv"
        },
        content = function(file) {
          write.csv(result,file,row.names=FALSE,quote=F)
        })
      
      output$sankey_plot <- renderSankeyNetwork({
        sankey_plot <- get_sankey_plot(text_to_process)  # Call the function to get the Sankey plot
        sankey_plot
      })
      
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
