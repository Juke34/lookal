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
      .title-text {
        font-size: 24px;  
        font-weight: bold;  
        color: #3896b4; 
        margin-right: auto;  
        padding-left: 10px;
      }
      .footer {
        text-align: center;
        margin-top: 20px;
        color: #6c757d;
        font-size: 10px; 
        line-height: 1.2; 
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
      
      .button-container {
        display: flex;
        align-items: center;
        margin-top: 20px;
      }
      
      .submit-button {
        margin-right: 10px;
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
      
      .tab-content {
        margin-top: 20px;
      }
      
    "))
  ),
  
  # Header with logo and title
  div(class = "header",
      span(class = "title-text", "Team Lookal"), # Title text
      img(src = "https://nbisweden.github.io/raukr-2024/assets/logos/raukr.png", class = "logo") # Logo image
  ),
  
  # Sidebar layout for navigation and controls
  sidebarLayout(
    sidebarPanel(width = 3,
                 # Tab panel moved to sidebar
                 tabsetPanel(id = "tabs",
                             tabPanel("Input Data"),
                             tabPanel("Output World Map"),
                             tabPanel("Sankey Plot")
                 )
    ),
    mainPanel(width = 9,
              fluidPage(
                column(width = 11,
              div(class = "tab-content",
                  # Content for Input Data tab
                  conditionalPanel(
                    condition = "input.tabs == 'Input Data'",
                    div(style = "max-width: 2000px; margin: auto;",
                        div(style = "text-align: justify; margin-bottom: 20px;",
                            p("Welcome to Lookal, our Shiny application for Text Etymology Plotting. This app allows you to explore the origins and evolution of textual content.")
                        ),
                        
                        div(style = "width: 200px; margin-bottom: 10px;",
                            selectInput("input_type", "Choose Input Type:", 
                                        choices = c("Text" = "text", "PDF" = "pdf"),
                                        selected = "text")
                        ),
                        
                        conditionalPanel(
                          condition = "input.input_type == 'text'",
                          div(
                            textAreaInput("text", "Your text:", "", width = "100%", rows = 3, resize = "vertical", placeholder = "Please enter your text here!")
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.input_type == 'pdf'",
                          div(style = "width: 100%; max-width: 600px; margin-bottom: 10px;",
                              fileInput("pdf", "Upload a PDF file:", accept = c(".pdf"))
                          )
                        ),
                        
                        div(class = "button-container", # Use flexbox for button alignment
                            actionButton("submit", "Submit", class = "submit-button"),
                            actionButton("clear", "Clear")
                        ),
                        
                        h3("Your Input:"),
                        textOutput("user_input") %>% withSpinner(type = 1, color = "#3896b4", size = 1.0),
                        textOutput("display_text")
                    )
                  ),
                  
                  # Content for Output World Map tab
                  conditionalPanel(
                    condition = "input.tabs == 'Output World Map'",
                    fluidPage(
                      h5("Your text is mainly connected to the following countries:"),
                      leafletOutput("map") %>% withSpinner(type = 1, color = "#3896b4", size = 1.5),
                      br(), br(),
                      DT::dataTableOutput("data_table"),
                      downloadButton("button_download", "Download Results")
                    )
                  ),
                  # Content for Sankey Plot tab
                  conditionalPanel(
                    condition = "input.tabs == 'Sankey Plot'",
                    fluidPage(
                      h3("Language Connections Sankey Plot"),
                      sankeyNetworkOutput("sankey_plot") %>% withSpinner(type = 1, color = "#3896b4", size = 1.5)
                    )
                  )
              ),
              # Footer section
              div(class = "footer",
                  p("© 2024 Team Lookal. Authors: Ine Bonthuis, Jacques Dainat, Tobias Fietze")
              )
     )
    )
   )
  )
)


# Define server logic
server <- function(input, output, session) {
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
    } else { 
      text_to_process <- "No input provided."
    }
    
    user_input(text_to_process)
    
    if (nchar(text_to_process) > 0) {
      result <- process_text(text_to_process)
      reactive_text(result)
      
      updated_sf <- world_sf %>% left_join(result, by = "ISO3")
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
      
      output$data_table <- DT::renderDataTable({
        datatable(reactive_text(), options = list(pageLength = 5, autoWidth = TRUE))
      })
      
      output$user_input <- renderText({
        user_input()
      })
      
      output$button_download <- downloadHandler(
        filename = function() {
          "processed_data.csv"
        },
        content = function(file) {
          write.csv(result, file, row.names = FALSE, quote = F)
        }
      )
      
      output$sankey_plot <- renderSankeyNetwork({
        sankey_plot <- get_sankey_plot(text_to_process)
        sankey_plot
      })
      
    }
  })
  
  # Clear button, clear textinput, world map, sankey plot
  observeEvent(input$clear, {
    updateTextAreaInput(session, "text", value = "")
    user_input("")
    reactive_text("")
    output$map <- renderLeaflet(NULL)
    output$data_table <- DT::renderDataTable(NULL)
    output$sankey_plot <- renderSankeyNetwork(NULL)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
