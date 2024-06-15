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

# Define UI for application
ui <- fluidPage(

  # Custom CSS to centralize and make inputs bigger
  tags$head(
    tags$style(HTML("
      .center-content {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
        min-height: 60vh; /* Adjust the height for better vertical centering */
      }
      .wide-input {
        width: 80%; /* Adjust the width as needed */
        max-width: 600px; /* Max width to control size on large screens */
        margin-top: 10px;
      }
      .submit-button {
        margin-top: 20px;
      }
    "))
  ),
  # Application title
  titlePanel("Team Lookal"),
  

  # Centralized layout for inputs
  fluidRow(
    column(12,
           div(class = "center-content",
               # Input: Select between text and PDF
               div(style = "width: 200%; max-width: 300px; margin-top: 10px;",
                   selectInput("input_type", "Choose Input Type:", 
                               choices = c("Text" = "text", "PDF" = "pdf"),
                               selected = "text")
               ),
               
               # Conditional input: Text area for text input
               conditionalPanel(
                 condition = "input.input_type == 'text'",
                 div(style = "width: 150%; max-width: 600px; margin-top: 10px;",
                     textAreaInput("text", "Your text:", "", rows = 3, resize = "vertical", placeholder = "Please enter your text here!")
                 )
               ),
               
               # Conditional input: File upload for PDF
               conditionalPanel(
                 condition = "input.input_type == 'pdf'",
                 div(style = "width: 80%; max-width: 600px; margin-top: 10px;",
                     fileInput("pdf", "Upload a PDF file:", accept = c(".pdf"))
                 )
               ),
               
               # Action button to submit the input
               div(class = "submit-button",
                   actionButton("submit", "Submit")
               )
           )
    )
  ),

    
  # Main panel to display outputs
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)


