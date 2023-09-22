library(shiny)

lasso_model <- readRDS("lasso_model.rds")

# Define the UI
ui <- fluidPage(
  titlePanel("Movie Predictions with Lasso Regression"),
  sidebarLayout(
    sidebarPanel(
      # Input fields for user to enter feature values
      numericInput("runtime", "Runtime (mins):", value = 100),
      numericInput("year", "Year:", value = 2022),
      numericInput("metascore", "Metascore:", value = 60),
      numericInput("numvotes", "Number of Votes:", value = 1000),
      numericInput("budget", "Budget (Normalized):", value = 16), # Adjust the initial value
      checkboxInput("isenglish", "Is English?", value = TRUE),
      selectInput("agerating", "Age Rating:",
                  choices = c("G", "PG", "PG-13", "15", "R"),
                  selected = "R"),
      fileInput("datafile", "Upload CSV File (Optional):"),
      actionButton("predictButton", "Predict"),
    ),
    mainPanel(
      # Display the prediction result
      h3("Predicted Total Views:"),
      verbatimTextOutput("predictionText")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$predictButton, {
    if(is.na(input$runtime)){
      showNotification("Runtime must be a number", type = "error")
      return()
    }
    
    if(input$runtime < 0){
      showNotification("Runtime cannot be negative", type = "error")
      return()
    }
    
    # Check if a file was uploaded
    if (!is.null(input$datafile)) {
      # If a file was uploaded, read it into a data frame
      data_upload <- read.csv(input$datafile$datapath)
    } else {
      # If no file was uploaded, create a data frame from user input
      data_upload <- data.frame(
        Runtime..mins. = input$runtime,
        Year = input$year,
        Metascore = input$metascore,
        Num.Votes = input$numvotes,
        Budget_Normalised = input$budget,
        IsEnglish = as.numeric(input$isenglish),
        AgeRating_enc = as.numeric(input$agerating)
      )
    }
    
    # Use the trained Lasso model to make predictions
    prediction <- predict(lasso_model, newx = as.matrix(data_upload), s = 0.01)
    
    # Display the prediction result
    output$predictionText <- renderText({
      pred <- round(exp(prediction), 0)
      pred <- format(pred, big.mark = ",")
      paste0("Predicted Total Views:", pred)
    })
  })
}

# Run the Shiny application
shinyApp(ui, server)
