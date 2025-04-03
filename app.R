library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Environmental Data Processor"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      fileInput("excel_file", "Choose Excel File", accept = ".xlsx"),
      fileInput("sample_file", "Choose Sample Times CSV File", accept = ".csv"),
      actionButton("process", "Process Data"),
      hr(),
      downloadButton("download_results", "Download Processed Results")
    ),
    
    mainPanel(
      h3("Processed Data Preview"),
      tableOutput("preview")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive expression to load the uploaded files
  data_input <- reactive({
    req(input$excel_file, input$sample_file)  # Ensure both files are uploaded
    
    # Read the Excel file and sample times CSV
    excelData <- read_excel(input$excel_file$datapath)
    csvSampleTimes <- read.csv(input$sample_file$datapath)
    
    list(excelData = excelData, sampleTimes = csvSampleTimes)
  })
  
  # Reactive expression to process the data once the button is clicked
  processed_data <- eventReactive(input$process, {
    # Get the data from the reactive expression
    data <- data_input()
    excelData <- data$excelData
    csvSampleTimes <- data$sampleTimes
    
    # Your data processing steps (using the code you have shared above)
    compiled_results <- excelData %>%
      map_dfr(~ {
        .x %>%
          select(sampleName, rawFileName, compound, reportableConcentration) %>%
          filter(!is.na(reportableConcentration))  # Remove rows where reportableConcentration is NA
      }) %>%
      pivot_wider(names_from = compound, values_from = reportableConcentration, values_fn = ~ .x[1]) %>%
      left_join(csvSampleTimes, by = c("sampleName" = "sample"))
    
    # Return the processed data
    compiled_results
  })
  
  # Display the preview of the processed data
  output$preview <- renderTable({
    processed_data()
  })
  
  # Allow the user to download the processed results as CSV
  output$download_results <- downloadHandler(
    filename = function() {
      paste("processed_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
