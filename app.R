library(tidyverse)
library(ggplot2)
library(readxl)
library(stringr)
library(lubridate)
library(janitor)
library(writexl)
library(shiny)

# UI definition
ui <- fluidPage(
  titlePanel("Check Altis Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Excel File", accept = c(".xlsx")),
      fileInput("sampletimes", "Choose Sample Times CSV (optional)", accept = c(".csv")),
      actionButton("process_button", "Process Data")
    ),
    
    mainPanel(
      tableOutput("processed_data_preview"),
      
      # Debugging outputs
      verbatimTextOutput("debug_excel_data"),  # To display the first few rows of excelData
      verbatimTextOutput("debug_column_names"),  # To display column names of excelData
      verbatimTextOutput("debug_all_data"),  # To display allData after renaming columns
      verbatimTextOutput("debug_column_existence"),  # To check if all columns exist
      verbatimTextOutput("debug_cleaned_data"),  # To display cleaned data after mutation
      verbatimTextOutput("debug_final_data"),  # To display final data after filtering
      verbatimTextOutput("debug_result_tables"),  # To display result_tables structure
      verbatimTextOutput("debug_compiled_results"),  # To display compiled_results structure
      
      downloadButton("download_csv", "Download compiled results as CSV"),
      downloadButton("download_excel", "Download report of flags for each compound")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive expression for processing the data file
processed_data <- eventReactive(input$process_button, {
  req(input$file)  # Ensure the Excel file is uploaded before processing
  
  # Read the uploaded Excel file
  # Read the uploaded Excel file
  file_path <- normalizePath(input$file$datapath)
  file_name <- basename(file_path)  # Extract the base file name
  
  # Get all sheet names
  sheets <- excel_sheets(file_path)
  
  # Read all sheets and bind them together
  excelData <- sheets %>%
    map(~ {
      # Read each sheet
      sheet_data <- read_excel(file_path, sheet = .x)
      
      # Convert all columns to character and add SourceFile column
      sheet_data <- sheet_data %>%
        mutate(across(everything(), as.character)) %>%
        mutate(SourceFile = file_name, SourceSheet = .x)  # Add the sheet name as metadata
      
      return(sheet_data)
    }) %>%
    bind_rows()  # Combine all sheets into one data frame
    
    allData <- excelData %>%
      rename(rawFileName = "Sample Raw File Name", sampleType = "Sample Type", sampleID = "Sample ID", 
             vialPosition = "Sample Vial Position", sampleOrder = "Sample Order", sampleOrderSortable = "Sample Order Sortable", 
             sampleLevel = "Sample Level", sampleAcquisitionDate = "Sample Acquisition Date", injectionVoluL = "Sample Injection Volume", 
             conversionFactor = "Sample Conversion Factor", compound = "Compound Name", retentionTime = "Retention Time", theoreticalAmount = "Theoretical Amount",
             calculatedAmount = "Calculated Amount", totalArea = "Total Area", totalHeight = "Total Height", 
             totalIonArea = "Total Ion Area", totalIonHeight = "Total Ion Height", totalIonResponse = "Total Ion Response",
             totalResponse = "Total Response", peakArea = "Peak Area", peakHeight = "Peak Height", responseFactor = "Response Factor", 
             istdCompoundName = "ISTD Compound Name", istdPeakArea = "ISTD Area") %>%
      mutate(across(c(theoreticalAmount, retentionTime, calculatedAmount, totalArea, totalHeight, 
                      totalIonArea, totalIonHeight, totalResponse, peakArea, 
                      peakHeight, responseFactor, istdPeakArea),
                    ~ parse_number(if_else(. %in% c("N/F", "N/A"), NA_character_, .)))) %>% 
      mutate(site = if_else(sampleType == "Unknown", word(rawFileName, 1, sep = "[-_]"), NA_character_),
             bottleNumber = if_else(sampleType == "Unknown", word(rawFileName, 2, sep = "[-_]"), NA_character_),
             sampleName = if_else(sampleType == "Unknown", paste(site, bottleNumber, sep = "-"), NA_character_)) %>% 
      select(site, bottleNumber, sampleName, everything())
    
    
    # Loop through the compounds to create result tables
    result_tables <- list()  # Initialize the result_tables list
    
    for (compoundName in unique(allData$compound)) {
      
      summaryStats <- allData %>% filter(compound == compoundName) %>%
        summarize(meanIstdArea = mean(istdPeakArea[sampleType %in% c("Cal Std", "Matrix Blank")], na.rm = TRUE),
                  meanBlankArea = mean(peakArea[sampleType == "Matrix Blank"], na.rm = TRUE),
                  meanQcLowArea = mean(peakArea[sampleType == "QC Check" & theoreticalAmount == 1.00], na.rm = TRUE),
                  meanQcHighArea = mean(peakArea[sampleType == "QC Check" & theoreticalAmount == 10.00], na.rm = TRUE),
                  detectionLimit = min(calculatedAmount[sampleType == "Cal Std" & abs(calculatedAmount - theoreticalAmount) / theoreticalAmount <= 0.2], na.rm = TRUE),
                  numBlanks = sum(as.numeric(sampleType == "Matrix Blank", na.rm = TRUE)),
                  numSamples = sum(as.numeric(sampleType == "Unknown", na.rm = TRUE)))
      
      if (!is.finite(summaryStats$detectionLimit)) {
        summaryStats$detectionLimit <- allData %>%
          filter(compound == compoundName, sampleType == "Cal Std", theoreticalAmount == 0.01) %>%
          pull(calculatedAmount) %>%
          first()
      }
      
      # Generate result table for the compound
      result_table <- allData %>% filter(compound == compoundName) %>%
        mutate(
          calRecovery = if_else(sampleType == "Cal Std", 
                                round(abs(calculatedAmount / theoreticalAmount), 3), 
                                NA_real_),
          calFlag = if_else(abs(1 - calRecovery) < 0.2, 
                            "PASS", 
                            "FLAG - Calibration standard outside 20% range"),
          istdRecovery = abs(istdPeakArea / summaryStats$meanIstdArea),
          istdFlag = case_when(is.na(istdPeakArea) ~ "FLAG - External calibration calculation",
                               abs(1 - istdRecovery) < 0.2 ~ "PASS",
                               TRUE ~ "FLAG - ISTD recovery outside 20% range by peak area"),
          blankRecovery = if_else(sampleType == "Matrix Blank", 
                                  round(abs(peakArea / summaryStats$meanBlankArea), 3),
                                  NA_real_),
          blankFlag = if_else(abs(1 - blankRecovery) < 0.5, 
                              "PASS",
                              "FLAG - Blank recovery outside of 50% range by peak area"),
          reportableConcentration = if_else(
            calculatedAmount / coalesce(istdRecovery, 1) >= summaryStats$detectionLimit,
            as.character(round(if_else(is.na(istdRecovery), calculatedAmount, calculatedAmount / istdRecovery), 3)),
            paste0("BDL (<", round(summaryStats$detectionLimit, 2), ")"))) %>% 
        select(rawFileName, sampleName, sampleType, compound, theoreticalAmount, calculatedAmount, reportableConcentration, calRecovery, calFlag, 
               istdPeakArea, istdRecovery, istdFlag, peakArea, blankRecovery, blankFlag)
      
      # Store the result table in result_tables list
      result_tables[[compoundName]] <- result_table
    }
    
    # Debugging: Print the structure of result_tables
    output$debug_result_tables <- renderPrint({
      cat("Structure of result_tables:\n")  # Title or header
      str(result_tables)  # This will print the structure of result_tables
    })
    
    # Debugging: Print the structure of result_tables
    output$debug_result_tables <- renderPrint({
      cat("Glimpse of result_tables:\n")
      glimpse(result_tables)  # This will print the structure of result_tables
    })
    
    # Combine all the result tables into compiled_results
    compiled_results <- result_tables %>%
      map_dfr(~ {
        .x %>%
          select(rawFileName, compound, reportableConcentration) %>%
          filter(!is.na(reportableConcentration))  # Uncomment if you need this filtering
      }) %>%
      pivot_wider(names_from = compound, values_from = reportableConcentration, values_fn = ~ .x[1])
    
    # Convert columns to character before joining
    if (!is.null(csvSampleTimes)) {
      csvSampleTimes$sample <- as.character(csvSampleTimes$sample)
    }
    compiled_results$sampleName <- as.character(compiled_results$sampleName)
    
    
    if (!is.null(csvSampleTimes)) {
      compiled_results <- compiled_results %>%
        left_join(csvSampleTimes, by = c("sampleName" = "sample"))
    }

    return(compiled_results)
  })
  
  # Render processed data table in the UI
  output$processed_data_preview <- renderTable({
    processed_data()
  })
  
  # Additional code for handling downloads, UI, etc.
  output$download_results <- downloadHandler(
    filename = function() {
      paste("processed_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
})
  
}

# Run the app
shinyApp(ui = ui, server = server)

