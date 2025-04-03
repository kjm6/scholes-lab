rm(list = ls())
lapply(c("tidyverse", "ggplot2", "readxl", "stringr", "lubridate", "janitor", "writexl"), library, character.only = TRUE)
# setwd("C:/Users/katie/OneDrive - UBC/Research/Turf/TurfStormwater_1")

file_paths <- c("AltisRun1.4_QuantitationData_w_ISTD_20250226112814.xlsx")
 sample_times_path <- "SampleTimes.csv"
 if (file.exists(sample_times_path)) {
   csvSampleTimes <- read.csv(sample_times_path)
 } else {
   csvSampleTimes <- NULL  # Set to NULL if the file is missing
   message("No SampleTimes.csv found. Proceeding without it.")
 }
result_tables <- list()

#Converting files into one data frame
excelData <- file_paths %>% set_names() %>%
  map(~ {
    file_path <- file.path(getwd(), .x)
    file_name <- basename(file_path) 
    sheets <- excel_sheets(file_path)
    sheets %>% 
      map(~ {sheet_data <- read_excel(file_path, sheet = .)
      sheet_data <- sheet_data %>%
        mutate(across(everything(), as.character)) %>% 
        mutate(SourceFile = file_name)
      return(sheet_data)}) %>% 
      bind_rows() 
  }) %>%
  bind_rows() 

#Cleaning Altis dataframe
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
  select(site, bottleNumber, sampleName, everything()) %>% 
  filter(compound %in% c("Benzotriazole", "6PPD Quinone", "6ppd", "HMMM"))

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
      filter(compound == compound, sampleType == "Cal Std", theoreticalAmount == 0.01) %>%
      pull(calculatedAmount) %>%
      first()
  }
  
  # Generate compound table
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
  
  result_tables[[compoundName]] <- result_table
  
}

#Printing Results
compiled_results <- result_tables %>%
  map_dfr(~ {
    .x %>%
      select(rawFileName, sampleName, compound, reportableConcentration) %>%
      filter(!is.na(reportableConcentration)) 
  }) %>%
  pivot_wider(names_from = compound, values_from = reportableConcentration, values_fn = ~ .x[1])

# Only join sample times if the file was loaded successfully

# Convert columns to character before joining
if (!is.null(csvSampleTimes)) {
  csvSampleTimes$sample <- as.character(csvSampleTimes$sample)
}
compiled_results$sampleName <- as.character(compiled_results$sampleName)


if (!is.null(csvSampleTimes)) {
  compiled_results <- compiled_results %>%
    left_join(csvSampleTimes, by = c("sampleName" = "sample"))
}
  
output_file_path <- file.path(getwd(), paste0("reportableresults_", gsub(".xlsx", "", basename(file_paths[1]) ), ".csv"))
write.csv(compiled_results, output_file_path, row.names = FALSE)
cat("Data saved as CSV at:", output_file_path, "\n")

output_excel_path <- file.path(getwd(), paste0("resultsFlagReports_", file_paths, ".xlsx"))
write_xlsx(result_tables, path = output_excel_path)
cat("Data saved as excel file at:", output_file_path, "\n")