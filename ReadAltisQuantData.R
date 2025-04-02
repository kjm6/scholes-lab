rm(list = ls())
lapply(c("tidyverse", "ggplot2", "readxl", "stringr", "lubridate", "janitor"), library, character.only = TRUE)
setwd("C:/Users/katie/OneDrive - UBC/Research/Turf/TurfStormwater_1") #SET THIS TO THE PATH WHERE YOUR FILE IS SAVED

file_paths <- c("AltisRun1.4_QuantitationData_w_ISTD_20250226112814.xlsx") #SET THIS TO EXCEL SPREADSHEET FILE NAME
csvSampleTimes <- read.csv("SampleTimes.csv") #SET THIS TO SAMPLE SPREADSHEET FILE NAME

#Converting files into one data frame
excelData <- file_paths %>% set_names() %>%
  map(~ {
    file_path <- file.path(getwd(), .x)
    file_name <- basename(file_path) 
    sheets <- excel_sheets(file_path)
    sheets %>% 
      map(~ {sheet_data <- read_excel(file_path, sheet = .)
      sheet_data <- sheet_data %>%
        mutate(across(everything(), as.character)) %>%  # Convert all columns to character temporarily
        mutate(SourceFile = file_name)
      return(sheet_data)}) %>% 
      bind_rows()  # Combine all sheets in the file into one data frame
  }) %>%
  bind_rows()  # Combine all data from different files

#Cleaning Altis dataframe
allData <- excelData %>%
  rename(RawFileName = "Sample Raw File Name", SampleType = "Sample Type", SampleID = "Sample ID", 
         VialPosition = "Sample Vial Position", SampleOrder = "Sample Order", SampleOrderSortable = "Sample Order Sortable", 
         SampleLevel = "Sample Level", SampleAcquisitionDate = "Sample Acquisition Date", InjectionVoluL = "Sample Injection Volume", 
         ConversionFactor = "Sample Conversion Factor", Compound = "Compound Name", RetentionTime = "Retention Time", TheoreticalAmount = "Theoretical Amount",
         CalculatedAmount = "Calculated Amount", TotalArea = "Total Area", TotalHeight = "Total Height", 
         TotalIonArea = "Total Ion Area", TotalIonHeight = "Total Ion Height", TotalIonResponse = "Total Ion Response",
         TotalResponse = "Total Response", PeakArea = "Peak Area", PeakHeight = "Peak Height", ResponseFactor = "Response Factor", 
         ISTDCompoundName = "ISTD Compound Name", ISTDArea = "ISTD Area") %>%
  mutate(across(c(TheoreticalAmount, RetentionTime, CalculatedAmount, TotalArea, TotalHeight, 
                  TotalIonArea, TotalIonHeight, TotalResponse, PeakArea, 
                  PeakHeight, ResponseFactor, ISTDArea),
                ~ parse_number(if_else(. %in% c("N/F", "N/A"), NA_character_, .)))) %>%
  mutate(Site = if_else(SampleType == "Unknown", word(RawFileName, 1, sep = "[-_]"), NA_character_),
         BottleNumber = if_else(SampleType == "Unknown", word(RawFileName, 2, sep = "[-_]"), NA_character_),
         SampleName = if_else(SampleType == "Unknown", paste(Site, BottleNumber, sep = "-"), NA_character_)) %>% 
  select(Site, BottleNumber, SampleName, everything()) %>% 
  filter(Compound %in% c("Benzotriazole", "6PPD Quinone", "6ppd", "HMMM"))

# Initialize result list
result_tables <- list()

# Process each compound
for (compound in unique(allData$Compound)) {
  
  # Compute summary statistics
  summaryStats <- allData %>% filter(Compound == compound) %>%
    summarize(meanIstdArea = mean(ISTDArea[SampleType %in% c("Cal Std", "Matrix Blank")], na.rm = TRUE),
              meanBlankArea = mean(PeakArea[SampleType == "Matrix Blank"], na.rm = TRUE),
              meanQcLowArea = mean(PeakArea[SampleType == "QC Check" & TheoreticalAmount == 1.00], na.rm = TRUE),
              meanQcHighArea = mean(PeakArea[SampleType == "QC Check" & TheoreticalAmount == 10.00], na.rm = TRUE),
              detectionLimit = min(CalculatedAmount[SampleType == "Cal Std" & abs(CalculatedAmount - TheoreticalAmount) / TheoreticalAmount <= 0.2], na.rm = TRUE),
    numBlanks = sum(as.numeric(SampleType == "Matrix Blank", na.rm = TRUE)),
    numSamples = sum(as.numeric(SampleType == "Unknown", na.rm = TRUE)))
  
  # Handle cases where detectionLimit is NA or infinite
  if (!is.finite(summaryStats$detectionLimit)) {
    summaryStats$detectionLimit <- allData %>%
      filter(Compound == compound, SampleType == "Cal Std", TheoreticalAmount == 0.01) %>%
      pull(CalculatedAmount) %>%
      first()
  }
  
  # Generate compound table
    result_table <- allData %>% filter(Compound == compound) %>%
      mutate(
        calRecovery = if_else(SampleType == "Cal Std", abs(CalculatedAmount - TheoreticalAmount) / TheoreticalAmount, NA_real_),
        IstdRecovery = abs(ISTDArea - summaryStats$meanIstdArea) / summaryStats$meanIstdArea,
        SufficientBlanks = if_else(summaryStats$numBlanks/summaryStats$numSamples > 0.0666, "Sufficient Blanks", "Insufficient Blanks"), #Not working?
        CalFlag = if_else(abs(calRecovery) > 0.2, "FLAG - Calibration standard outside 20% range", "PASS"),
        DetectionLimit = case_when(
          any(!is.na(calRecovery) & calRecovery <= 0.2) ~ as.character(summaryStats$detectionLimit),  
          TheoreticalAmount < summaryStats$detectionLimit ~ paste0("BDL(<", round(summaryStats$detectionLimit, 2), "ppb)"),
          TRUE ~ as.character(TheoreticalAmount))) %>%
      select(Compound, SampleType, SampleLevel, TheoreticalAmount, CalculatedAmount, DetectionLimit, calRecovery, CalFlag, 
             PeakArea, ISTDArea, IstdRecovery, SampleAcquisitionDate, SufficientBlanks)
  
  # Store results
  result_tables[[compound]] <- result_table
  
}
