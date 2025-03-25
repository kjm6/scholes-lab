lapply(c("tidyverse", "ggplot2", "readxl", "stringr", "lubridate"), library, character.only = TRUE)
setwd("C:/Users/katie/OneDrive - UBC/Research/Turf/TurfStormwater_1") #SET THIS TO THE PATH WHERE YOUR FILE IS SAVED

file_paths <- c("AltisRun1.4_QuantitationData_w_ISTD_20250226112814.xlsx") #SET THIS TO EXCEL SPREADSHEET FILE NAME
csvSampleTimes <- read.csv("SampleTimes.csv") #SET THIS TO SAMPLE SPREADSHEET FILE NAME

#Converting files into one data frame
excelData <- file_paths %>%
  set_names() %>%
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
  select(Site, BottleNumber, SampleName, everything())

#Summary
summary <- bind_rows(
  allData %>% 
    filter(SampleType == "Matrix Blank") %>% 
    group_by(Compound) %>% 
    summarize(MeanBlankPeakArea = mean(PeakArea, na.rm = TRUE)) %>%
    mutate(SampleType = "Matrix Blank") %>%
    ungroup(),
  allData %>% 
    filter(SampleType == "Cal Std") %>% 
    group_by(Compound) %>% 
    summarize(MeanISTDPeakArea = mean(ISTDArea, na.rm = TRUE)) %>%
    mutate(SampleType = "Cal Std") %>%
    ungroup()
)


#Checking Values
allDataChecked <- allData %>% 
  filter(SampleType == "Cal Std") %>% 
  mutate(ccFlag = if_else(abs(CalculatedAmount - TheoreticalAmount) / CalculatedAmount > 0.2, 
                          "FLAG", NA_character_))%>% 
  mutate(ccIstdFlag = if_else)
  select(ccFlag, everything())

#Checking ISTD in Samples
istdCheck <- allData %>% 
  filter(SampleType == "Unknown") %>% 
  group_by(Compound) %>% 
  summarize()
  

  


