rm(list=ls()) #start fresh by clearing your environment

lapply(c("tidyverse", "ggplot2", "readxl", "stringr", "lubridate", "scales", "patchwork"), library, character.only = TRUE)
setwd("C:/Users/katie/OneDrive - UBC/Research/Turf/TurfStormwater_1")

file_paths <- c("TurfStormwater1.1_QuantitationData_w_ISTD_20250226153712.xlsx", 
                "TurfStormwater1.3_QuantitationData_w_ISTD_20250226162046.xlsx",
                "Run1.5_QuantitationData_w_ISTD_20250226152046.xlsx")
csvSampleTimes <- read.csv("SampleTimes.csv")

csvWeather <- read.csv("en_climate_hourly_BC_1108446_01-2025_P1H.csv") %>% 
  select(Station.Name, Year, Month, Day, Time..LST., Temp...C., Precip..Amount..mm., Rel.Hum....) %>% 
  rename(Station = "Station.Name",  TimeLST = "Time..LST.", TempC = "Temp...C.", 
         RelHumPercent = "Rel.Hum....", PrecipMM = "Precip..Amount..mm.") %>% 
  mutate(DateTimeLST = ymd(paste(Year, Month, Day, sep = "-")) + hms(TimeLST)) %>% 
  mutate(DateTimeUTC = as.POSIXct(DateTimeLST, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

excel_sheets <- file_paths %>%
  set_names() %>%  # Set file names as list names
  map(~ {
    file_path <- file.path(getwd(), .x)  # Get full file path
    file_name <- basename(file_path)     # Extract just the file name (not the full path)
    sheets <- excel_sheets(file_path)    # Get sheet names in the Excel file
    sheets %>% 
      map(~ {
        sheet_data <- read_excel(file_path, sheet = .)
        
        sheet_data <- sheet_data %>%
          mutate(across(everything(), as.character)) %>%  # Convert all columns to character
          mutate(SourceFile = file_name)
        
        return(sheet_data)
      }) %>% 
      bind_rows()  # Combine all sheets in the file into one data frame
  }) %>%
  bind_rows()  # Combine all data from different files

masterData <- excel_sheets %>%
  select(-"Sample ID", - "Sample Name", -"Sample Vial Position", -"Sample Order", 
         -"Sample Order Sortable", -"Theoretical Amount") %>% 
  rename(FileName = "Sample Raw File Name", SampleType = "Sample Type", SampleLevel = "Sample Level", 
         SampleAcquisitionDate = "Sample Acquisition Date", InjectionVoluL = "Sample Injection Volume", 
         ConversionFactor = "Sample Conversion Factor", Compound = "Compound Name", RT = "Retention Time", 
         CalcConc = "Calculated Amount", TotArea = "Total Area", TotHeight = "Total Height", TotIonArea = "Total Ion Area", 
         TotIonHeight = "Total Ion Height", TotResponse = "Total Response", PeakArea = "Peak Area", PeakHeight = "Peak Height",
         ResponseFactor = "Response Factor", ISTD = "ISTD Compound Name", ISTDArea = "ISTD Area") %>% 
  mutate(across(RT:ISTDArea, ~ suppressWarnings(as.numeric(.)))) %>% 
  separate("FileName", into = c("Site", "Number", "RunPolarity"), sep = "_", extra = "merge") %>% 
  filter(rowSums(is.na(excel_sheets)) < ncol(excel_sheets)) %>% 
  mutate(SampleAcquisitionDate = as.Date(SampleAcquisitionDate, format = "%Y-%m-%d")) %>% 
  mutate(SampleName = paste(Site, Number, sep = "-")) %>% 
  left_join(csvSampleTimes, by = c("SampleName" = "sample")) %>% 
  mutate(DateTime = ymd(date) + hms(time)) %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) 

sites <- c("ARG")
compounds_to_keep <- c("6ppd", "6ppd Quinone", "Benzotriazole", "HMMM")

# Loop over each site
for (site_name in sites) {
  # Extract the specific data for the site
  site_data <- masterData %>% 
    filter(Site == site_name) %>%
    filter(SampleType == "Unknown") %>% 
    filter(RunPolarity == "POS") %>% 
    filter(Compound %in% compounds_to_keep)  # Filter for desired compounds
  
    # Organics plot (Compound vs Concentration)
    organics_plot <- site_data %>%
      ggplot(aes(x = DateTime, y = CalcConc, color = Compound)) +
      geom_point(alpha = 0.7, size = 1.5) +
      labs(title = paste(site_name, "Organic Compound Concentration"), y = "Concentration (ppb)") +
      theme_light() +
      theme(panel.grid = element_blank(), axis.title.x = element_blank()) +
      theme(legend.position = "none") +
      facet_wrap(~Compound, scales = "free_y")
    
    # # Precipitation plot (Time vs Precipitation)
    # precip_plot <- weather_filtered %>%
    #   ggplot(aes(x = DateTimeUTC, y = PrecipMM)) +
    #   geom_col(fill = "blue", alpha = 0.6) +
    #   scale_y_reverse() +
    #   labs(title = paste(site_name, "Precipitation"), y = "Precipitation (mm)", x = NULL) +
    #   theme_light() +
    #   scale_y_reverse(position = "right") +
    #   theme(panel.grid = element_blank(), axis.text.x = element_blank())
    # 
    # # Combine the plots: Precipitation plot on top, Organics plot below
    # combined_plot <- precip_plot / organics_plot + plot_layout(heights = c(1, 3))
    
    # Print the combined plot for each site
    print(organics_plot)
  }
