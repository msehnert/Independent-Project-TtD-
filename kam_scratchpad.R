
#KAM tries to read in data using metadata sheet

library(tidyverse)
library(lubridate)

data_dir <- "Data"
metadata <- read.csv("TTD_metadata.csv", stringsAsFactors = FALSE)

csv_list <- list.files(
  path = data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

cleanData <- function(dataSheet, cycleNumber, cycleType, spp, obsStart, obsEnd, file_name) {
  dataSheet %>%
    filter(between(obs, obsStart, obsEnd)) %>%
    mutate(
      logNum   = obs - (obsStart - 1),
      cycle    = cycleNumber,
      treatment = cycleType,
      species  = spp,
      File     = file_name
    ) %>%
    select(date, species, cycle, treatment, logNum, A, gsw, Ci)
  #add additional columns above if more data is desired from licor output
}

read_and_clean_all <- function(metadata, data_dir = "Data") {
  
  metadata <- metadata %>%
    mutate(File = as.character(File))
  
  # For each row in metadata, read, clean, and return a tibble
  all_clean <- metadata %>%
    pmap_dfr(function(Date, Instrument, Species, Treatment, Cycle, Start, End, File, Notes) {
      
      # Build full file path
      fullpath <- file.path(data_dir, File)
      
      # Read raw CSV
      raw <- readr::read_csv(fullpath, show_col_types = FALSE)
      
      # Apply cleaning based on metadata fields
      cleanData(
        dataSheet   = raw,
        cycleNumber = Cycle,       # use the metadata "Cycle" (A/B/etc.)
        cycleType   = Treatment,   # use the metadata "Treatment"
        spp         = Species,     # use the metadata "Species"
        obsStart    = Start,
        obsEnd      = End,
        file_name   = File
        
      )
    })
  
  return(all_clean)
}

cleaned_all <- read_and_clean_all(metadata, data_dir = "Data")

glimpse(cleaned_all)


ggplot(cleaned_all, aes(logNum, A, color = treatment)) +
  geom_point() + geom_line() + facet_grid(species ~ cycle)
