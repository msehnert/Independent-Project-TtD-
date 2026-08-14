##debugging aaaaa

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(zoo)


debugData <- read.csv("data/debug data sheet.csv")

debugOut1 <- debugData |> 
  mutate(test = NA) |> 
  group_by(species, cycle, treatment, replicate) |> 
    testingScratchpad()

testingScratchpad <- function(dataIn) {
  for(i in 1:length(dataIn$cycle)) {
   dataIn$test[i] <- i
  } 
  return(dataIn)
}

debugOut2 <- debugData |> 
  mutate(test=NA) |> 
  group_by(species, cycle, treatment, replicate) 

indexesOut <- 
