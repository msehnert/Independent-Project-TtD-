## Script to read in autolog data from LI 6800 for independent project
## Maggie Sehnert, 2026-07-16

## packages
library(lubridate)
library(ggplot2)
library(dplyr)


##creating/checking data
rawData = read.csv("2026-07-15 ttd test cleaned.csv")
colnames(rawData) #get column names
str(rawData) #get col, data type, first few values

#beech
rawData %>%
  filter(obs < 29) %>%
  select(obs, date, A, Ci, gsw) -> beechData

#tulip
rawData %>% 
  filter(obs > 28) %>%
  select(obs, date, A, Ci, gsw) -> tulipData

##graph tulip data
ggplot(tulipData, aes(x = obs, y = A)) + 
  geom_point() + 
  geom_line()

#graph beech
ggplot(beechData, aes(x = obs, y = A)) + 
  geom_point() + 
  geom_line()

#which tree is which 
rawData %>%
  mutate(species = if_else(obs < 29, "beech", "tulip")) %>%
  select(obs, date, A, Ci, gsw, species) %>%
  group_by(species) %>%
  mutate(obs2 = row_number())-> labeledData

#graph both A
ggplot(labeledData, aes(x = obs2, y = A, color = species)) + 
  geom_point() + 
  geom_line()

#graph both gsw
ggplot(labeledData, aes(x = obs2, y = gsw, color = species)) + 
  geom_point() + 
  geom_line()

#graph both both
ggplot(labeledData, aes(x = obs2)) + 
  geom_point(aes(y= A/10, color = "green", shape = species)) +
  geom_point(aes(y = gsw, color = "blue", shape = species))


