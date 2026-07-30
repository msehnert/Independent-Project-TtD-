#Test Visual (full cycle)
#Maggie Sehnert 
#2026-07-16

#setting up----
library(lubridate)
library(ggplot2)
library(dplyr)


#import data----
UTData <- read.csv("2026-07-16-0925_UTProcTest_cleaned.csv") #bucket, fresh
str(UTData)
NoNameData <- read.csv("2026-07-16-0824_NoNameProcTest_cleaned.csv") #detatched, salt
PNNLData <- read.csv("2026-07-16_PNNLProcTest_cleaned.csv") #control, control


#separate into cycles ----
control1 <- PNNLData %>% 
  filter(obs <= 37) %>% 
  mutate(cycle = "control1") %>% 
  select(obs, A, gsw, date, Ci, cycle) 

control2 <- PNNLData %>% 
  filter(obs >= 38) %>% 
  mutate(obs2 = obs-37) %>% 
  mutate(cycle = "control2") %>% 
  select(obs, obs2, A, gsw, date, Ci, cycle)

bucket <- UTData %>% 
  filter(obs <= 37) %>% 
  mutate(cycle = "bucket") %>% 
  select(obs, A, gsw, date, Ci, cycle)

fresh <- UTData %>% 
  filter(obs >= 38) %>% 
  mutate(obs2 = obs - 37) %>% 
  mutate(cycle = "fresh") %>% 
  select(obs2, A, gsw, date, Ci, cycle)

detached <- NoNameData %>% 
  filter(obs <= 37) %>% 
  mutate(cycle = "detached") %>% 
  select(obs, A, gsw, date, Ci, cycle)

salt <- NoNameData %>% 
  filter(obs >= 38) %>% 
  mutate(obs2 = obs - 37) %>%
  mutate(cycle = "salt") %>% 
  select(obs2, A, gsw, date, Ci, cycle)


#graph each test----
ggplot(control1, aes(x = obs, y = A)) + geom_line() + geom_point()
ggplot(control2, aes(x= obs2, y = A)) + geom_line() + geom_point()
ggplot(PNNLData, aes(x= obs, y = A)) + geom_line() + geom_point() + ggtitle("Control over time (both cycles)")
ggplot(detached, aes(x = obs, y = A)) + geom_line() + geom_point()
ggplot(bucket, aes(x= obs, y = A)) + geom_line() + geom_point()
ggplot(salt, aes(x = obs, y = A)) + geom_line() + geom_point()
ggplot(fresh, aes(x= obs, y = A)) + geom_line() + geom_point()


#mega graph---- 
# Source - https://stackoverflow.com/a/36290204
# Posted by eipi10, modified by community. See post 'Timeline' for change history
# Retrieved 2026-07-16, License - CC BY-SA 3.0

ggplot() + 
  geom_line(data = control1, aes(x = obs, y = A), color = 'green') +
  geom_line(data = control2, aes(x = obs2, y = A), color = 'blue') +
  geom_line(data = bucket, aes(x = obs, y = A), color = 'purple') + 
  geom_line(data = detached, aes(x = obs, y = A), color = 'orange') +
  geom_line(data = fresh, aes(x = obs2, y = A), color = 'red') +
  geom_line(data = salt, aes(x = obs2, y = A), color = 'yellow')+ 
  theme_bw() + 
  coord_cartesian(ylim = c(0, 4)) # mega graph

#graph by cycle
#cycle 1
ggplot() + 
  geom_line(data = control1, aes(x = obs, y = A, color = cycle)) +
  geom_line(data = bucket, aes(x = obs, y = A, color = cycle)) + 
  geom_line(data = detached, aes(x = obs, y = A, color = cycle)) + 
  theme_bw() + 
  ggtitle("Cylce 1 (ToD)") +
  coord_cartesian(ylim = c(0, 4))
 

#cycle 2
ggplot() + 
  geom_line(data = control2, aes(x = obs2, y = A, color = cycle)) +
  geom_line(data = fresh, aes(x = obs2, y = A, color = cycle)) +
  geom_line(data = salt, aes(x = obs2, y = A, color = cycle))+ 
  theme_bw() + 
  ggtitle("Cycle 2 (buckets)") + 
  coord_cartesian(ylim = c(0, 4))
   # mega graph

ggplot() + 
  geom_line(data = control1, aes(x = obs, y = A, color = cycle)) +
  geom_line(data = fresh, aes(x = obs2, y = A, color = cycle)) +
  geom_line(data = salt, aes(x = obs2, y = A, color = cycle))+ 
  theme_bw() + 
  ggtitle("Cycle 2 (buckets) (with control1 comparison)") + 
  coord_cartesian(ylim = c(0, 4))





