# COVID Economic Analysis
# Author: Jason Gonzalez
# Description: Examines relationship between county income differences
# and COVID mortality rates using county-level datasets.

## load libraries 
library(tidyverse)
library(readxl)
library(scales)

countyData <- read_excel("data/unemployment_data.xlsx")
glimpse(countyData)


## filter out the na data and rename columns 

countyData <- countyData %>%
  select(2, 3, 95)

countyData <- countyData %>%
  filter_all(any_vars(!is.na(.))) %>% ## this groups all the caolumns to remove any NA values 
  select(1, 2, 3) %>%
  rename(state = 1, areaName = 2, medianHouseholdIncome = 3)

countyDataCopy <- countyData

## make a seperate frame to hold the seperate county 
stateAvgIncomes <- countyDataCopy %>% 
  filter(!str_detect(areaName, ",") & (state != "US" & state != "State")) %>% 
  rename(avgIncome = medianHouseholdIncome)

stateAvgIncomes

## make a column for the median household income for each state 
countyData <- countyData %>% 
  filter((state != "US" & state != "State")) 

countyData
stateAvgIncomes

joinedCountyData <- full_join(countyData, stateAvgIncomes, by = "state")

glimpse(joinedCountyData)

joinedCountyData <- joinedCountyData %>% rename(county = areaName.x)
joinedCountyData$county <- str_extract(joinedCountyData$county, "[^ ]+") 
joinedCountyData$state <- state.name[match(joinedCountyData$state, state.abb)]

joinedCountyData

## create seperate column with the difference 
joinedCountyData <- joinedCountyData %>% 
  group_by(county) %>% 
  mutate(medianHouseholdIncome = as.numeric(medianHouseholdIncome),
         avgIncome = as.numeric(avgIncome),
         averageDifference = medianHouseholdIncome - avgIncome)

joinedCountyData <- joinedCountyData %>%
  group_by(state) %>%
  mutate(avgStateIncome = mean(medianHouseholdIncome, na.rm = TRUE),
         diff = medianHouseholdIncome - avgStateIncome) 

## read the .txt file and seperate by ","
covidData <- read.table("data/county_data.txt", header = TRUE, sep = ",",  quote = "") ## quote disregards periods 

# Rename the columns
colnames(covidData) <- c("date", "county", "state", "fips", "cases", "deaths")

glimpse(covidData)

## calculate the percentage of covid deaths for each county 
covidData <- covidData %>%
  filter(date == "2022-05-13" & !str_detect(county, "St. Mary"))

covidData <- covidData %>% 
  mutate(percentDeath = deaths/cases)

## sort from highest to lowest percent death 
covidData <- covidData %>%
  filter(county != "Unknown" & county != "Emporia city" & county != "San Augustine" & county != "Jeff Davis") %>% 
  arrange(desc(percentDeath)) %>% 
  slice(1:25)

mergedCountyCovid <- left_join(covidData, joinedCountyData, by = c("county", "state")) 



## do the actual plotting 

mergedCountyCovid %>% 
  ggplot(., aes(x = reorder(county, percentDeath), y = percentDeath, fill = diff)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "#004080", limits = range(-20000, 20000), breaks = c(-10000, 0, 10000), labels = c("$-10,000", "$0", "$10,000"))  +
  geom_text(aes(label = paste(county, state, sep = ", ")), 
            hjust = 1, nudge_y = 0.015, size = 3) + 
  coord_flip() + 
  scale_y_continuous(labels = function(x) paste0(format(x * 100, nsmall = 1), "%"),  # format y-axis labels with one decimal place
                     breaks = seq(0, 0.075, by = 0.025)) + 
  labs(y = "Percentage of Covid Cases Resulting in Death", legend) +  # add y-axis label
  theme_classic() +  # set plot theme to classic
  theme(axis.title.y = element_text(size = rel(0.8)),  # set y-axis label size
        axis.text.y = element_blank(),  # remove y-axis tick labels
        axis.ticks.y = element_blank(),  # remove y-axis tick marks
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),  # remove axis lines
        axis.text.x = element_text(size = rel(0.8)),  # set x-axis label size
        panel.grid.major = element_blank(),  # remove major grid lines
        panel.grid.minor = element_blank(),  # remove minor grid lines
        panel.background = element_rect(fill = "white"),  # set background color to white
        legend.position = "top",  # move legend to top
        legend.justification = "center",  # center align legend
        legend.key = element_rect(fill = NA, color = NA),
        axis.title.x = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.5))) +  # center align legend
  labs(title = "Financial Situations of American Counties with the Highest Rates of Deaths per Covid Cases", size = rel(1.5), face = "bold", subtitle = "The color of the bar indicates whether the median household income of the county falls below or above the average\nhousehold income of all counties in the selected county's state. The value of the color indicates how extreme this difference is.", fill = "", x = "") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold"),  # center align title and set title size
        plot.subtitle = element_text(hjust = 0.5, size = rel(0.8)))  # center align subtitle and set subtitle size

print(last_plot())