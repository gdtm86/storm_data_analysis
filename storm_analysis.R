
# The basic goal of this assignment is to explore the NOAA Storm Database and 
# answer some basic questions about severe weather events.

## load the libraries
library(ggplot2)
library(dplyr)
library(knitr)
library(scales)

## Load the dataset and explore the dataset
fileName <- ("/Users/gmedasani/Documents/courses/Coursera/DataScience-Track/
ReproducibleResearch/Week3/project2/repdata%2Fdata%2FStormData.csv.bz2")
fileName <- gsub("\n", "", fileName)
storm_data <- read.csv(fileName)
dim(storm_data)
names(storm_data)

## Clean the data
# Based on the Questions 1 and 2, we are only intested in the following 
# variables of the storm database
# a. EVTYPE
# b. FATALITIES
# c. INJURIES
# d. PROPDMG
# e. PROPDMGEXP
# f. CROPDMG
# g. CROPDMGEXP

storm_data <- select(storm_data, EVTYPE,FATALITIES,INJURIES,
                           PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)

##### Question 1
## Across the United States, which types of events (as indicated in the 
## EVTYPE variable) are most harmful with respect to population health?

storm_fatalities_injuries_by_evtype <- group_by(select(storm_data, 
                                              EVTYPE,FATALITIES,INJURIES), 
                                       EVTYPE)

storm_affected_population <- summarise(storm_fatalities_injuries_by_evtype,
                             total_fatalities = sum(FATALITIES, na.rm=TRUE),
                             total_injuries = sum(INJURIES, na.rm=TRUE))

storm_affected_population <- select(mutate(storm_affected_population, 
                affected_population = total_fatalities +
                total_injuries), weather_event_type = 
                        EVTYPE,affected_population)

storm_affected_population_ordered <- arrange(storm_affected_population, 
                                             affected_population)

top5_evtypes_by_affected_population <- tail(storm_affected_population_ordered
                                             ,5)

top5_evtypes_by_affected_population_formatted <- mutate(
        top5_evtypes_by_affected_population,
                                        affected_population = 
                                        comma(affected_population))

### Top 5 Weather Events by Affected Population
p1 <- ggplot(data = top5_evtypes_by_affected_population, 
             aes(x = affected_population, y = weather_event_type))

plot1 <- p1 + 
geom_point(aes(size = top5_evtypes_by_affected_population$affected_population,
        colour = top5_evtypes_by_affected_population$weather_event_type,
        label = top5_evtypes_by_affected_population$affected_population)) + 
geom_text(aes(label = 
        top5_evtypes_by_affected_population_formatted$affected_population), 
        size = 4) +
scale_size(range = c(10,25)) +
scale_y_discrete(limits = 
         top5_evtypes_by_affected_population$weather_event_type)+
theme_bw()

#Print the plot
plot1


##### Question 2
## Across the United States, which types of events have the greatest 
## economic consequences?

findMagnitude <- function(magnitudeCharacter){
        magnitudeCharacter <- toupper(as.character(magnitudeCharacter))
        multiplyNumber <- 0
        if ( magnitudeCharacter == 'B'){
                multiplyNumber <- 1000000000
        }else if (magnitudeCharacter == 'M'){
                multiplyNumber <- 1000000
        }else if (magnitudeCharacter == 'K'){
                multiplyNumber <- 1000
        }else if (magnitudeCharacter == 'H'){
                multiplyNumber <- 100
        }else{
                multiplyNumber <- 1
        }
        return(multiplyNumber)
}

storm_data['PROPDMGEXP'] <- sapply(storm_data$PROPDMGEXP, 
                                             findMagnitude)
storm_data['CROPDMGEXP'] <- sapply(storm_data$CROPDMGEXP,
                                             findMagnitude)
storm_data <- select(mutate(storm_data, 
                               PROPDMG = as.numeric(PROPDMG*PROPDMGEXP),
                               CROPDMG = as.numeric(CROPDMG*CROPDMGEXP),
                               damage_in_dollars = PROPDMG + CROPDMG),
                               weather_event_type = EVTYPE,
                               damage_in_dollars)
storm_damage_by_evtype <- summarise(group_by(storm_data,weather_event_type), 
                                total_damage_in_dollars = 
                                sum(damage_in_dollars, na.rm = TRUE))

storm_damage_by_evtype_ordered <- arrange(storm_damage_by_evtype,
                                          total_damage_in_dollars)

top5_evtypes_by_economic_damage <- tail(storm_damage_by_evtype_ordered,5)

top5_evtypes_by_economic_damage_formatted <- 
        mutate(top5_evtypes_by_economic_damage,
        total_damage_in_dollars = 
        dollar(total_damage_in_dollars))

### Top 5 Weather Events by Economic Damage (in Dollars)
p2 <- ggplot(data = top5_evtypes_by_economic_damage, 
             aes(x = total_damage_in_dollars, y = weather_event_type))

plot2 <- p2 + 
geom_point(aes(size = top5_evtypes_by_economic_damage$total_damage_in_dollars,
           colour = top5_evtypes_by_economic_damage$weather_event_type,
           label = top5_evtypes_by_economic_damage$total_damage_in_dollars)) + 
geom_text(aes(label = 
        top5_evtypes_by_economic_damage_formatted$total_damage_in_dollars), 
        size = 4) +
scale_size_continuous(range = c(5,25)) + 
scale_y_discrete(limits = top5_evtypes_by_economic_damage$weather_event_type)+
theme_bw()

#Print the plot
plot2


