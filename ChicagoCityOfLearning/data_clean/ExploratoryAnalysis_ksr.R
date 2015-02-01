# Objective: Describing current distibution of summer learning opportunities and their overall accesability
library(ggplot2)
library(plyr)
library(geosphere)

######### read in the after-school program data #########
data <- read.csv('schedule_program_export.csv', header = TRUE, stringsAsFactors = FALSE)
data <- data[ ,1:14]

## cleaning the data (probably could use some more cleaning ...)
# remove all rows that don't have valid zipcodes
data$zipcode <- as.integer(data$zipcode)
data <- data[!is.na(data$zipcode), ]
data <- data[data$zipcode > 59999 & data$zipcode < 70000, ]

data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)


# remove all duplicate rows
data <- data[!duplicated(data),]

## initial analysis
zipcode.counts <- ddply(data, .(zipcode), nrow)

# plot # of oppurtunities/zipcode
names(zipcode.counts) <- c("zipcode", "count")
zipcode.plot <- ggplot(zipcode.counts, aes(x = as.factor(zipcode), y = count)) +geom_bar(stat = 'identity')
zipcode.plot <- zipcode.plot + theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),
      axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + xlab("zipcode")
print(zipcode.plot)

# plot # of oppurtunities/zipcode split by category 
category.counts <- ddply(data, .(zipcode, category.name), nrow)
names(category.counts) <- c("zipcode", "category", "count")

category.plot <- ggplot(category.counts, aes(x = as.factor(zipcode), y = count)) + 
        geom_bar(stat = 'identity')+
        facet_wrap(~category, nrow = 4, ncol = 3)
category.plot <- category.plot + theme(axis.title.x = element_text(face="bold", colour="#990000", size=14),
                                     axis.text.x  = element_text(angle=90, vjust=0.5, size=)) + xlab("zipcode")
print(category.plot)

######### read in the school data #########

school <- read.csv('CPS_Schools_2013-2014_Academic_Year.csv', header = TRUE, stringsAsFactors = FALSE)

# remove all rows that don't have valid zipcodes
school$ZIP <- as.integer(school$ZIP)
school <- school[!is.na(school$ZIP), ]
school <- school[school$ZIP > 59999 & school$ZIP < 70000, ]

# remove all duplicate rows
school <- school[!duplicated(school),]


######### merge data #########

CompleteData <- merge(data, school)

### calculate number of programs within a mile for each school

#distHaversine(p1, p2, r=6378137) 
get.dist <- function(l) { #define function get.location for cleaning location data
        p1 <- c(CompleteData$latitude[l], CompleteData$longitude[l])
        p2 <- c(CompleteData$Latitude[l], CompleteData$Longitude[l])
        
        if (length(p1) == 2 & length(p2) == 2) {
        dist <- distHaversine(p1, p2, r=6378137) # calculates shortest distance between each point
        return(dist)}
        else {
                return(NA)
        }}

CompleteData$dist <- sapply(1:nrow(CompleteData), get.dist)  #add column with the distance between program and school
radius <- 1609.34 #1 mile = 1609.34 meters
CompleteData$within_radius <- ifelse(CompleteData$dist < radius, 1, 0)

Programs.accessability.byschool <- ddply(CompleteData, .(ZIP, SchoolName), summarise, withinMile = sum(within_radius, na.rm = TRUE), averageDistance = mean(dist/radius, na.rm = TRUE))


#Next steps: download census data, merge by zipcode (if available), correlate demographics with porgram accessability measures





