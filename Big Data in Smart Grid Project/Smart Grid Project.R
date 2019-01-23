install.packages("ggplot2", dependencies = T) 
install.packages("knitr", dependencies = T)
install.packages("dplyr", dependencies = T)
install.packages("tidyverse", dependencies = T)
install.packages("readxl", dependencies = T)
install.packages("tidyr", dependencies = T)
install.packages("readr", dependencies = T)
install.packages("ggmap", dependencies = T)



library("ggplot2")
library("knitr")
library("dplyr")
library("tidyverse")
library("readxl")
library(tidyr)
library(readr) 
library(ggmap) 


windTurbines <- read.csv("C:/Big Data in Smart Grid Project/windfarms/windTurbines.csv")
abd2012 <- read.csv("C:/Big Data in Smart Grid Project/abd2012/result.csv")


#WINDTURBINES

# Type of turbine mostly used
turbine_type_group <- group_by(windTurbines, type_tower)
turbine_type <- summarise(turbine_type_group, total_count= n_distinct(unique_id))

ggplot(turbine_type, aes(x=type_tower, y=total_count))+ theme(aspect.ratio=2/5,axis.text.x=element_text(angle = -25, hjust = 0)) + 
  geom_bar(stat="identity",colour="#FF9999", fill="#FF9999")


# Find top 10 known manufacturer - Not including unknown manufacturer

manufact_group <- group_by(windTurbines, manufac)
manufact_turbine_cnt <- summarise(manufact_group, total_count= n_distinct(unique_id))
manufact_turbine_cnt <- arrange(manufact_turbine_cnt, desc(total_count))
manufact_turbine_cnt <- manufact_turbine_cnt[manufact_turbine_cnt$manufac != 'unknown',]
top_10 <- manufact_turbine_cnt[1:10,]

ggplot(top_10, aes(x=manufac, y=total_count, fill=manufac))+ theme(aspect.ratio=2/5,axis.text.x=element_text(angle = -25, hjust = 0)) +
  geom_bar(stat = "identity")



#find the top 10 cities against the total number - not including unknown cities
city_group <- group_by(windTurbines, county)
city_turbine_cnt <- summarise(city_group, total_count= n_distinct(unique_id))
city_turbine_cnt <- arrange(city_turbine_cnt, desc(total_count))
city_turbine_cnt <- city_turbine_cnt[city_turbine_cnt$county != 'unknown',]
top_10 <- city_turbine_cnt[1:10,]

ggplot(top_10, aes(x=county, y=total_count, fill=county))+ theme(aspect.ratio=2/5,axis.text.x=element_text(angle = -25, hjust = 0)) +
  geom_bar(stat = "identity")

#ABD2012 

#This map contains comprehensive wind farm project and installation information in the United States through the end of 2012. 
#1146 wind project locations are in service with an installed cumulative wind power capacity of 60,999 MW.
#8 wind project locations are under construction. These locations will add an additional 426 MW of wind power capacity.
#This map contains 1785 total wind farm locations, including: in service, under construction and proposed locations.
#All locations have a combined wind power capacity with potential for generating up to 87,624 MW .


#  find the top 10 cities against the total number of Unit - not including unknown cities
NumberOfUnitdata <- aggregate(abd2012$NumberOfUnits, by=list(Place=abd2012$Place), FUN=sum)
NumberOfUnitdata <- arrange(NumberOfUnitdata, desc(x))
NumberOfUnitdata <- NumberOfUnitdata[NumberOfUnitdata$Place != 'unknown',]
top_10 <- NumberOfUnitdata[1:10,]
ggplot(top_10, aes(x=Place, y=x, fill=x))+ theme(aspect.ratio=2/5,axis.text.x=element_text(angle = -25, hjust = 0)) +
  geom_bar(stat = "identity") + ylab('Total Number Of Units')



#  find the top 10 cities against the total Generating Capacity - not including unknown cities
abd2012$GeneratingCapacity <- as.numeric(abd2012$GeneratingCapacity)
GeneratingCapaci <- aggregate(abd2012$GeneratingCapacity, by=list(Place=abd2012$Place), FUN=sum)
GeneratingCapaci <- arrange(GeneratingCapaci, desc(x))
GeneratingCapaci <- GeneratingCapaci[GeneratingCapaci$Place != 'unknown',]
top_10 <- GeneratingCapaci[1:10,]
top_10$x <- as.factor(top_10$x)
top_10
ggplot(top_10, aes(x=Place, y=x, fill=x))+ theme(aspect.ratio=2/5,axis.text.x=element_text(angle = -25, hjust = 0)) +
  geom_bar(stat = "identity") + ylab('Total Generating Capacity (MW)')



#  find the top 10 Manufacturer against the total number of Unit - not including unknown Manufacturer
manufac_group <- group_by(abd2012, WindTurbineManufacturer)
manufac_turbine_cnt <- aggregate(abd2012$NumberOfUnits, by=list(WindTurbineManufacturer=abd2012$WindTurbineManufacturer), FUN=sum)
manufac_turbine_cnt <- arrange(manufac_turbine_cnt, desc(x))
manufac_turbine_cnt <- manufac_turbine_cnt[manufac_turbine_cnt$x != 'NA',]
top_10 <- manufac_turbine_cnt[1:10,]

ggplot(top_10, aes(x=WindTurbineManufacturer, y=x, fill=WindTurbineManufacturer))+ theme(aspect.ratio=2/5,axis.text.x=element_text(angle = -25, hjust = 0)) +
  geom_bar(stat = "identity") + ylab('Number Of Units')
