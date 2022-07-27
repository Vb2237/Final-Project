#Install necessary packages
library(tidyverse)
library(janitor)
library(stringr)

#Data Exploration
#Import CSV of DOHMH School Cafeteria Inspections 
cafeteria_inspections <- read_csv("DOHMH_School_Cafeteria_inspections.csv")

#Examine the data
glimpse(cafeteria_inspections)

#Look at a summary of the data
summary(cafeteria_inspections)

#Data Cleaning
cafeteria_clean_names <- cafeteria_inspections %>% #use data from this dataframe
  clean_names() %>% #Clean up column names
  rename(violation_date = inspection_date) #rename inspection date to violation date

#cafeteria_clean_names$Combined <-str_c(cafeteria_clean_names$record_id,'',cafeteria_clean_names$violation_date) %>% 
          
#Goal 1- Explore trends over time

#Create a table showing 
cafeteria_trends <- cafeteria_clean_names %>% #use data from this dataframe
  select(violation_date, borough) %>% #select the columns of interest: violation_date and borough
  group_by(borough) %>% #set up aggregate by borough
  summarize(violations_count = n()) #count of violations

#Export table as CSV for export
write_csv(cafeteria_trends,'Trends.csv')

#Import table to Datawrapper to create a line graph 

#Goal 2- Find out what percentage of total violations are critical level inspections 

#Create a table showing the number of violations per borough
total_by_boro <- cafeteria_clean_names %>% #use data from this dataframe
  select(record_id, school_name, critical_level, borough) %>% #select the columns of interest
  group_by(borough) %>% #set up aggregate by borough
  summarize(all_violations=n()) #count of violations

#Create a second table showing the number of critical level violations per borough      
critical_by_boro <- cafeteria_clean_names %>% #use data from this dataframe
  select(record_id, school_name, critical_level, borough) %>% #select the columns of interest
  group_by(borough) %>% #set up aggregate by borough
  filter(critical_level=='C') %>% #filter by critical level "C"
  summarize(critical_violations=n()) #summarize counts in a column named "critical_violations"

#Join these two tables by borough 
violations <- left_join(total_by_boro,critical_by_boro, by="borough")

#Export result of aggregation and save it as a CSV
write_csv(violations,'Inspections.csv')

#Use Microsoft Excel to calculate percentage of critical inspections by borough

#Export Excel Table and import it into DataWrapper for visualization purposes

#Goal 3- Look at count of violations on a map
cafeteria_map <- cafeteria_clean_names %>% #use data from this dataframe
  select(zip_code, latitude, longitude, critical_level) %>% #select the columns of interest
  filter(critical_level=='C') %>% #filter by critical level "C"
  summarize(zip_code, latitude, longitude) #show these columns in a table

#Export result of aggregation and save it as a CSV
write_csv(cafeteria_map,'cafeteria_map.csv')

#Use Tableau to create map 

#Goal 4- Explore violation descriptions
violation_description <- cafeteria_clean_names %>% #use data from this dataframe
  select(critical_level, violation_description, violation_date) %>% #select the columns of interest
  filter(critical_level=='C') #filter by critical level "C"
  summarize(violation_description, violation_date) #show these columns in a table
  
#Export result of aggregation and save it as a CSV
write_csv(violation_description,'violation_description.csv')

#Use Tableau to create a visualization that shows most popular violation description











