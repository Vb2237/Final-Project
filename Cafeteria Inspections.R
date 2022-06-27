#Install the necessary packages
library(tidyverse)
library(janitor)

# Import CSV of School Cafeteria Inspections
cafeteria_inspections <- read_csv("DOHMH_School_Cafeteria_inspections.csv")

#Clean up column names
cafeteria_clean_names <- clean_names(cafeteria_inspections)

#Now let's do some light analysis on this data

#Select the columns of interest 
cafeteria_selected <- select(cafeteria_clean_names, school_name, borough, zip_code, critical_level,inspection_date)

#Filter to look at the critical level violations
cafeteria_filtered <- filter(cafeteria_selected,critical_level=='C')

#Mutate a column with the count of inspections since the data set does not provide it
cafeteria_mutate <- mutate(cafeteria_filtered, inspections= as.numeric("1"))

#Group by borough
cafeteria_boro <- group_by(cafeteria_mutate,borough)

#Summarize how many critical level violations there are
cafeteria_boro <- summarize(cafeteria_boro, total_inspections = sum(inspections))

# Now do the same analysis, but in one line of code and making only one new table
cafeteria_tot_inspections <- cafeteria_inspections %>%
  clean_names() %>% 
  select(school_name, borough, zip_code, critical_level,inspection_date) %>% 
  filter(critical_level=='C') %>% 
  mutate(inspections= as.numeric("1")) %>% 
  group_by(borough) %>% 
  summarize(total_inspections = sum(inspections))

#The table created named "cafeteria_tot_inspections" shows the total number of inspections per borough

#Export result oof aggregation
write_csv(cafeteria_tot_inspections,'CafeteriaInspections.csv')
