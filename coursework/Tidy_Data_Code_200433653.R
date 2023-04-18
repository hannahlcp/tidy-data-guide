# BIO319 Assessment 2 Tidy Data Code
# Hannah Chaudhry-Phipps (200433653)
# 15/03/23

install.packages('tidyr')
install.packages('dplyr')
library(tidyr)
library('dplyr')

# 1 Import, fill and pivot the data

WHO_Disease <- read.table('/cloud/project/assignment_sets_2023/data_cleaning_assignment_200433653.txt', 
                          header = T, 
                          sep = '\t', 
                          na.strings = c('', 'NA')) %>%
  fill(WHO.region.Country.area) %>%
  pivot_longer(cols = c(3:14),
               names_to = 'Year',
               values_to = 'Count') %>%
  pivot_wider(names_from = 'X', 
              values_from = 'Count')

# 2 Rename columns

WHO_Disease <- WHO_Disease %>% 
  rename(Country = WHO.region.Country.area) %>%
  rename(plasmodium_falciparum_cases = 'Total P. falciparum') %>%
  rename(plasmodium_vivax_cases = 'Total P. vivax') %>%
  rename_with(~ gsub('Total ', '', .x)) %>%
  rename_with(~ gsub(' ', '_', .x)) %>%
  rename_with(tolower)

# 3 Clean numerical columns, and cast them to the numerical class

# Function which cleans numbers, and casts them to numerical class
clean_number <- function(x) {as.numeric(gsub('[^-.0-9]', '', x))}

# Clean and cast columns 2 to 9
WHO_Disease <- WHO_Disease %>% 
  mutate(across(!country, clean_number))

# 4 Rename country rows, and convert the country column to a factor

WHO_Disease <- WHO_Disease %>%
  mutate('country' = gsub(' \\(Islamic Republic of\\)|United Republic of |Republic of ', '', country)) %>% 
  mutate('country' = gsub(' ', '_', country)) %>%
  mutate('country' = as.factor(country))

# 5 Export data 

write.table(x = WHO_Disease, 
            file = 'WHO2021_Reported_Cases_Clean_200433653.txt', 
            sep = '\t', 
            col.names = T,
            row.names = F,
            quote = F)
