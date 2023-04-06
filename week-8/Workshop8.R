# Workshop 8 Script
# Hannah Chaudhry-Phipps
# 13/03/2023


# 1 Introduction ----------------------------------------------------------


install.packages('tidyr')
library('tidyr')
install.packages('dplyr')
library('dplyr')


# 2 Select ----------------------------------------------------------------


beetles <- read.table("dung_beetles.csv", sep=",",header=T)

# select() is used to keep or drop columns using their names 
# and types, using the pipe %>%

# Therefore, we can use the select function to get rid of 
# the columns at the end, containing commas, by...

# a) selecting the column numbers
beetles %>% select(1:68)

# b) selecting the Month column, and species columns using grep()
beetles %>% select(c(Month, grep('_', x = colnames(beetles))))

# c) selecting what we do not want, using !
beetles_select <- beetles %>% select(!grep('^X', colnames(beetles)))


# 3 Rename ----------------------------------------------------------------


# rename_with() uses a function to transform the selected columns
# rename() uses new_name = old_name

fixcopris <- function(x) {gsub("opis","opris",x)}

rename_with(beetles, fixcopris)

# Alternatively, without having to explicitly create a function:
rename_with(beetles, ~ (gsub("opis","opris", .x, fixed = TRUE)))

# Pipe the select() and rename() commands together, then add pivot_longer()

# Pipe for select() and rename() commands:
beetles_select %>% select(!grep('^X', colnames(beetles_select))) %>% rename_with(fixcopris) 

# pivot_longer() code:
beetles_pivot <- pivot_longer(beetles_select, cols = c(3:ncol(beetles_select)), names_to = 'Spp', values_to = 'Count')

# Pipe of commands together:
beetles_pivot <- beetles_select %>% select(!grep('^X', colnames(beetles_select))) %>% rename_with(fixcopris) %>% pivot_longer(cols = c(3:ncol(beetles_select)), names_to = 'Spp', values_to = 'Count')

# Make all column names lower case
beetles <- rename_with(beetles_pivot, tolower)


# 4 Separate --------------------------------------------------------------

# Using the separate_wider_delim function to separate columns
beetles %>% separate_wider_delim(cols = 'spp', delim = '_',
                                 names = c('genus', 'species'))


# 5 Mutate ----------------------------------------------------------------


# Can replace values with the mutate() function

# So can replace _ with a space to separate the Genus and species
beetles %>% mutate("spp" = gsub(pattern = '_', replacement = ' ', x = beetles$spp))

# Let us go back to the W.H.O. Word Malaria Report
casesdf <- read.table("WMR2022_reported_cases_3.txt",
                      sep="\t",
                      header=T,
                      na.strings=c("")) %>% 
  fill(country) %>% 
  pivot_longer(cols=c(3:14),
               names_to="year",
               values_to="cases") %>%
  pivot_wider(names_from = method,
              values_from = cases)

# Rename the columns using rename() syntax where new_name = old_name
casesdf <- casesdf %>% rename('suspected' = 'Suspected cases') %>% rename('examined' = 'Microscopy examined') %>% rename('positive' = 'Microscopy positive')

str(casesdf,vec.len=2)

# Need to remove X before year value
casesdf <- casesdf %>% mutate('year' = gsub(pattern = 'X', replacement = '', x = casesdf$year))

str(casesdf)

# Changing format with mutate()

# Convert the year column of the dataframe to a numeric vector
casesdf %>% mutate('year' = as.numeric(casesdf$year))

# Using a pipe
casesdf %>% 
  mutate('year' = gsub('X', '', casesdf$year)) %>% 
  mutate('year' = as.numeric(casesdf$year))

# Now update your the initial mutate function to remove 'X' and convert it to a 
# numerical value (nest R functions)
# Start from the inner function, and work out
casesdf <- casesdf %>% 
  mutate('year' = as.numeric(gsub('X', '', year)))
# Note, we do not NEED to include the dataframe name, as mutate() can figure this out
# So we have just included the column name

# Remove numbers from character columns using re [0-9]
casesdf <- casesdf %>% 
  mutate('country' = gsub(pattern = '[0-9]', replacement = '', x = country))

# Remove characters from number columns using re [^-.0-9]
casesdf %>% 
  mutate('suspected' = gsub('[^-.0-9]', '', suspected))

# Nested function to make numeric too
casesdf <- casesdf %>%
  mutate('suspected' = as.numeric(gsub('[^-.0-9]', '', suspected)))

# Make a function which cleans numbers and casts them to a numerical value
clean_number <- function(x) {as.numeric(gsub('[^-.0-9]', '', x))}

# Mutate across
casesdf <- casesdf %>% mutate(across(c(examined, positive), clean_number))
# The function is applied to each column specified

# Alternatively, select everything except the 'country' column
casesdf %>% mutate(across(!country, clean_number))

# Calculations with mutate()

casesdf %>% mutate(test_positivity = positive / examined) 

# Round to two significant figures:
casesdf %>% 
  mutate(test_positivity = positive / examined) %>%
  mutate(test_positivity = round(test_positivity,2))

# And nesting the function gets:
casesdf <- casesdf %>% 
  mutate(test_positivity = round((positive / examined),2)) 


# 6 Factors ---------------------------------------------------------------


# Use as.factor() and mutate() to convert country to a factor
casesdf %>% mutate('country' = as.factor(country))

# Shows the factor categories
levels(casesdf$country)

# Eritrea has been misspelt; change it
casesdf <- casesdf %>%
  mutate('country' = gsub('Eritrae', 'Eritrea', country))

# Convert it to a factor (as opposed to character)
casesdf <- casesdf %>% 
  mutate('country' = as.factor(country))

levels(casesdf$country)


# 7 Write -----------------------------------------------------------------


?write.table

write.table(x = casesdf, 
            file = 'WMR2022_reported_cases_clean.txt', 
            sep = '\t', 
            quote = FALSE, 
            row.names = FALSE, 
            col.names = TRUE)

read.table('WMR2022_reported_cases_clean.txt')








