# Workshop 6 Script
# Hannah Chaudhry-Phipps
# 27/02/2023


# 1 Introduction ----------------------------------------------------------


# Rules for tidy data:
# 1. Each row is an observation
# 2. Each column is a variable
# 3. Each value has its own cell

View(mtcars)
# Each car is an observation --> row
# Each variable is a thing we can measure on that car --> column
# Each value has its own cell
mtcars$mpg[mtcars$disp>200]


# 2 Philosophy of tidydata ------------------------------------------------


# Observation - 
# Variable - site, month, species, and number of individuals observed

# Column headers should be variable names, NOT values

beetles1 <- read.csv("dung_beetles_v1.csv")
beetles1
# The column headers are species names, which are values

beetles2 <- read.csv("dung_beetles_v2.csv")
beetles2
# The same again... the column headers are months, which are values

beetles3 <- read.csv("dung_beetles_v3.csv")
beetles3
# The columns site_1 to site_5 are also values; the column headers are NOT 
# variable names, but are values

beetles4 <- read.csv("dung_beetles_v4.csv")
beetles4
# Tidy table; each row is an observation, and each column is a variable, with
# the column header as the variable name (NOT value)

# Problem with beetles1
# To find out number of sites (one type of variable) in the beetles1 table
usites <- unique(beetles1$Site)
length(usites)
?unique # Returns a vector with duplicate elements removed

# To find out number of species (another type of variable) in the beetles1 table
colnames(beetles1)[3:ncol(beetles1)]

# We are having to use two different functions to determine the number of variables on different parts of the dataset

# Problem with beetles3
# Find out number of sites
colnames(beetles3)[3:ncol(beetles3)]

# Find out number of species
species_3 <- unique(beetles3$spp)
length(species_3)

# Again we are having to use two different functions

# Beetles4

# Find out number of sites
usites_4 <- unique(beetles4$Site)
length(usites_4)

# Find out number of species
species_4 <- unique(beetles4$spp)
length(species_4)

# Find out number of months
month_4 <- unique(beetles4$Month)
length(month_4)

# The same function works for counting all the unique values of three variables


# 3 Overviews of Datasets -------------------------------------------------


str(beetles4) 
# Structure of dataset; shows all the columns with the first few values

summary(beetles4)
# Summary statistics of each column

head(beetles4, n = 10)
# Shows the first ten columns of the dataset (if n is not specified, defaults to six)

View(beetles4)  
# Shows the whole dataset in a new tab


# 4 Reading Tables --------------------------------------------------------


# We can use read.table(), and change the default arguments to read files in many different formats
beetlesdf <- read.table("dung_beetles_read_1.csv", sep=",",header=T)  
# Notice how we set the separator - this is telling R that the the data values are seperated
# by columns (as opposed to default white space)
# Also notice how we set header = T - this is telling R the first line of the file contains
# the names of the variables

# If we use the default read.table() arguments:
beetlesdf_experiment <- read.table("dung_beetles_read_1.csv") 
# The data is imported incorrectly

# The following two files will not read if we just use the default settings
read.table("dung_beetles_read_2.txt")
read.table("dung_beetles_read_3.txt")

# To read the second dung beetles file correctly, we need to set the sep to tab
beetlesdf_2 <- read.table("dung_beetles_read_2.txt", header = T, sep = '\t')
View(beetlesdf_2)

# To read the third dung beetles file correctly, we need to use the skip argument
beetlesdf_3 <- read.table("dung_beetles_read_3.txt", header = T, sep = '\t', skip = 1)
View(beetlesdf_3)


# 4 Fill ------------------------------------------------------------------


# Can use the 'tidyr' package (part of the tidyverse package) to fill blank spaces

install.packages('tidyr')
library(tidyr)
?fill 

fill(beetlesdf_2, Site, .direction = 'down')  
beetlesdf_2 <- fill(beetlesdf_2, Site, .direction = 'down') 
View(beetlesdf_2)

# To read the fourth dung beetles file correctly, we need to specify the '-' should be read as NA
beetlesdf_4 <- read.table("dung_beetles_read_4.txt", header = T, na.strings = '-')


# 5 The Pipe --------------------------------------------------------------


# 'Pipes'/chains together a sequence of operations 
# Takes the output of the expression on the left, and passes it to the function on the right
# Note, the dataset name is not repeated when using the pipe
beetlesdf <- read.table("dung_beetles_read_1.csv", sep=",",header=T) %>% fill(Site)


# 6 Pivoting -------------------------------------------------------------


# pivot_longer()
# Sorts out the problem of variables in column headers
# Manipulates the table so column names become variables
# Increases the number of rows, and decreases the number of columns
# So makes datasets LONGER

?pivot_longer
vignette("pivot")

beetlesdf_pivot <- pivot_longer(data=beetlesdf, cols = c("Caccobius_bawangensis", "Catharsius_dayacus", "Catharsius_renaudpauliani", "Copis_agnus", "Copis_ramosiceps", "Copis_sinicus", "Microcopis_doriae", "Microcopis_hidakai"),
                                 names_to="Spp",
                                 values_to = "Count")
# cols argument is the columns we want to pivot into longer format (i.e., into rows)
# names_to argument specifies the new column to create from information specified by cols argument
# values_to argument specifies the name of the column to create from the data stored in cell values 

View(beetlesdf_pivot)

# Tidy up the code:
beetlesdf_pivot <- pivot_longer(data=beetlesdf, cols = c(3:ncol(beetlesdf)),
                                 names_to="Spp",
                                 values_to = "Count")

# tidy-select argument provides a concise dialect for selecting variables based on their names or properties
# E.g.,:
# starts_with("a")
# ends_with("z")
# contains("b")
# matches("x.y") # uses regular expression

# We can use the tidy-select argument to tidy up the code...

beetlesdf_tidy <- pivot_longer(data=beetlesdf, cols = contains("_"), names_to="Spp", values_to = "Count")

View(beetlesdf_tidy)

# pivot_wider()
# Sorts out the problem where multiple variables are stored in one column
# Therefore, adds columns (taking the values from rows), making the table wider

casesdf <- read.table("WMR2022_reported_cases_1.txt", sep="\t", header = TRUE, na.strings = "")
# sep argument tells the function what white space to recognise (in this case a tab) and then strips the space
# na.strings tells the function which strings are to be interpreted as 'NA' values (should be specified after the sep argument)
View(casesdf)

# fill requires specified NA values

casesdf_fill <- fill(casesdf, country)
View(casesdf_fill)

# Can also be written as:
casesdf <- read.table("WMR2022_reported_cases_1.txt", sep="\t", header = TRUE, na.strings = "") %>% fill(country)
View(casesdf)

# Here, each row isn't a SINGLE observation; each observation has been repeated
# Column headings should be variable names i.e., suspected cases, microscopy examined, and microscopy positive
# We want to take values from the 'method' column, and spread them out as individual columns
pivot_wider(casesdf,names_from="method",values_from ="n")


# 7 The Big Challenge -----------------------------------------------------


# Q 1
# Fill empty cells in the country column
casesdf_full <- read.table("WMR2022_reported_cases_2.txt",sep="\t", header = TRUE, na.strings = "") %>% fill(country)
View(casesdf_full)

# Q 2
# Use pivot_longer to move all years into a single column

casesdf_full_longer <- pivot_longer(casesdf_full, cols = contains("X"), names_to = "Year")
View(casesdf_full_longer)

# Q 3
# Use pivot_wider move all the method variables into their own column

casesdf_full_longer_wider <- pivot_wider(casesdf_full_longer, names_from = "method", values_from = "value")
View(casesdf_full_longer_wider)

# Q 4
# Can you use the pipe function to achieve this in a single command?

# For Q 2 and Q 3 combined
# Do not need to repeat the dataset in the second pivot_wider function
casesdf_pipe <- pivot_longer(casesdf_full, cols = contains("X"), names_to = "Year") %>% pivot_wider(names_from = "method", values_from = "value")
View(casesdf_pipe)

# For Q 2, 3, and 4 combined
# Do not need to repeat the dataset in any of the functions
casesdf_full <- read.table("WMR2022_reported_cases_2.txt",sep="\t", header = TRUE, na.strings = "") %>% fill(country) %>% pivot_longer(cols = contains("X"), names_to = "Year") %>% pivot_wider(names_from = "method", values_from = "value")
View(casesdf_full)



