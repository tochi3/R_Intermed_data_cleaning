# Intermediate R - data cleaning 
# Instructors:
#   Ameet Doshi (Public Policy): 
#     ameet.doshi@library.gatech.edu
#   Jay Forrest (Earth and Atmospheric Sciences|International Affairs): 
#     jay.forrest@library.gatech.edu

# Packages - expansions to R code for specific operations, package names are case sensitive
install.packages("tidyverse") # a set of data science tools including dplyr, tidyr and stringr
install.packages("skimr") # a package to facilitate data summaries
install.packages("nycflights13") # contains data for NYC flights from 2013
install.packages("ggplot2")
install.packages("caret")
install.packages("fastDummies")

library(tidyverse)
library(skimr)
library(nycflights13)
library(caret)
library(fastDummies)
library(ggplot2)


# R Quick Overview
# R Style Guide: https://google.github.io/styleguide/Rguide.xml

# Data Wrangling Cheat Sheet
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# Punctuation
# Quotes:  Generally Double Quotes are standards for character data types, 
# unless the value already includes a quote, then use single quotes around the entire phrase

# Semicolons are not need for end of line, they can be used to separate commands on a single line,
# but this practice is not recommended

# Functions and arguments
help(help)

# Summary statistics

# Built-in data set 
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/sleep.html
data(sleep)

# Summary statistics
summary(sleep)

# Basic Plots
hist(sleep$extra)
plot(sleep$group, sleep$extra)

# Mean and Standard Deviation
# extra is the numeric increase in hours of sleep 
# based on the administration of a soporific

mean(sleep$extra)
sd(sleep$extra) 

# Centering the Means with scale
sleep$scale_extra <- scale(sleep$extra, center=T, scale=F)
hist(sleep$scale_extra)

# Z scores
sleep$z_extra <- scale(sleep$extra, center=T, scale=T)
hist(sleep$z_extra)

# dataFrame a list of vectors of equal length i.e. a table
# we installed this data with the nycflights13 library
# Reference Guide:  https://cran.r-project.org/web/packages/nycflights13/index.html

flights

##### dplyr
# dplyr using the NYC flights data
# For dplyr we are going to explore filtering, arrange, group, and mutate functions
# This data set has missing values in several thousand observations on several variables
summary(flights) 
glimpse(flights)

### Filter
# filter : filter allows to view a subset of rows the data frame
flightsJFK <- filter(flights, origin=="JFK")
# We can use is.na to pull just observations with NA values
flightsDepTimeNA <- filter(flights, is.na(dep_time))
# or exclude them (n.b. this is only excluding NA on a single column)
flightsCleanDepTime <- filter(flights, !is.na(dep_time))

### Select
# we can also subset of columns using select
flightsCarrierOriginDest <- select(flights, carrier, origin, dest)
summary(flightsCarrierOriginDest)
factor(flightsCarrierOriginDest$carrier) # how many categories
# factor shows us the 16 unique values for the $carrier
table(flightsCarrierOriginDest$carrier)  # categories and frequency
# table shows us the unique values and the frequency of use

### Arrange
# Arrange allows us to change the order of the data
flightsSortDepTime <- arrange(flights, dep_time)
flightsSortDepTime <- arrange(flights, desc(dep_time))

### Group and Summary
# lets create a data frame of that groups the data by month
flightsByMonth <- group_by(flights, year, month)
# now lets look at the number of flights and average departure delay
summarise(flightsByMonth, count = n(), delay = mean(dep_delay, na.rm=TRUE))

### Mutate
# mutate allows us to create a new column at the end of the data frame
# as a function of existing columns
flights <- mutate(flights, speed = distance / air_time * 60)

# Joins (https://r4ds.had.co.nz/relational-data.html)
# This join adds a column 'names' that is the full version of the carrier abbreviation
# R selects the appropriate column based on the same column name
flights <- left_join(flights, airlines)
table(flights$name)

# We can also specify the join information, so if we wanted to bring in weather information
# we use by to specify the relationship, and use the c() concatenation operator to base
# the join on multiple columns
flights <- left_join(flights, weather, by=c('origin' = 'origin', 'time_hour'='time_hour'))

# Descriptive statistics before data cleaning
skim(flights)

# Data cleaning 
cleanFlights <- flights[complete.cases(flights),]  # only select complete data rows for the cleandata frame
cleanFlights <- unique(cleanFlights) # eliminate duplicate values (none in our example file)
View(cleanFlights)

# Descriptive statistics after data cleaning
skim(cleanFlights)

# Normalizing the Data 
# Normalizing makes the scale of numeric values equivalent.
# We used scale() earlier, but the caret library
# gives us a more powerful tool with preProcess()
# Caret is a great packages on its own: 
# https://cran.r-project.org/web/packages/caret/vignettes/caret.html
preProcess <- preProcess(cleanFlights, method=c("center", "scale"))

normFlights <- predict(preProcess, cleanFlights)
summary(normFlights)
summary(cleanFlights)

normFlights2 <- fastDummies::dummy_cols(normFlights, select_columns = "carrier")
skim(normFlights2)

# we won't cover imputation today (we removed null values)
# but it some cases it is appropriate to calculate them
# The MICE library is a good tool for that approach
# https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/

# Lets remove the flights data to clean things up
rm(list=ls())

# tidyr
# Tidy Data
# Following three rules makes a dataset tidy: 
# -- variables are in columns
# -- observations are in rows
# -- and values are in cells.
# For tidyr we are going to demonstrate how to reshape data 
# and to separate values across multiple columns
# Lets take a look at the tables we will be working with:
# these tables are part of the tidyverse library and are for TB reported cases

# BTW tibbles
# vignette("tibble")

table1

table2

table3

table4a

table4b

# each table shows the same values of four variables:
#country, year, population, and cases, 
# but each dataset organizes the values in a different way

# We want to "tidy" our data such that it adheres to three rules:

# Each variable must have its own column.
# Each observation must have its own row.
# Each value must have its own cell.

# Compute rate per 10,000
table1_exp <- table1 %>%  
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>%
  count(year, wt = cases)

# Note that we are not actually modifying table1 here - why?
table1

# Visualize changes over time using ggplot2
ggplot(table1, aes(year, cases, population)) +  
  geom_line(aes(group = country), color = "grey50") +  
  geom_point(aes(color = country))

## SPREADING AND GATHERING ##

# Two commons problems with untidy data:
# 1) One variable might be spread across multiple columns.
# 2) One observation might be scattered across multiple rows.

# Lets tidy up table4a - what's the problem here?
table4a

# We need to gather those columns into a new pair of variables

# We will be working with three parameters:

# FIRST: The set of columns that represent values, not variables. 
# In this example, those are the columns 1999 and 2000.

# SECOND: The name of the variable whose values form the column names. 
# In tidyr that is called the "key" and for our purposes we will designate it as "year"

# THIRD: The name of the variable whose values are spread over the cells. 
# In tidyr that argument is called "value" and here it’s the number of cases.

# These are the arguments that are used in the gather() command in tidyr

table4aClean <- table4a %>%  
  gather(`1999`, `2000`, key = "year", value = "cases")

# Lets compare

table4a
table4aClean

# Now, on your own, clean up table4b





table4bClean <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

table4b
table4bClean

# We will use a dplyr command to join these two clean tables into a single table
# with variables names as columns

table4Clean <- left_join(table4aClean, table4bClean)

table4Clean

# How would you visualize changes over time?

ggplot(table4Clean, aes(year, cases)) +  
  geom_line(aes(group = country), color = "grey50") +  
  geom_point(aes(color = country))

## SPREADING

table2

# The spread() command has two parameters:

# 1) The column that contains variable names, the "key" column. Here, it’s "type".
# 2) The column that contains values forms multiple variables, the value column. Here, it’s count.

spread(table2, key = type, value = count)

# This results in a cleaner data set

# SEPARATE (time permitting)
# We notice that table3 has some weirdness going on...

table3

# Use the separate() command to separate the rate variable
# into two new variables: cases and population

table3 %>%  
  separate(rate, into = c("cases", "population"), 
           convert = TRUE)

help(separate)


