# This is to load the libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# This is to load the data set
covid_dataset <- read.csv("covid_dataset.csv")
view(covid_dataset)

#EXPLORING THE DATA IN ORDER TO PREPARE THE DATA.

#This shows how many rows and columns this data set has.
dim(covid_dataset)


#This displays the name of the variables
names(covid_dataset)

#This displays the type of each one of the variables
glimpse(covid_dataset)

#CLEANING THE DATA SET
# This is to check for missing values
#NA stand for "Not Available" and it means that the data is missing
#the data it was not collected and we don't know anything about it
mv <- colSums(is.na(covid_dataset))
mv


#This is to calculate the percentage of missing values for one of the each colums from the data set
#As the percentage of "note" column is 100% the column will be removed from the data set
mv_percentage <- colMeans(is.na(covid_dataset)) *100
print(mv_percentage)

# This is to remove the "Note" column from the original data set and will generate a new data set without the column "Note"
#This column is being removed because the entire column is fulfilled of missing values
covid_New <- covid_dataset[, !(names(covid_dataset) %in% c("note"))]

# This is to replace missing values with the mode for the "country_code" variable
covid_New$country_code <- ifelse(is.na(covid_New$country_code), mode(covid_New$country_code), covid_New$country_code)

# This is to identify variable types
categorical_cols <- c("country", "country_code", "continent", "indicator", "year_week", "source")  # Categorical variables
discrete_cols <- c("weekly_count", "cumulative_count")  # Discrete variables
continuous_cols <- c("population", "rate_14_day")  # Continuous variables


# This is to create a data frame to represent variables types if they are 
# Categorical, Discrete or Continuous
variable_types <- data.frame(
  Variable = c(categorical_cols, discrete_cols, continuous_cols),
  Type = c(rep("Categorical", length(categorical_cols)),
           rep("Discrete", length(discrete_cols)),
           rep("Continuous", length(continuous_cols)))
)

# This is to create a bar plot to visualize variable types
ggplot(variable_types, aes(x = Variable, fill = Type)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Variable Types Bar Plot",
       x = "Variable",
       y = "Count",
       fill = "Type") +
  scale_fill_manual(values = c("Categorical" = "blue", "Discrete" = "green", "Continuous" = "red"))

#This is to calculate the statistical parameters(mean, median, minimum, maximum and standard deviation)
# The calculation is done for each of the numerical variables in the data set

#this is to calculate the statistical parameters of the integer variable "population" from the data set
summary(covid_New$population)
boxplot(covid_New$population)
hist(covid_New$population)

#this is to calculate the standard deviation of the "population" integer variable from the data set
calculate_standard_deviation <- sd(covid_New$population)
print(calculate_standard_deviation)

#this is to calculate the statistical parameters of the integer variable "weekly_count" from the data set
summary(covid_New$weekly_count)


#this is to calculate the standard deviation of the "weekly_count" integer variable from the data set
calculate_standard_deviation <- sd(covid_New$weekly_count)
print(calculate_standard_deviation)

#this is to calculate the statistical parameters of the numeric variable "rate_14_day" from the data set
summary(covid_New$rate_14_day)


#this is to calculate the standard deviation of the "weekly_count" integer variable from the data set excluding missing values
calculate_standard_deviation <- sd(covid_New$rate_14_day, na.rm = TRUE)
print(calculate_standard_deviation)


#this is to calculate the statistical parameters of the integer variable "cumulative_count" from the data set
summary(covid_New$cumulative_count)


#this is to calculate the standard deviation of the "cumulative_count" integer variable from the data set excluding missing values
calculate_standard_deviation <- sd(covid_New$cumulative_count, na.rm = TRUE)
print(calculate_standard_deviation)


