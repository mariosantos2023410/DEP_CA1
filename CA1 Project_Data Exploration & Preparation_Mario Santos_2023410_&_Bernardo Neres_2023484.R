# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
covid_dataset <- read.csv("covid_dataset.csv")

# Let's remove the note column and rate_14_day
covid_New <- covid_dataset[, !(names(covid_dataset) %in% c("note", "rate_14_day"))]

# Check for missing values
mv <- colSums(is.na(covid_New))
mv

# Replace missing values with the mode for the "country_code" variable
covid_New$country_code <- ifelse(is.na(covid_New$country_code), mode(covid_New$country_code), covid_New$country_code)

# Identify variable types
categorical_cols <- c("country", "country_code", "continent", "indicator", "year_week")  
discrete_cols <- c("weekly_count", "cumulative_count")  # Discrete variables
continuous_cols <- c("population", "rate_14_day")  # Continuous variables

# Create a data frame to represent variable types
variable_types <- data.frame(
  Variable = c(categorical_cols, discrete_cols, continuous_cols),
  Type = c(rep("Categorical", length(categorical_cols)),
           rep("Discrete", length(discrete_cols)),
           rep("Continuous", length(continuous_cols)))
)

# Create a bar plot to visualize variable types
ggplot(variable_types, aes(x = Variable, fill = Type)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Variable Types Bar Plot",
       x = "Variable",
       y = "Count",
       fill = "Type") +
  scale_fill_manual(values = c("Categorical" = "blue", "Discrete" = "green", "Continuous" = "red"))
