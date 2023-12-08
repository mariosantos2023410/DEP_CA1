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
view(covid_New)

# This is to identify duplicates on specific columns
duplicates <- duplicated(covid_New[, c("country", "country_code", "continent",
                                       "population", "indicator", "weekly_count", "year_week", "rate_14_day", 
                                       "cumulative_count", "source")])

#This is to Localize duplicates in the data frame
duplicates_rows <- covid_New[duplicates, ]

#This is to print the data frame with duplicates
print(duplicates_rows)

#This is to replace missing values with the mode for the variables
covid_New$country_code <- ifelse(is.na(covid_New$country_code), mode(covid_New$country_code), covid_New$country_code)
covid_New$weekly_count <- ifelse(is.na(covid_New$weekly_count), mean(covid_New$weekly_count, na.rm = TRUE), covid_New$weekly_count)
covid_New$rate_14_day <- ifelse(is.na(covid_New$rate_14_day), mean(covid_New$rate_14_day, na.rm = TRUE), covid_New$rate_14_day)
covid_New$cumulative_count <- ifelse(is.na(covid_New$cumulative_count), mean(covid_New$cumulative_count, na.rm = TRUE), covid_New$cumulative_count)

#This is to check the missing values after handling and treating them.
mv <- colSums(is.na(covid_New))
mv
view(covid_New)

# This is to identify variable types
categorical_cols <- c("country", "country_code", "continent", "indicator", "year_week", "source")  # Categorical variables
discrete_cols <- c("weekly_count", "cumulative_count")  # Discrete variables
continuous_cols <- c("population", "rate_14_day")  # Continuous variables

#This is to create a data frame to represent variables types if they are 
#Categorical, Discrete or Continuous
variable_types <- data.frame(
  Variable = c(categorical_cols, discrete_cols, continuous_cols),
  Type = c(rep("Categorical", length(categorical_cols)),
           rep("Discrete", length(discrete_cols)),
           rep("Continuous", length(continuous_cols)))
)

#This is to create a bar plot to visualize variable types
ggplot(variable_types, aes(x = Variable, fill = Type)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Variable Types",
       x = "Variable",
       y = "Count",
       fill = "Type") +
  scale_fill_manual(values = c("Categorical" = "blue", "Discrete" = "green", "Continuous" = "red"))

#This is to calculate the statistical parameters(mean, median, minimum, maximum and standard deviation)
# The calculation is done for each of the numerical variables in the data set

#This is to calculate the statistical parameters of the integer variable "population" from the data set
summary(covid_New$population)
#This is to show the box plot with the outliers of the "population" column
boxplot(covid_New$population)
#This is to show the histogram of the "population" column
hist(covid_New$population)

#This is to calculate the standard deviation of the "population" integer variable from the data set
cal_sd <- sd(covid_New$population)
print(cal_sd)

#this is to calculate the statistical parameters of the integer variable "weekly_count" from the data set
summary(covid_New$weekly_count)
#This is to show the box plot with the outliers of the "weekly_count" column
boxplot(covid_New$weekly_count)
#This is to show the histogram of the "weekly_count" column
hist(covid_New$weekly_count)
#this is to calculate the standard deviation of the "weekly_count" integer variable from the data set
calc_sd <- sd(covid_New$weekly_count)
print(calc_sd)

#this is to calculate the statistical parameters of the numeric variable "rate_14_day" from the data set
summary(covid_New$rate_14_day)
#This is to show the box plot with the outliers of the "rate_14_day" column
boxplot(covid_New$rate_14_day)
#This is to show the histogram of the "rate_14_day" column
hist(covid_New$rate_14_day)
#this is to calculate the standard deviation of the "weekly_count" integer variable from the data set excluding missing values
calc_sd <- sd(covid_New$rate_14_day)
print(calc_sd)


#This is to calculate the statistical parameters of the integer variable "cumulative_count" from the data set
summary(covid_New$cumulative_count)
#This is to show the box plot with the outliers of the "cumulative_count" column
boxplot(covid_New$cumulative_count)
#This is to show the histogram of the "cumulative_count" column
hist(covid_New$cumulative_count)
#This is to calculate the standard deviation of the "cumulative_count" integer variable from the data set excluding missing values
calc_sd <- sd(covid_New$cumulative_count)
print(cal_sd)


#This is a scatter Plot for the correlation of Weekly Count Vs Rate 14 Day in Ireland
# This is to filter data for Ireland
eir_info <- covid_New[covid_New$country == "Ireland", ]
# This is to load the library
library(ggplot2)
#This is to show off the plot
ggplot(eir_info, aes(x = weekly_count, y = rate_14_day)) +
  geom_point() +
  labs(title = "Scatter Plot: Weekly Count vs Rate 14 Day (Ireland)",
       x = "weekly_count",
       y = "rate_14_day")

# This is to show the correlation for All Countries in the EU Zone to show off Rate 14 Day Over Time
library(ggplot2)
ggplot(covid_New, aes(x = year_week, y = rate_14_day, group = 1)) +
  geom_line() +
  labs(title = "Line Plot: Rate 14 Day Over Time For All Countries in Europe",
       x = "Year-Week",
       y = "Rate 14 Day")


#This is to show the correlation in Ireland using Line Plot to show-off Rate 14 Day Over Time
# This is to filtering data in Ireland
ireland <- covid_New[covid_New$country == "Ireland", ]
# This is the line plot for Ireland
library(ggplot2)
ggplot(ireland, aes(x = year_week, y = rate_14_day, group = 1)) +
  geom_line() +
  labs(title = "Line Plot: Rate 14 Day Over Time in Ireland",
       x = "Year-Week",
       y = "Rate 14 Day")


# This is to calculate the correlation matrix between the features of the data set
corr_mat <- cor(covid_New[, c('population', 'weekly_count', 'rate_14_day', 'cumulative_count')], use = "complete.obs")

# This is to visualize the correlation matrix with a heat map
library(corrplot)
corrplot(corr_mat, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# This is to import the library
install.packages("caret")
#This is to load the library
library(caret)
# This is to select the respectively columns of the data set
num_cols <- c("population", "rate_14_day", "cumulative_count", "weekly_count")
# This is to apply Min-Max Scaling
covid_New[num_cols] <- sapply(covid_New[num_cols], function(x) ifelse((x), 0.5, (x - min(x)) / (max(x) - min(x))))
# This is to view the head of the normalized data
head(covid_New)


# This is to select the respectively columns of the data set
num_cols <- c("population", "rate_14_day", "cumulative_count", "weekly_count")
# This is to find the Mean and Standard Deviation for each column
mean_values <- colMeans(covid_New[num_cols])
sd_values <- apply(covid_New[num_cols], 2, sd)
# This is to apply z-score Standardization
covid_New[num_cols] <- scale(covid_New[num_cols], center = mean_values, scale = sd_values)
# This is to view the standardized data
head(covid_New)


# This is the Robust Scalar Calculation
# This is extracting numerical columns
num_cols <- c("population", "rate_14_day", "cumulative_count", "weekly_count")
# This is to apply the method Robust Scaling
pp_params <- preProcess(covid_New[num_cols], method = c("center", "scale"))
# This is to apply the learned parameters to the original data
covid_New_robust <- predict(pp_params, newdata = covid_New[num_cols])
# This is to view the head of the Robust Scaled data
head(covid_New_robust)

#This is an application of Dummy Encoding in one of the categorical variables of data set
# Load the necessary library

# This is to install the package
install.packages("fastDummies")

# Load the library
library(fastDummies)

# Country code is the categorical variable for dummy encoding
cat_cols <- c("country_code")

# This is to apply one-hot encoding
covid_New_enc <- dummy_cols(covid_New, select_columns = cat_cols)

# This is to view the encoded dataset
head(covid_New_enc)


#This is to Apply Principal Component Analysis to this dataset
#Numerical Columns are selected for PCA
num_cols <- c("population", "weekly_count", "rate_14_day", "cumulative_count")
dt_pca <- covid_New_enc[, num_cols]

# Check for missing values
mv <- any(is.na(dt_pca))

if (mv) {
  # Handle missing values 
  # Replacing the missing values with column means
  dt_pca[is.na(dt_pca)] <- colMeans(dt_pca, na.rm = TRUE)
}

#This is the Data standardization
scaled_dt <- scale(dt_pca)

# Three components applied to run PCA
num_comp <- 3
pca_res <- prcomp(scaled_data, center = TRUE, scale. = TRUE, rank. = num_comp)

# This is to extract the principal components
pp_comp <- pca_res$x

# To run the results
summary(pca_res)
View(pca_res)

# This is to create a dataframe with Team Members and Spent Hours during the Project
Team_Member <- data.frame(
  Team_Member = c("Mario", "Bernardo"),
  Spent_Hours = c(31, 23)
)

# This will generate a bar chart with the details
barplot(
  height = Team_Member$Spent_Hours, 
  names.arg = Team_Member$Team_Member,
  col = c("blue", "green"), 
  main = "Effort and Time Spent during This Project",
  xlab = "Team Members", 
  ylab = "Hours Spent", 
  ylim = c(0, max(Team_Member$Spent_Hours) + 5)
)

# These are the labels
text(
  x = barplot(
    height = Team_Member$Spent_Hours, 
    col = c("blue", "green"), 
    add = TRUE
  ) + 0.25,
  y = Team_Member$Spent_Hours + 1,
  labels = Team_Member$Spent_Hours,
  col = "black",
  pos = 3  
)

# These are the legends
legend("topright", legend = Team_Member$Team_Member, fill = c("blue", "green"), inset = 0.02)