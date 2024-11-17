#Load Libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(DMwR)
library(DMwR2)

#read dataset (csv)
studentDropoutData <- read.csv("student dropout_kaggle.csv")
studentDropoutData

#getting an overview of the dataset
glimpse(studentDropoutData)

#inspecting the dataset
str(studentDropoutData)
summary(studentDropoutData)
head(studentDropoutData, n=3)

#check missing values
colSums(is.na(studentDropoutData))

#Handling Categorical Variables
##perform OHE
studentDropoutData$Gender <- ifelse(studentDropoutData$Gender == "M", 1, 0)  #Male = 1, Female = 0
studentDropoutData$Address <- ifelse(studentDropoutData$Address == "Urban", 1, 0) #Urban =1, Rural =0
###run head() to check changes


dummies <- model.matrix(~ School + Reason_for_Choosing_School - 1, data = studentDropoutData)
data <- cbind(studentDropoutData, dummies)  # Add the new dummy variables to the dataset
data <- data %>% select(-School, -Reason_for_Choosing_School)

#double checking
head(data)
glimpse(data)

##Normalising numerical data
#identifying numerical columns
all_numerical_columns <- select_if(studentDropoutData, is.numeric)
all_numerical_columns
#rm(numerical_columns)

# Selected numeric columns to normalize
numeric_cols <- c("Age", "Number_of_Absences", "Grade_1", "Grade_2", "Final_Grade")
# Normalize numerical columns to have mean = 0 and standard deviation = 1
data[numeric_cols] <- scale(data[numeric_cols])

# Use a boxplot to visualize outliers in the "Number_of_Absences" column (descriptive statistics)
boxplot(data$Number_of_Absences, main = "Outliers in Number of Absences")

# Remove rows where "Number_of_Absences" is greater than the 99th percentile
data <- data %>% filter(Number_of_Absences < quantile(Number_of_Absences, 0.99))
# Filters out rows with extreme values in "Number_of_Absences"

##Feature Engineering
#Combining father and mother education
data$Total_Parental_Education <- data$Mother_Education + data$Father_Education
# Creates a consolidated measure of the parents' education levels

# Calculate the average grade for each student using Grade_1, Grade_2, and Final_Grade
data$Average_Grade <- rowMeans(data[, c("Grade_1", "Grade_2", "Final_Grade")])
# Provides a single metric summarizing academic performance

##Correlation Analysis
# Calculate the correlation matrix for numerical columns
cor_matrix <- cor(data %>% select_if(is.numeric))

# Visualize the correlation matrix
corrplot(cor_matrix, method = 'color')
# Creates a heatmap to show the strength of relationships between variables

# Save the dataset to a CSV file for correlation tool processing online
write.csv(data, "correlation_student_data.csv", row.names = FALSE)

# Check the distribution of the "Dropped_Out" target variable
table(data$Dropped_Out)
# Displays the counts for each class (e.g., 0 = didn't drop out, 1 = dropped out)

# Use SMOTE to handle imbalance (if applicable)
balanced_data <- SMOTE(Dropped_Out ~ ., data = data, perc.over = 100, perc.under = 200)
# Oversamples the minority class (Dropped_Out = 1) to balance the dataset
