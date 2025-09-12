# HR Analytics - Statistical Analysis
# Comprehensive analysis of employee data

# Load required libraries
library(tidyverse)
library(ggplot2)
library(randomForest)
library(corrplot)

# Load and clean data
cat("Loading HR data...\n")
hr_data <- read.csv("../data/raw/dataset.csv", sep = ";")

# Clean column names
colnames(hr_data) <- c("employee_id", "first_name", "last_name", "gender", 
                       "state", "city", "education", "birthdate", "hiredate", 
                       "termdate", "department", "job_title", "salary", "performance")

# Data processing
hr_data <- hr_data %>%
  mutate(
    # Calculate age and tenure
    birthdate = as.Date(birthdate, format = "%d/%m/%Y"),
    hiredate = as.Date(hiredate, format = "%d/%m/%Y"),
    termdate = ifelse(termdate == "", NA, as.Date(termdate, format = "%d/%m/%Y")),
    
    age = as.numeric(Sys.Date() - birthdate) / 365.25,
    tenure_years = as.numeric((as.Date(ifelse(is.na(termdate), Sys.Date(), termdate)) - hiredate) / 365.25),
    is_terminated = !is.na(termdate),
    
    # Clean salary
    salary = as.numeric(gsub("[^0-9]", "", salary))
  ) %>%
  filter(!is.na(salary), salary > 30000, salary < 200000)

# Basic analysis
cat("Dataset Summary:\n")
cat("Total employees:", nrow(hr_data), "\n")
cat("Active employees:", sum(!hr_data$is_terminated), "\n")
cat("Turnover rate:", round(mean(hr_data$is_terminated) * 100, 1), "%\n")

# Department analysis
dept_summary <- hr_data %>%
  group_by(department) %>%
  summarise(
    count = n(),
    avg_salary = round(mean(salary), 0),
    turnover_rate = round(mean(is_terminated) * 100, 1)
  ) %>%
  arrange(desc(count))

print("Department Analysis:")
print(dept_summary)

# Gender pay analysis
gender_pay <- hr_data %>%
  group_by(gender) %>%
  summarise(
    count = n(),
    avg_salary = round(mean(salary), 0)
  )

print("Gender Pay Analysis:")
print(gender_pay)

# Performance analysis
performance_summary <- hr_data %>%
  group_by(performance) %>%
  summarise(
    count = n(),
    avg_salary = round(mean(salary), 0),
    percentage = round(n()/nrow(hr_data) * 100, 1)
  )

print("Performance Distribution:")
print(performance_summary)

cat("Analysis complete! Key insights generated.\n")
