# Load required libraries
library(dplyr)
library(stringr)

# Load data
data <- read.csv("retail_data.csv")
View(data)

# Clean Transaction_ID and Phone in one pipeline
clean_data <- data %>%
  mutate(
    # Clean Phone: Remove non-numeric characters
    Phone_clean = gsub("[^0-9]", "", Phone),
    
    Country = str_squish(Country),# Remove extra spaces
    Country = str_to_title(Country), # Capitalize properly
    
    # Convert Age to numeric (in case it's stored as character)
    Age = as.numeric(Age),
    
    # Remove non-numeric characters like $, commas
    Income = as.numeric(gsub("[^0-9.-]", "", Income)
    
  ) %>%
  filter(
    # Filter Transaction_ID
    !is.na(Transaction_ID),
    Transaction_ID != "",
    !duplicated(Transaction_ID),
    !grepl("[a-zA-Z]", Transaction_ID),
    
    # Filter Phone
    !is.na(Phone_clean),
    Phone_clean != "",
    grepl("^[0-9]+$", Phone_clean),
    nchar(Phone_clean) >= 10,
    nchar(Phone_clean) <= 12,
    
    # Filter City: Remove NA or empty string
    !is.na(City),
    City != "",
    
    # Filter State: Remove NA or empty string
    !is.na(State),
    State != "",
    
    # Filter Zipcode: Remove non-numeric values and check length
    !is.na(Zipcode),
    Zipcode != "",
    
    # Filter Country: Remove non-numeric values and check length
    !is.na(Country),
    Country != "",
    
    # Filter out NA, negative, zero, or implausibly high ages
    !is.na(Age), 
    Age > 0, 
    Age <= 100,
    Age != "",
    
    # Clean Gender column
    !is.na(Gender),
    Gender != "",
    
    # Clean Income column
    !is.na(Income),
    Income != "",
    Income > 0
    
  ) %>%
  arrange(Transaction_ID))

# View cleaned data
View(clean_data)

# Optional checks
cat("Duplicated Transaction_IDs: ", any(duplicated(clean_data$Transaction_ID)), "\n")
cat("NA in Phone_clean: ", sum(is.na(clean_data$Phone_clean)), "\n")
cat("Empty Phone_clean: ", sum(clean_data$Phone_clean == ""), "\n")
cat("NA in City: ", sum(is.na(clean_data$City)), "\n")
cat("Empty City: ", sum(clean_data$City == ""), "\n")
cat("NA in State: ", sum(is.na(clean_data$State)), "\n")
cat("Empty State: ", sum(clean_data$State == ""), "\n")
cat("NA in Zipcode: ", sum(is.na(clean_data$Zipcode_clean)), "\n")
cat("Empty Zipcode: ", sum(clean_data$Zipcode_clean == ""), "\n")
cat("NA values in Country: ", sum(is.na(clean_data$Country)), "\n")
cat("Empty strings in Country: ", sum(clean_data$Country == ""), "\n")
cat("Unwanted characters in Country: ", 
    any(grepl("[^A-Za-z\\s]", clean_data$Country)), "\n")
cat("Improper capitalization in Country: ", 
    any(clean_data$Country != str_to_title(clean_data$Country)), "\n")
cat("NA values in Age: ", sum(is.na(clean_data$Age)), "\n")
cat("Ages <= 0: ", sum(clean_data$Age <= 0), "\n")
cat("Ages > 120: ", sum(clean_data$Age > 120), "\n")
cat("The largest age is:", max(data$Age, na.rm = TRUE), "\n")
cat("The largest age is:", max(clean_data$Age, na.rm = TRUE), "\n")
cat("Number of empty strings in Gender:", sum(clean_data$Gender == ""), "\n")
cat("Number of NA values in Gender:", sum(is.na(clean_data$Gender)), "\n")
cat("Gender value counts:\n")
print(table(clean_data$Gender, useNA = "ifany"))
cat("Unique Gender values:\n")
print(unique(clean_data$Gender))
cat("Number of NA values in Income:", sum(is.na(data$Income)), "\n")
cat("Empty Income: ", sum(data$Income == ""), "\n")
cat("Number of NA values in Income:", sum(is.na(clean_data$Income)), "\n")
cat("Empty Income: ", sum(clean_data$Income == ""), "\n")