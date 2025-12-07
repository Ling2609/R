# Load required libraries
library(dplyr)
library(stringr)

# Load data
data <- read.csv("retail_data.csv")
View(data)
data[data==""]<-NA

# Clean Transaction_ID and Phone in one pipeline
data_clean <- data %>%
  select(-Name,-Email, -Phone, -Address, -Zipcode) %>% 
  
  mutate(
    # Clean Country
    Country = str_squish(Country),# Remove extra spaces
    Country = str_to_title(Country), # Capitalize properly
    
    # Convert Age to numeric (in case it's stored as character)
    Age = as.numeric(Age),
    
    # Clean and standardize format to dd/mm/yyyy (without validation)
    Date= Date %>%
      str_trim() %>%               # Remove whitespace
      str_replace_all("[-/]", "/") %>%  # Replace all separators with /
      str_replace_all("/+", "/") %>%    # Remove duplicate slashes
      str_replace("^(\\d{1})/(\\d{1})/(\\d{4})$", "0\\1/0\\2/\\3") %>%  # Add leading zeros
      str_replace("^(\\d{2})/(\\d{1})/(\\d{4})$", "\\1/0\\2/\\3") %>%   # for day/month
      str_replace("^(\\d{1})/(\\d{2})/(\\d{4})$", "0\\1/\\2/\\3") %>%
      str_replace("^(\\d)/(\\d)/(\\d{4})$", "0\\1/0\\2/\\3") %>%        # Catch single digits
      str_extract("^\\d{2}/\\d{2}/\\d{4}$"),# Keep only properly formatted
    
    Time= str_replace_all(Time, "[^0-9:]", "") %>%
      str_replace("^([0-9]{2})([0-9]{2})([0-9]{2})$", "\\1:\\2:\\3") %>%
      str_replace("^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})$", 
                  sprintf("%02d:%02d:%02d", 
                          as.numeric(str_extract(., "^([0-9]{1,2})")),
                          as.numeric(str_extract(., ":([0-9]{1,2}):") %>% str_remove_all(":")),
                          as.numeric(str_extract(., "([0-9]{1,2})$")))),
    Time= if_else(str_detect(Time, "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"), 
                  Time, 
                  NA_character_),
    
    across(c(Total_Purchases, Amount, Total_Amount), 
           ~ str_replace_all(., "[^0-9.]", "")),
    
    Total_Purchases = as.integer(Total_Purchases),
    Amount = as.numeric(Amount) %>% round(2),
    Total_Amount= as.numeric(Total_Amount) %>% round(2),
    
    # Validate ranges
    Total_Purchases = if_else(between(Total_Purchases, 1, 1000), 
                              Total_Purchases, NA_integer_),
    Amount = if_else(between(Amount, 1, 9999), Amount, NA_real_),
    
    # Ensure Total_Amount = Amount * Total_Purchases
    Total_Amount = if_else(
      near(Total_Amount, Amount* Total_Purchases, tol = 0.01),
      Total_Amount,
      Amount * Total_Purchases
    ),
    Total_Amount = if_else(between(Total_Amount, 1, 1000000), 
                           Total_Amount, NA_real_),
    
    #factorising here
    Gender= as.factor(Gender),
    Income= as.factor(Income),
    Customer_Segment=as.factor(Customer_Segment),
    Product_Category=as.factor(Product_Category),
    Product_Brand=as.factor(Product_Brand),
    Product_Type=as.factor(Product_Type),
    Feedback=as.factor(Feedback),
    Shipping_Method=as.factor(Shipping_Method),
    Payment_Method=as.factor(Payment_Method),
    Order_Status=as.factor(Order_Status),
    Ratings=as.factor(Ratings),
    City=as.factor(City),
    State=as.factor(State),
    Country=as.factor(Country),
    Month=as.factor(Month),
    
  ) %>%
  filter(
    !Transaction_ID %in% Transaction_ID[duplicated(Transaction_ID) | duplicated(Transaction_ID, fromLast = TRUE)],
    grepl("^[0-9]+$", Transaction_ID),
    
    # Age range validation
    between(Age, 1, 100),
    
    !(is.na(Date)),
    !(is.na(Time))
    
  ) %>%
  arrange(Transaction_ID)
View(data_clean)
# Check the structure and summary of your cleaned data
str(data_clean)

# Check for any constant columns (columns with no variation)
summary(data_clean)

# Check for the number of missing values in each column
colSums(is.na(data_clean))

#install.packages("mice")
library(mice)

# Specify columns to impute
columns_to_impute <- setdiff(names(data_clean), c("Transaction_ID", "Customer_ID"))

# Perform imputation on the selected columns
mice_data <- mice(data_clean[, columns_to_impute], m = 5, method = 'pmm', seed = 500)

# Check the imputation results
summary(mice_data)

# Complete the imputed dataset (using the first imputed dataset)
complete_data <- complete(mice_data, 1)

# View the completed data
View(complete_data)

saveRDS(complete_data, file="complete_data.rds")

str(complete_data)

# Check for any constant columns (columns with no variation)
summary(complete_data)

# Check for the number of missing values in each columnn
colSums(is.na(complete_data))