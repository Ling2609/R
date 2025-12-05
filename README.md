Retail Customer Satisfaction: Demographic & Geographic Analysis

📌 Project Context

This repository contains the R code and logic for the individual component of a retail data science project developed by Low Wei Ling (TP080089). Due to file size constraints, the dataset files are not hosted here.

This work serves as the Data Engineering foundation (Strict Pipeline) and the Lead Analysis for Objective 2, focusing on two core areas:

Robust Data Engineering: Developing a strict, integrity-focused data cleaning pipeline.

Demographic & Geographic Analytics: Investigating how customer age, income, gender, and location influence satisfaction ratings.

📂 Repository Contents

File

Description

cleaning.R

The Strict Cleaning Pipeline. A robust R script that validates data boundaries, enforces logic, and removes incomplete records to ensure 100% data integrity without imputation.

TP080089 LOW WEI LING.R

Individual Analysis Script. The specific analysis code focusing on Objective 2 (Demographics & Geographics).

⚠️ Note on Data Files:
The raw data (retail_data.csv) and processed data (complete_data.csv and complete_data_recreated.csv) are excluded from this repository due to large file sizes. To run the code, you must have the raw data file saved locally.

🛠️ Part 1: The Data Cleaning Pipeline (cleaning.R)

This project implements a "Strict Validation" approach. Unlike methods that guess missing values (Imputation), this pipeline prioritizes data authenticity by removing any rows with missing or inconsistent data.

Key Validation Steps:

Diagnostics: Pre-calculation of min/max/counts to scientifically justify filtering boundaries (e.g., proving why Zipcodes < 4 digits are errors).

Logic Enforcement:

Financials: Verifies Total_Amount matches Price * Quantity.

Demographics: Filters for realistic ranges (e.g., Age 18-100).

Formats: Standardizes text to Title Case (e.g., "new york" → "New York").

Zero-Tolerance Policy: The pipeline ends with a final audit to guarantee the output file has 0 NAs and 0 empty strings.

📊 Part 2: Individual Analysis (TP080089 LOW WEI LING.R)

This analysis investigates the question: "Which demographic and geographic factors actually drive customer ratings?"

🔍 Key Findings

Age is the Dominant Factor:

Statistical testing (Chi-Square & Cramér’s V) identified Age as the strongest predictor of customer satisfaction.

Insight: Older customers (46+) consistently provide higher ratings compared to younger demographics (18-25).

Income Paradox:

Contrary to expectations, higher income levels showed a weak negative correlation with ratings, suggesting affluent customers may have higher service expectations.

Geography's Limited Impact:

While variation exists between countries (e.g., Canada vs. USA), spatial analysis confirms that location is a weaker driver of satisfaction compared to demographic factors.

🚀 How to Run the Code

1. Prerequisites

Ensure the following R libraries are installed:

install.packages(c("dplyr", "stringr", "ggplot2", "vcd", "caret", "randomForest", "xgboost", "leaflet", "sf", "rnaturalearth", "rnaturalearthdata"))


2. Setup Data

Since the data is not in the repo, place the raw retail_data.csv file into the root folder of this project on your local machine.

3. Execution Order

Step A: Generate the Clean Data
Run the cleaning pipeline first. This will process the raw file and create the clean dataset locally.

source("cleaning.R")


Output: A new file named complete_data_recreated.csv will be generated in your folder.

Step B: Run the Analysis
Once the clean file exists, run the analysis script.

source("TP080089 LOW WEI LING.R")


Output: This will generate the summary tables, statistical test results, and visualizations used in the report.
