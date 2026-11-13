# SECTION 1: Loading prepared splits and defining candidate models -----

# Starting from the datasets exported by Modelling_Data_Preparation.R.
# This script fits the candidate regression models, checks assumptions,
# validates predictions on held-out 2025 records and produces forecasts.

# Checking that the CA3 project folder has been set up
project_dir <- "/Users/corey/Downloads/Data Science CA3/CA 3/CA3"

stopifnot(
  dir.exists(project_dir)
)

# Identifying the folders created during data preparation
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "output")
figures_dir <- file.path(project_dir, "figures")

# Creating the output and figures folders if they do not already exist
dir.create(output_dir, showWarnings = FALSE)
dir.create(figures_dir, showWarnings = FALSE)

# Checking that the prepared RDS files are available
stopifnot(
  file.exists(file.path(data_dir, "price_data.rds")),
  file.exists(file.path(data_dir, "train_dev_2019_2023.rds")),
  file.exists(file.path(data_dir, "val_2024.rds")),
  file.exists(file.path(data_dir, "train_2019_2024.rds")),
  file.exists(file.path(data_dir, "test_2025.rds")),
  file.exists(file.path(data_dir, "price_data_all_records.rds"))
)

# Loading the main modelling and split datasets
price_data <- readRDS(
  file.path(data_dir, "price_data.rds")
)

train_dev_2019_2023 <- readRDS(
  file.path(data_dir, "train_dev_2019_2023.rds")
)

val_2024 <- readRDS(
  file.path(data_dir, "val_2024.rds")
)

train_2019_2024 <- readRDS(
  file.path(data_dir, "train_2019_2024.rds")
)

test_2025 <- readRDS(
  file.path(data_dir, "test_2025.rds")
)

# Loading the dataset containing all formatted records
# This will be used later for the duplicate-policy sensitivity check
price_data_all_records <- readRDS(
  file.path(data_dir, "price_data_all_records.rds")
)

train_2019_2024_all_records <- subset(
  price_data_all_records,
  year >= 2019 & year <= 2024
)
test_2025_all_records <- subset(
  price_data_all_records,
  year == 2025
)
rownames(train_2019_2024_all_records) <- NULL
rownames(test_2025_all_records) <- NULL

# Checking the main modelling datasets before fitting
stopifnot(
  nrow(price_data) == 5070,
  nrow(train_dev_2019_2023) == 3748,
  nrow(val_2024) == 647,
  nrow(train_2019_2024) == 4395,
  nrow(test_2025) == 675,
  nrow(train_dev_2019_2023) + nrow(val_2024) == nrow(train_2019_2024),
  nrow(train_2019_2024) + nrow(test_2025) == nrow(price_data),
  inherits(price_data$sale_date, "Date"),
  is.numeric(price_data$price_eur),
  is.numeric(price_data$log_price_eur),
  is.numeric(price_data$time_years),
  is.factor(price_data$quarter),
  !is.ordered(price_data$quarter),
  !is.ordered(train_2019_2024$quarter),
  !is.ordered(test_2025$quarter),
  identical(levels(price_data$quarter), c("Q1", "Q2", "Q3", "Q4")),
  max(train_dev_2019_2023$sale_date) < min(val_2024$sale_date),
  max(train_2019_2024$sale_date) < min(test_2025$sale_date)
)

# Checking the sensitivity-check datasets before fitting
stopifnot(
  nrow(price_data_all_records) == 5074,
  nrow(train_2019_2024_all_records) == 4398,
  nrow(test_2025_all_records) == 676
)

# Checking that all four quarters contain records in each split
stopifnot(
  all(table(train_dev_2019_2023$quarter) > 0),
  all(table(val_2024$quarter) > 0),
  all(table(train_2019_2024$quarter) > 0),
  all(table(test_2025$quarter) > 0)
)

# Setting the significance level, date origin and chart colours
alpha <- 0.05
origin_date <- as.Date("2019-01-01")
days_per_year <- 365.25
chart_colour <- "#FF7F50"
secondary_colour <- "#5B7083"

# Defining the candidate regression formulas
model_a_formula <- price_eur ~ time_years + quarter
model_b_formula <- log_price_eur ~ time_years + quarter

# # CA3 research question and candidate models
#
# ## Research question
# Dataset: price_data (train_2019_2024 and test_2025)
# Variables: price_eur, log_price_eur, time_years and quarter
# Question: To what extent can sale timing and calendar quarter predict
# recorded full-market, second-hand residential prices in County Sligo,
# and how accurately does a model trained on 2019-2024 records predict
# held-out 2025 prices?
#
# ## Candidate models
# Baseline (Mean): Training-set mean recorded price.
# Baseline (Median): Training-set median recorded price.
# Model A: price_eur ~ time_years + quarter
# Model B: log_price_eur ~ time_years + quarter
#
# Model B predictions are converted back to euros using both exp(y_hat)
# and Duan's smearing adjustment, exp(y_hat) * mean(exp(e_train)).
#
# ## Validation and forecasting splits
# Development check: Fit on 2019-2023 (n = 3,748), check on 2024 (n = 647).
# Main evaluation: Fit on 2019-2024 (n = 4,395), test on 2025 (n = 675).
# Forecasting: Held-out 2025 test cases and 2026 quarterly scenarios.
#
# The models use calendar variables derived from sale_date and do not
# include property size, condition or location.

# Displaying the datasets loaded for modelling
cat("Prepared datasets loaded successfully.\n")
cat("Eligible price records (2019-2025):", nrow(price_data), "\n")
cat("Development training records (2019-2023):", nrow(train_dev_2019_2023), "\n")
cat("Development validation records (2024):", nrow(val_2024), "\n")
cat("Main training records (2019-2024):", nrow(train_2019_2024), "\n")
cat("Held-out 2025 test records:", nrow(test_2025), "\n")
cat("Sensitivity-check records:", nrow(price_data_all_records), "\n")
cat("Model A formula:", deparse(model_a_formula), "\n")
cat("Model B formula:", deparse(model_b_formula), "\n")
cat("Significance level:", alpha, "\n")
