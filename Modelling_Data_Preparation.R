# SECTION 1: Importing and inspecting the master dataset --------------
# Continuing from CA1 and CA2 using the compiled Sligo master CSV.
# Keeping the raw master file unchanged and preparing the modelling subsets.

# Setting the project folder and master file if not already defined
if (!exists("project_dir")) {
  project_dir <- getwd()
}

if (!exists("input_file")) {
  input_file <- file.path(project_dir, "Sligo_Master_2019_2025.csv")
}

# Checking that the project folder and master CSV exist
stopifnot(
  exists("project_dir"),
  dir.exists(project_dir),
  exists("input_file"),
  file.exists(input_file)
)

# Creating the folder for preparation outputs
output_dir <- file.path(project_dir, "output")
dir.create(output_dir, showWarnings = FALSE)

# Reading the master CSV as character data before type conversion
raw <- read.csv(
  input_file,
  check.names = FALSE,
  colClasses = "character",
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

# Checking that the nine PSRA column names match the master schema
expected_columns <- c(
  "Date of Sale (dd/mm/yyyy)",
  "Address",
  "County",
  "Eircode",
  "Price (\u20ac)",
  "Not Full Market Price",
  "VAT Exclusive",
  "Description of Property",
  "Property Size Description"
)

stopifnot(
  identical(names(raw), expected_columns),
  nrow(raw) == 5964,
  ncol(raw) == 9
)

# Displaying the imported record and variable counts
cat("Records imported:", nrow(raw), "\n")
cat("Variables imported:", ncol(raw), "\n")

names(raw)
str(raw)

# Auditing missing values and exact duplicate rows in the master import
duplicate_count <- sum(duplicated(raw))
cat("Additional exact copies in master:", duplicate_count, "\n")

master_import_audit <- data.frame(
  variable = names(raw),
  total_records = nrow(raw),
  missing_records = colSums(is.na(raw)),
  missing_percent = round(100 * colMeans(is.na(raw)), 2),
  unique_non_missing = vapply(
    raw,
    function(x) length(unique(x[!is.na(x)])),
    integer(1)
  ),
  row.names = NULL
)

print(master_import_audit, row.names = FALSE)

# Checking the categorical values in the raw master
table(raw[["County"]], useNA = "ifany")
table(raw[["Not Full Market Price"]], useNA = "ifany")
table(raw[["VAT Exclusive"]], useNA = "ifany")
table(raw[["Description of Property"]], useNA = "ifany")

# Saving the master import audit for the CA3 report
write.csv(
  master_import_audit,
  file.path(output_dir, "01_master_import_audit.csv"),
  row.names = FALSE
)

stopifnot(
  file.exists(file.path(output_dir, "01_master_import_audit.csv")),
  file.info(file.path(output_dir, "01_master_import_audit.csv"))$size > 0
)

cat(
  "\nSection 1 completed. Master records verified:",
  nrow(raw),
  "\n"
)


# SECTION 2: Formatting variables and applying eligibility rules ------

# Checking that the imported master data and output folder exist
stopifnot(
  exists("raw"),
  exists("output_dir"),
  nrow(raw) == 5964
)

# Creating a working copy and keeping raw unchanged
sligo <- raw

# Renaming columns to consistent R variable names
names(sligo) <- c(
  "sale_date",
  "address",
  "county",
  "eircode",
  "price_eur",
  "not_full_market_price",
  "vat_exclusive",
  "property_status",
  "property_size_description"
)

# Stripping surrounding spaces and converting empty strings to NA
sligo[] <- lapply(sligo, function(x) {
  x <- trimws(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x
})

# Preserving the original date text to check conversion accuracy
original_dates <- sligo$sale_date

# Parsing sale dates in day/month/year format
sligo$sale_date <- as.Date(
  original_dates,
  format = "%d/%m/%Y"
)

# Stripping euro symbols and commas before numeric price conversion
sligo$price_eur <- as.numeric(
  gsub("[\u20ac,]", "", sligo$price_eur)
)

# Verifying clean date and price conversions
stopifnot(
  !anyNA(sligo$sale_date),
  !anyNA(sligo$price_eur),
  all(is.finite(sligo$price_eur)),
  all(sligo$price_eur > 0),
  all(format(sligo$sale_date, "%d/%m/%Y") == original_dates)
)

# Checking binary indicator values before factor conversion
stopifnot(
  !anyNA(sligo$not_full_market_price),
  !anyNA(sligo$vat_exclusive),
  all(sligo$not_full_market_price %in% c("No", "Yes")),
  all(sligo$vat_exclusive %in% c("No", "Yes"))
)

sligo$not_full_market_price <- factor(
  sligo$not_full_market_price,
  levels = c("No", "Yes")
)

sligo$vat_exclusive <- factor(
  sligo$vat_exclusive,
  levels = c("No", "Yes")
)

# Checking and converting property status descriptions
property_labels <- c(
  "New Dwelling house /Apartment",
  "Second-Hand Dwelling house /Apartment"
)

stopifnot(
  !anyNA(sligo$property_status),
  all(sligo$property_status %in% property_labels)
)

sligo$property_status <- factor(
  sligo$property_status,
  levels = property_labels,
  labels = c("New", "Second-hand")
)

# Confirming that every record is in County Sligo
stopifnot(
  !anyNA(sligo$county),
  all(sligo$county == "Sligo")
)

sligo$county <- factor(sligo$county)

# Identifying exact duplicate rows across all nine original columns
duplicate_rows <- duplicated(raw)

# Retaining all formatted records for sensitivity checking
sligo_all_records <- sligo

# Retaining the first occurrence of each exact match for the main analysis
sligo_clean <- sligo_all_records[!duplicate_rows, ]
rownames(sligo_clean) <- NULL

stopifnot(
  nrow(sligo_all_records) == 5964,
  nrow(sligo_clean) == 5957,
  !anyDuplicated(raw[!duplicate_rows, ])
)

# Applying the CA2 principal price eligibility criteria:
# Second-hand status, full market price (No) and VAT Exclusive (No)
price_data <- subset(
  sligo_clean,
  not_full_market_price == "No" &
    property_status == "Second-hand" &
    vat_exclusive == "No"
)
price_data <- droplevels(price_data)
rownames(price_data) <- NULL

price_data_all_records <- subset(
  sligo_all_records,
  not_full_market_price == "No" &
    property_status == "Second-hand" &
    vat_exclusive == "No"
)
price_data_all_records <- droplevels(price_data_all_records)
rownames(price_data_all_records) <- NULL

stopifnot(
  nrow(price_data) == 5070,
  nrow(price_data_all_records) == 5074,
  all(price_data$not_full_market_price == "No"),
  all(price_data$property_status == "Second-hand"),
  all(price_data$vat_exclusive == "No")
)

# Auditing variable suitability within the 5,070 eligible price records
suitability_audit <- data.frame(
  variable = names(price_data),
  r_class = vapply(
    price_data,
    function(x) paste(class(x), collapse = ", "),
    character(1)
  ),
  eligible_records = nrow(price_data),
  missing_in_eligible = colSums(is.na(price_data)),
  missing_percent_eligible = round(
    100 * colMeans(is.na(price_data)),
    2
  ),
  distinct_non_missing_eligible = vapply(
    price_data,
    function(x) length(unique(x[!is.na(x)])),
    integer(1)
  ),
  modelling_role = c(
    "Source date used to derive elapsed sale time (time_years) and quarter",
    "Excluded from modelling and public exports (unstructured street address)",
    "Constant in eligible subset (Sligo only); zero predictor variation",
    "Excluded from modelling (41.97% missing in eligible subset)",
    "Continuous response variable (recorded price in EUR; also log-transformed)",
    "Constant in eligible subset (all No); defines full-market scope",
    "Constant in eligible subset (all No); defines VAT-inclusive scope",
    "Constant in eligible subset (all Second-hand); defines dwelling scope",
    "Excluded from modelling (100% missing in all records)"
  ),
  row.names = NULL
)

print(suitability_audit, row.names = FALSE)

# Recording the scope and eligibility notes
modelling_scope_notes <- c(
  "CA3 continues with the CA2 eligible price subset (n = 5,070; sensitivity n = 5,074).",
  "Eligibility requires second-hand status, Not Full Market Price = No and VAT Exclusive = No.",
  "Within the eligible subset, county, property_status, not_full_market_price and vat_exclusive are constant.",
  "Property size description is 100% missing and Eircode is missing in 2,128 of 5,070 eligible records (41.97%).",
  "Sale date and price are complete in all 5,070 eligible records.",
  "Predictive modelling therefore uses derived calendar predictors (elapsed time and quarter) to predict price."
)

# Saving the variable suitability table and scope notes
write.csv(
  suitability_audit,
  file.path(output_dir, "02_eligibility_and_variable_suitability.csv"),
  row.names = FALSE
)

writeLines(
  modelling_scope_notes,
  file.path(output_dir, "02_modelling_scope_notes.txt")
)

stopifnot(
  file.exists(file.path(output_dir, "02_eligibility_and_variable_suitability.csv")),
  file.exists(file.path(output_dir, "02_modelling_scope_notes.txt"))
)

cat(
  "\nSection 2 completed.",
  "\nMain records after duplicate policy:", nrow(sligo_clean),
  "\nEligible price-modelling records:", nrow(price_data),
  "\nSensitivity eligible records:", nrow(price_data_all_records),
  "\n"
)


# SECTION 3: Deriving the predictive modelling features ----------------

# Checking that the eligible price datasets and output folder exist
stopifnot(
  exists("price_data"),
  exists("price_data_all_records"),
  exists("output_dir"),
  nrow(price_data) == 5070,
  nrow(price_data_all_records) == 5074
)

# Defining the fixed date origin for the continuous sale-time predictor
# 1 January 2019 is the start of the first calendar year in the dataset
origin_date <- as.Date("2019-01-01")
days_per_year <- 365.25

# Defining a single function to derive calendar and response features
add_modelling_features <- function(data, origin, year_length) {

  stopifnot(
    inherits(data$sale_date, "Date"),
    is.numeric(data$price_eur),
    !anyNA(data$sale_date),
    !anyNA(data$price_eur),
    all(is.finite(data$price_eur)),
    all(data$price_eur > 0)
  )

  # Extracting calendar year and month as integers
  data$year <- as.integer(format(data$sale_date, "%Y"))
  data$month <- as.integer(format(data$sale_date, "%m"))

  stopifnot(
    all(data$year %in% 2019:2025),
    all(data$month %in% 1:12)
  )

  # Creating quarter as an UNORDERED factor with Q1 as reference level
  # Using treatment contrasts rather than polynomial contrasts in lm()
  data$quarter <- factor(
    (data$month - 1L) %/% 3L + 1L,
    levels = 1:4,
    labels = c("Q1", "Q2", "Q3", "Q4"),
    ordered = FALSE
  )

  # Retaining the CA1/CA2 analytical period for continuity checks
  data$period <- factor(
    ifelse(
      data$year == 2019,
      "2019",
      ifelse(data$year <= 2021, "2020-2021", "2022-2025")
    ),
    levels = c("2019", "2020-2021", "2022-2025"),
    ordered = FALSE
  )

  # Calculating elapsed days and elapsed years since 1 January 2019
  data$time_days <- as.numeric(data$sale_date - origin)
  data$time_years <- data$time_days / year_length

  # Calculating natural log-transformed price for Model B
  data$log_price_eur <- log(data$price_eur)

  # Checking the derived variables
  stopifnot(
    !anyNA(data$quarter),
    !is.ordered(data$quarter),
    identical(levels(data$quarter), c("Q1", "Q2", "Q3", "Q4")),
    !anyNA(data$time_days),
    !anyNA(data$time_years),
    all(is.finite(data$time_years)),
    all(data$time_years >= 0),
    !anyNA(data$log_price_eur),
    all(is.finite(data$log_price_eur))
  )

  return(data)
}

# Applying the feature derivation to both eligible datasets
price_data <- add_modelling_features(
  price_data,
  origin = origin_date,
  year_length = days_per_year
)

price_data_all_records <- add_modelling_features(
  price_data_all_records,
  origin = origin_date,
  year_length = days_per_year
)

stopifnot(
  nrow(price_data) == 5070,
  nrow(price_data_all_records) == 5074
)

# Summarising the derived continuous and factor variables
derived_feature_summary <- data.frame(
  variable = c(
    "price_eur",
    "log_price_eur",
    "time_days",
    "time_years"
  ),
  description = c(
    "Recorded full-market second-hand price (EUR)",
    "Natural logarithm of recorded price, log(price_eur)",
    "Elapsed days since 2019-01-01",
    "Elapsed years since 2019-01-01 (time_days / 365.25)"
  ),
  records = nrow(price_data),
  minimum = c(
    min(price_data$price_eur),
    min(price_data$log_price_eur),
    min(price_data$time_days),
    min(price_data$time_years)
  ),
  q1 = c(
    unname(quantile(price_data$price_eur, 0.25)),
    unname(quantile(price_data$log_price_eur, 0.25)),
    unname(quantile(price_data$time_days, 0.25)),
    unname(quantile(price_data$time_years, 0.25))
  ),
  median = c(
    median(price_data$price_eur),
    median(price_data$log_price_eur),
    median(price_data$time_days),
    median(price_data$time_years)
  ),
  mean = c(
    mean(price_data$price_eur),
    mean(price_data$log_price_eur),
    mean(price_data$time_days),
    mean(price_data$time_years)
  ),
  q3 = c(
    unname(quantile(price_data$price_eur, 0.75)),
    unname(quantile(price_data$log_price_eur, 0.75)),
    unname(quantile(price_data$time_days, 0.75)),
    unname(quantile(price_data$time_years, 0.75))
  ),
  maximum = c(
    max(price_data$price_eur),
    max(price_data$log_price_eur),
    max(price_data$time_days),
    max(price_data$time_years)
  ),
  sd = c(
    sd(price_data$price_eur),
    sd(price_data$log_price_eur),
    sd(price_data$time_days),
    sd(price_data$time_years)
  ),
  row.names = NULL
)

print(derived_feature_summary, row.names = FALSE, digits = 6)

# Cross-tabulating eligible records by sale year and quarter
year_quarter_matrix <- with(
  price_data,
  table(
    year = factor(year, levels = 2019:2025),
    quarter = quarter
  )
)

print(addmargins(year_quarter_matrix))

quarterly_counts_by_year <- data.frame(
  year = as.integer(rownames(year_quarter_matrix)),
  Q1 = as.integer(year_quarter_matrix[, "Q1"]),
  Q2 = as.integer(year_quarter_matrix[, "Q2"]),
  Q3 = as.integer(year_quarter_matrix[, "Q3"]),
  Q4 = as.integer(year_quarter_matrix[, "Q4"]),
  total_records = as.integer(rowSums(year_quarter_matrix)),
  row.names = NULL
)

stopifnot(
  sum(quarterly_counts_by_year$total_records) == nrow(price_data)
)

# Saving the derived feature summaries
write.csv(
  derived_feature_summary,
  file.path(output_dir, "03_derived_feature_summary.csv"),
  row.names = FALSE
)

write.csv(
  quarterly_counts_by_year,
  file.path(output_dir, "03_quarterly_counts_by_year.csv"),
  row.names = FALSE
)

stopifnot(
  file.exists(file.path(output_dir, "03_derived_feature_summary.csv")),
  file.exists(file.path(output_dir, "03_quarterly_counts_by_year.csv"))
)

cat(
  "\nSection 3 completed.",
  "\nOrigin date:", format(origin_date, "%Y-%m-%d"),
  "\nQuarter ordered factor?", is.ordered(price_data$quarter),
  "\nQuarter reference level:", levels(price_data$quarter)[1],
  "\n"
)


# SECTION 4: Constructing chronological train, validation and test splits

# Checking that the feature-engineered datasets exist
stopifnot(
  exists("price_data"),
  exists("price_data_all_records"),
  exists("output_dir"),
  nrow(price_data) == 5070,
  nrow(price_data_all_records) == 5074,
  all(c("time_years", "quarter", "log_price_eur") %in% names(price_data))
)

# Splitting the dataset chronologically before making modelling decisions:
# 1. Internal development split: 2019-2023 training vs 2024 validation
# 2. Primary modelling split: 2019-2024 training vs 2025 held-out test
train_dev_2019_2023 <- subset(price_data, year >= 2019 & year <= 2023)
val_2024 <- subset(price_data, year == 2024)

train_2019_2024 <- subset(price_data, year >= 2019 & year <= 2024)
test_2025 <- subset(price_data, year == 2025)

# Resetting row names on each split
rownames(train_dev_2019_2023) <- NULL
rownames(val_2024) <- NULL
rownames(train_2019_2024) <- NULL
rownames(test_2025) <- NULL

# Creating the corresponding sensitivity splits with exact copies retained
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

# Verifying split sizes, non-overlap and chronological ordering
stopifnot(
  nrow(train_dev_2019_2023) == 3748,
  nrow(val_2024) == 647,
  nrow(train_2019_2024) == 4395,
  nrow(test_2025) == 675,
  nrow(train_dev_2019_2023) + nrow(val_2024) == nrow(train_2019_2024),
  nrow(train_2019_2024) + nrow(test_2025) == nrow(price_data),
  nrow(train_2019_2024_all_records) == 4398,
  nrow(test_2025_all_records) == 676,
  nrow(train_2019_2024_all_records) + nrow(test_2025_all_records) ==
    nrow(price_data_all_records),
  max(train_dev_2019_2023$sale_date) < min(val_2024$sale_date),
  max(train_2019_2024$sale_date) < min(test_2025$sale_date),
  all(table(train_dev_2019_2023$quarter) > 0),
  all(table(val_2024$quarter) > 0),
  all(table(train_2019_2024$quarter) > 0),
  all(table(test_2025$quarter) > 0)
)

# Defining a helper to summarise each chronological split
summarise_split <- function(data, split_name, role_description) {
  data.frame(
    split = split_name,
    role = role_description,
    records = nrow(data),
    share_of_eligible_percent = round(
      100 * nrow(data) / nrow(price_data),
      2
    ),
    start_date = format(min(data$sale_date), "%Y-%m-%d"),
    end_date = format(max(data$sale_date), "%Y-%m-%d"),
    min_time_years = min(data$time_years),
    max_time_years = max(data$time_years),
    median_price_eur = median(data$price_eur),
    mean_price_eur = mean(data$price_eur),
    sd_price_eur = sd(data$price_eur),
    iqr_price_eur = IQR(data$price_eur),
    median_log_price = median(data$log_price_eur),
    mean_log_price = mean(data$log_price_eur),
    sd_log_price = sd(data$log_price_eur),
    Q1_records = sum(data$quarter == "Q1"),
    Q2_records = sum(data$quarter == "Q2"),
    Q3_records = sum(data$quarter == "Q3"),
    Q4_records = sum(data$quarter == "Q4"),
    row.names = NULL
  )
}

chronological_split_summary <- rbind(
  summarise_split(
    train_dev_2019_2023,
    "train_dev_2019_2023",
    "Internal development training set (2019-2023)"
  ),
  summarise_split(
    val_2024,
    "val_2024",
    "Internal development validation set (2024)"
  ),
  summarise_split(
    train_2019_2024,
    "train_2019_2024",
    "Primary training set (2019-2024)"
  ),
  summarise_split(
    test_2025,
    "test_2025",
    "Held-out final test set (2025)"
  ),
  summarise_split(
    price_data,
    "price_data_2019_2025",
    "Complete eligible dataset (2019-2025; final refit for 2026 forecasts)"
  )
)

print(chronological_split_summary, row.names = FALSE, digits = 6)

# Summarising annual price and log-price statistics across 2019-2025
annual_splits <- split(price_data, price_data$year)

annual_modelling_summary <- do.call(
  rbind,
  lapply(names(annual_splits), function(yr) {
    df_yr <- annual_splits[[yr]]
    data.frame(
      year = as.integer(yr),
      split_role = ifelse(as.integer(yr) <= 2024, "Training (2019-2024)", "Held-out test (2025)"),
      records = nrow(df_yr),
      median_price_eur = median(df_yr$price_eur),
      mean_price_eur = mean(df_yr$price_eur),
      sd_price_eur = sd(df_yr$price_eur),
      q1_price_eur = unname(quantile(df_yr$price_eur, 0.25)),
      q3_price_eur = unname(quantile(df_yr$price_eur, 0.75)),
      iqr_price_eur = IQR(df_yr$price_eur),
      median_log_price = median(df_yr$log_price_eur),
      mean_log_price = mean(df_yr$log_price_eur),
      sd_log_price = sd(df_yr$log_price_eur),
      row.names = NULL
    )
  })
)

print(annual_modelling_summary, row.names = FALSE, digits = 6)

# Saving the chronological split and annual summaries
write.csv(
  chronological_split_summary,
  file.path(output_dir, "04_chronological_split_summary.csv"),
  row.names = FALSE
)

write.csv(
  annual_modelling_summary,
  file.path(output_dir, "04_annual_modelling_summary.csv"),
  row.names = FALSE
)

stopifnot(
  file.exists(file.path(output_dir, "04_chronological_split_summary.csv")),
  file.exists(file.path(output_dir, "04_annual_modelling_summary.csv"))
)

cat(
  "\nSection 4 completed.",
  "\nDevelopment split: 2019-2023 (n =", nrow(train_dev_2019_2023),
  ") vs 2024 (n =", nrow(val_2024), ")",
  "\nPrimary split: 2019-2024 (n =", nrow(train_2019_2024),
  ") vs 2025 (n =", nrow(test_2025), ")\n"
)


# SECTION 5: Assessing training distributions and relationships --------

# Checking that the primary training dataset and output folder exist
stopifnot(
  exists("train_2019_2024"),
  exists("project_dir"),
  exists("output_dir"),
  nrow(train_2019_2024) == 4395
)

# Creating the folder for preparation figures
figures_dir <- file.path(project_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE)

# Setting the coral orange and slate grey palette used in CA2
chart_colour <- "#FF7F50"
secondary_colour <- "#5B7083"

# Defining a helper for sample skewness
calculate_skewness <- function(x) {
  x_centred <- x - mean(x)
  mean(x_centred^3) / (sd(x)^3)
}

# Running Shapiro-Wilk and distributional checks on the 2019-2024 training set
# (Shapiro-Wilk in R requires 3 <= n <= 5000; n = 4,395 satisfies this)
shapiro_raw_train <- shapiro.test(train_2019_2024$price_eur)
shapiro_log_train <- shapiro.test(train_2019_2024$log_price_eur)

training_distribution_diagnostics <- data.frame(
  response_scale = c(
    "Raw recorded price (price_eur)",
    "Log-transformed price (log_price_eur)"
  ),
  dataset = "train_2019_2024",
  records = nrow(train_2019_2024),
  mean = c(
    mean(train_2019_2024$price_eur),
    mean(train_2019_2024$log_price_eur)
  ),
  median = c(
    median(train_2019_2024$price_eur),
    median(train_2019_2024$log_price_eur)
  ),
  sd = c(
    sd(train_2019_2024$price_eur),
    sd(train_2019_2024$log_price_eur)
  ),
  iqr = c(
    IQR(train_2019_2024$price_eur),
    IQR(train_2019_2024$log_price_eur)
  ),
  skewness = c(
    calculate_skewness(train_2019_2024$price_eur),
    calculate_skewness(train_2019_2024$log_price_eur)
  ),
  shapiro_W = c(
    unname(shapiro_raw_train$statistic),
    unname(shapiro_log_train$statistic)
  ),
  shapiro_p_value = c(
    shapiro_raw_train$p.value,
    shapiro_log_train$p.value
  ),
  row.names = NULL
)

print(training_distribution_diagnostics, row.names = FALSE, digits = 6)

# Summarising 2019-2024 training prices and log-prices across quarters
train_by_quarter <- split(train_2019_2024, train_2019_2024$quarter)

training_quarter_summary <- do.call(
  rbind,
  lapply(names(train_by_quarter), function(q_name) {
    df_q <- train_by_quarter[[q_name]]
    data.frame(
      quarter = q_name,
      records = nrow(df_q),
      median_price_eur = median(df_q$price_eur),
      mean_price_eur = mean(df_q$price_eur),
      sd_price_eur = sd(df_q$price_eur),
      q1_price_eur = unname(quantile(df_q$price_eur, 0.25)),
      q3_price_eur = unname(quantile(df_q$price_eur, 0.75)),
      iqr_price_eur = IQR(df_q$price_eur),
      median_log_price = median(df_q$log_price_eur),
      mean_log_price = mean(df_q$log_price_eur),
      sd_log_price = sd(df_q$log_price_eur),
      row.names = NULL
    )
  })
)

print(training_quarter_summary, row.names = FALSE, digits = 6)

write.csv(
  training_distribution_diagnostics,
  file.path(output_dir, "05_training_distribution_diagnostics.csv"),
  row.names = FALSE
)

write.csv(
  training_quarter_summary,
  file.path(output_dir, "05_training_quarter_summary.csv"),
  row.names = FALSE
)


# Chart 1: Raw vs log-transformed price distributions on training data

fig1_path <- file.path(
  figures_dir,
  "01_training_price_vs_log_price_distributions.png"
)

png(
  fig1_path,
  width = 2200,
  height = 1600,
  res = 180
)

par(
  mfrow = c(2, 2),
  mar = c(4.5, 4.5, 3.5, 1.5),
  las = 1
)

train_price_k <- train_2019_2024$price_eur / 1000

hist(
  train_price_k,
  breaks = 40,
  col = chart_colour,
  border = "white",
  main = paste0(
    "Training prices (2019-2024); n = ",
    nrow(train_2019_2024)
  ),
  xlab = "Recorded price (EUR thousands)",
  ylab = "Records"
)

qqnorm(
  train_price_k,
  main = "Training price normal Q-Q plot (EUR thousands)",
  xlab = "Normal quantile",
  ylab = "Recorded price (EUR thousands)",
  pch = 16,
  cex = 0.5,
  col = adjustcolor(chart_colour, alpha.f = 0.55)
)
qqline(train_price_k, col = "black", lwd = 2)

hist(
  train_2019_2024$log_price_eur,
  breaks = 40,
  col = secondary_colour,
  border = "white",
  main = paste0(
    "Training log-prices (2019-2024); n = ",
    nrow(train_2019_2024)
  ),
  xlab = "Log recorded price, log(EUR)",
  ylab = "Records"
)

qqnorm(
  train_2019_2024$log_price_eur,
  main = "Training log-price normal Q-Q plot",
  xlab = "Normal quantile",
  ylab = "Log recorded price, log(EUR)",
  pch = 16,
  cex = 0.5,
  col = adjustcolor(secondary_colour, alpha.f = 0.55)
)
qqline(train_2019_2024$log_price_eur, col = "black", lwd = 2)

dev.off()


# Chart 2: Price and log-price against elapsed time on training data

fig2_path <- file.path(
  figures_dir,
  "02_training_price_and_log_price_over_time.png"
)

train_zoom_limit <- 600
train_above_zoom <- sum(train_2019_2024$price_eur > train_zoom_limit * 1000)

png(
  fig2_path,
  width = 2400,
  height = 2000,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(5, 6, 4, 2),
  las = 1
)

plot(
  x = train_2019_2024$time_years,
  y = train_2019_2024$price_eur / 1000,
  ylim = c(0, train_zoom_limit),
  pch = 16,
  cex = 0.45,
  col = adjustcolor(chart_colour, alpha.f = 0.35),
  main = "Training recorded prices over elapsed time (2019-2024: zoomed view)",
  xlab = "Elapsed time since 1 January 2019 (years)",
  ylab = "Recorded price (EUR thousands)"
)

abline(
  lm(I(price_eur / 1000) ~ time_years, data = train_2019_2024),
  col = "black",
  lwd = 2.2
)

mtext(
  paste0(
    "Zoomed to EUR 600,000; ",
    train_above_zoom,
    " training records above this range remain in the fitted trend line (n = 4,395)."
  ),
  side = 1,
  line = 3.8,
  cex = 0.72
)

plot(
  x = train_2019_2024$time_years,
  y = train_2019_2024$log_price_eur,
  pch = 16,
  cex = 0.45,
  col = adjustcolor(secondary_colour, alpha.f = 0.35),
  main = "Training log-transformed prices over elapsed time (2019-2024: full range)",
  xlab = "Elapsed time since 1 January 2019 (years)",
  ylab = "Log recorded price, log(EUR)"
)

abline(
  lm(log_price_eur ~ time_years, data = train_2019_2024),
  col = "black",
  lwd = 2.2
)

dev.off()


# Chart 3: Training price and log-price distributions by quarter

fig3_path <- file.path(
  figures_dir,
  "03_training_price_distributions_by_quarter.png"
)

png(
  fig3_path,
  width = 2400,
  height = 2000,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(5, 6, 4, 2),
  las = 1
)

set.seed(123)

boxplot(
  price_eur / 1000 ~ quarter,
  data = train_2019_2024,
  horizontal = TRUE,
  col = chart_colour,
  border = "black",
  outline = FALSE,
  ylim = c(0, train_zoom_limit),
  main = "Training recorded prices by calendar quarter (2019-2024: zoomed view)",
  xlab = "Recorded price (EUR thousands)",
  ylab = ""
)

stripchart(
  price_eur / 1000 ~ quarter,
  data = train_2019_2024,
  method = "jitter",
  jitter = 0.12,
  vertical = FALSE,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("black", alpha.f = 0.15),
  add = TRUE
)

mtext(
  paste0(
    "Zoomed to EUR 600,000; ",
    train_above_zoom,
    " training records above this range remain in the boxplot summaries (n = 4,395)."
  ),
  side = 1,
  line = 3.8,
  cex = 0.72
)

boxplot(
  log_price_eur ~ quarter,
  data = train_2019_2024,
  horizontal = TRUE,
  col = secondary_colour,
  border = "black",
  outline = FALSE,
  main = "Training log-transformed prices by calendar quarter (2019-2024: full range)",
  xlab = "Log recorded price, log(EUR)",
  ylab = ""
)

stripchart(
  log_price_eur ~ quarter,
  data = train_2019_2024,
  method = "jitter",
  jitter = 0.12,
  vertical = FALSE,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("black", alpha.f = 0.15),
  add = TRUE
)

dev.off()

# Checking that all Section 5 outputs and figures exist
section5_files <- c(
  file.path(output_dir, "05_training_distribution_diagnostics.csv"),
  file.path(output_dir, "05_training_quarter_summary.csv"),
  fig1_path,
  fig2_path,
  fig3_path
)

stopifnot(
  all(file.exists(section5_files)),
  all(file.info(section5_files)$size > 0)
)

cat(
  "\nSection 5 completed.",
  "\nTraining skewness (raw price):",
  round(training_distribution_diagnostics$skewness[1], 3),
  "\nTraining skewness (log price):",
  round(training_distribution_diagnostics$skewness[2], 3),
  "\nPreparation charts saved:", 3, "\n"
)


# SECTION 6: Validating and exporting the prepared modelling data ------

# Checking that all prepared modelling subsets and folders exist
stopifnot(
  exists("raw"),
  exists("sligo_clean"),
  exists("sligo_all_records"),
  exists("price_data"),
  exists("train_dev_2019_2023"),
  exists("val_2024"),
  exists("train_2019_2024"),
  exists("test_2025"),
  exists("price_data_all_records"),
  exists("train_2019_2024_all_records"),
  exists("test_2025_all_records"),
  exists("project_dir"),
  exists("output_dir"),
  exists("figures_dir")
)

# Creating a data folder for the exported modelling datasets
data_dir <- file.path(project_dir, "data")
dir.create(data_dir, showWarnings = FALSE)

# Selecting the analytical and modelling fields for export
# Excluding raw street addresses, Eircodes, constant county and empty size
modelling_fields <- c(
  "sale_date",
  "year",
  "month",
  "quarter",
  "period",
  "time_days",
  "time_years",
  "price_eur",
  "log_price_eur",
  "not_full_market_price",
  "vat_exclusive",
  "property_status"
)

# Preparing clean export objects with only the modelling fields
price_data_export <- price_data[modelling_fields]
train_dev_export <- train_dev_2019_2023[modelling_fields]
val_2024_export <- val_2024[modelling_fields]
train_2019_2024_export <- train_2019_2024[modelling_fields]
test_2025_export <- test_2025[modelling_fields]
price_data_all_export <- price_data_all_records[modelling_fields]

# Checking completeness, row counts and factor classes before export
stopifnot(
  nrow(price_data_export) == 5070,
  nrow(train_dev_export) == 3748,
  nrow(val_2024_export) == 647,
  nrow(train_2019_2024_export) == 4395,
  nrow(test_2025_export) == 675,
  nrow(price_data_all_export) == 5074,
  all(complete.cases(price_data_export)),
  all(complete.cases(train_dev_export)),
  all(complete.cases(val_2024_export)),
  all(complete.cases(train_2019_2024_export)),
  all(complete.cases(test_2025_export)),
  all(complete.cases(price_data_all_export)),
  !is.ordered(price_data_export$quarter),
  !is.ordered(train_2019_2024_export$quarter),
  !is.ordered(test_2025_export$quarter),
  identical(levels(train_2019_2024_export$quarter), c("Q1", "Q2", "Q3", "Q4")),
  !any(c("address", "eircode") %in% names(price_data_export))
)


# Exporting the RDS objects for the predictive modelling script

saveRDS(
  price_data_export,
  file.path(data_dir, "price_data.rds")
)

saveRDS(
  train_dev_export,
  file.path(data_dir, "train_dev_2019_2023.rds")
)

saveRDS(
  val_2024_export,
  file.path(data_dir, "val_2024.rds")
)

saveRDS(
  train_2019_2024_export,
  file.path(data_dir, "train_2019_2024.rds")
)

saveRDS(
  test_2025_export,
  file.path(data_dir, "test_2025.rds")
)

saveRDS(
  price_data_all_export,
  file.path(data_dir, "price_data_all_records.rds")
)


# Exporting readable CSV copies of each modelling dataset

write.csv(
  price_data_export,
  file.path(data_dir, "price_data.csv"),
  row.names = FALSE
)

write.csv(
  train_dev_export,
  file.path(data_dir, "train_dev_2019_2023.csv"),
  row.names = FALSE
)

write.csv(
  val_2024_export,
  file.path(data_dir, "val_2024.csv"),
  row.names = FALSE
)

write.csv(
  train_2019_2024_export,
  file.path(data_dir, "train_2019_2024.csv"),
  row.names = FALSE
)

write.csv(
  test_2025_export,
  file.path(data_dir, "test_2025.csv"),
  row.names = FALSE
)

write.csv(
  price_data_all_export,
  file.path(data_dir, "price_data_all_records.csv"),
  row.names = FALSE
)


# Reading back the exported RDS files to verify integrity

check_price_data <- readRDS(file.path(data_dir, "price_data.rds"))
check_train_dev <- readRDS(file.path(data_dir, "train_dev_2019_2023.rds"))
check_val_2024 <- readRDS(file.path(data_dir, "val_2024.rds"))
check_train_main <- readRDS(file.path(data_dir, "train_2019_2024.rds"))
check_test_2025 <- readRDS(file.path(data_dir, "test_2025.rds"))
check_price_all <- readRDS(file.path(data_dir, "price_data_all_records.rds"))

stopifnot(
  identical(check_price_data, price_data_export),
  identical(check_train_dev, train_dev_export),
  identical(check_val_2024, val_2024_export),
  identical(check_train_main, train_2019_2024_export),
  identical(check_test_2025, test_2025_export),
  identical(check_price_all, price_data_all_export)
)

rm(
  check_price_data,
  check_train_dev,
  check_val_2024,
  check_train_main,
  check_test_2025,
  check_price_all
)


# Recording the final modelling preparation audit and notes

final_modelling_prep_audit <- data.frame(
  stage = c(
    "Original master records imported",
    "Additional exact copies set aside",
    "Main records after duplicate policy",
    "Eligible full-market second-hand price records (2019-2025)",
    "Development training records (2019-2023)",
    "Development validation records (2024)",
    "Primary training records (2019-2024)",
    "Held-out final test records (2025)",
    "Eligible price records with exact copies retained (sensitivity)",
    "Exported modelling variables per dataset",
    "Exported RDS dataset files",
    "Exported CSV dataset files",
    "Preparation diagnostic charts saved"
  ),
  count = c(
    nrow(raw),
    nrow(raw) - nrow(sligo_clean),
    nrow(sligo_clean),
    nrow(price_data_export),
    nrow(train_dev_export),
    nrow(val_2024_export),
    nrow(train_2019_2024_export),
    nrow(test_2025_export),
    nrow(price_data_all_export),
    ncol(price_data_export),
    length(list.files(data_dir, pattern = "\\.rds$")),
    length(list.files(data_dir, pattern = "\\.csv$")),
    length(list.files(figures_dir, pattern = "\\.png$"))
  ),
  row.names = NULL
)

print(final_modelling_prep_audit, row.names = FALSE)

write.csv(
  final_modelling_prep_audit,
  file.path(output_dir, "06_final_modelling_prep_audit.csv"),
  row.names = FALSE
)

final_modelling_prep_notes <- c(
  "The compiled master dataset (Sligo_Master_2019_2025.csv) contained 5,964 records.",
  "Applying the CA2 exact-copy policy left 5,957 main records (7 exact-copy pairs set aside).",
  "Applying the full-market, second-hand, VAT Exclusive = No rules left 5,070 eligible price records.",
  "Elapsed time (time_years) was calculated from a fixed origin of 1 January 2019 (365.25 days per year).",
  "Calendar quarter (Q1-Q4) was stored as an unordered factor with Q1 as the reference level.",
  "Natural log-transformed price (log_price_eur) was created alongside raw price_eur.",
  "The dataset was split chronologically into 2019-2024 training (n = 4,395) and held-out 2025 test (n = 675).",
  "An internal development split of 2019-2023 (n = 3,748) and 2024 (n = 647) was also exported.",
  "Addresses and Eircodes were excluded from all exported RDS and CSV modelling datasets.",
  "All 5,074 eligible records with exact copies retained were exported for sensitivity checking."
)

writeLines(
  final_modelling_prep_notes,
  file.path(output_dir, "06_final_modelling_prep_notes.txt")
)

# Recording the R session information for the preparation script
writeLines(
  capture.output(sessionInfo()),
  file.path(output_dir, "06_preparation_session_info.txt")
)

# Checking that all exported files exist and are non-empty
exported_rds_files <- list.files(
  data_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

exported_csv_files <- list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

stopifnot(
  length(exported_rds_files) == 6,
  length(exported_csv_files) == 6,
  all(file.info(exported_rds_files)$size > 0),
  all(file.info(exported_csv_files)$size > 0),
  file.exists(file.path(output_dir, "06_final_modelling_prep_audit.csv")),
  file.exists(file.path(output_dir, "06_final_modelling_prep_notes.txt")),
  file.exists(file.path(output_dir, "06_preparation_session_info.txt"))
)

cat("\nModelling data preparation completed successfully.\n")
cat("Eligible price records (2019-2025):", nrow(price_data_export), "\n")
cat("Development training records (2019-2023):", nrow(train_dev_export), "\n")
cat("Development validation records (2024):", nrow(val_2024_export), "\n")
cat("Primary training records (2019-2024):", nrow(train_2019_2024_export), "\n")
cat("Held-out 2025 test records:", nrow(test_2025_export), "\n")
cat("RDS files exported:", length(exported_rds_files), "\n")
cat("CSV files exported:", length(exported_csv_files), "\n")
