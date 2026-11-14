# SECTION 1: Loading prepared splits and defining candidate models

# Loading the modelling splits exported by Modelling_Data_Preparation.R

# Checking project folder
project_dir <- "/Users/corey/Documents/Education/Data Analytics/ATU - Big Data Analytics/Data Science/CA 3/CA3/CA3"

stopifnot(
  dir.exists(project_dir)
)

# Setting paths to data, output and figures folders
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "output")
figures_dir <- file.path(project_dir, "figures")

# Creating output and figures folders
dir.create(output_dir, showWarnings = FALSE)
dir.create(figures_dir, showWarnings = FALSE)

# Checking prepared RDS files
stopifnot(
  file.exists(file.path(data_dir, "price_data.rds")),
  file.exists(file.path(data_dir, "train_dev_2019_2023.rds")),
  file.exists(file.path(data_dir, "val_2024.rds")),
  file.exists(file.path(data_dir, "train_2019_2024.rds")),
  file.exists(file.path(data_dir, "test_2025.rds")),
  file.exists(file.path(data_dir, "price_data_all_records.rds"))
)

# Loading main modelling and split datasets
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

# Loading all formatted eligible records for the duplicate sensitivity check
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

# Checking main modelling datasets before fitting
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

# Checking sensitivity-check datasets before fitting
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

# Setting significance level, date origin and chart colours
alpha <- 0.05
origin_date <- as.Date("2019-01-01")
days_per_year <- 365.25
chart_colour <- "#FF7F50"
secondary_colour <- "#5B7083"

# Defining candidate regression formulas
model_a_formula <- price_eur ~ time_years + quarter
model_b_formula <- log_price_eur ~ time_years + quarter

# # CA3 research question and candidate models
#
# ## Research question
# Dataset: price_data (train_2019_2024 and test_2025)
# Variables: price_eur, log_price_eur, time_years and quarter
#
# ## Candidate models
# Baseline (Mean): Training-set mean price_eur
# Baseline (Median): Training-set median price_eur
# Model A: price_eur ~ time_years + quarter
# Model B: log_price_eur ~ time_years + quarter
# Back-transformation: exp(y_hat) and Duan smearing exp(y_hat) * mean(exp(e_train))
#
# ## Splits
# Development split: 2019-2023 (n = 3,748) vs 2024 (n = 647)
# Main split: 2019-2024 (n = 4,395) vs 2025 (n = 675)

# Displaying loaded dataset counts
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


# SECTION 2: Checking candidate models on the 2019-2023 development split

# Checking development datasets and formulas
stopifnot(
  exists("train_dev_2019_2023"),
  exists("val_2024"),
  exists("model_a_formula"),
  exists("model_b_formula"),
  exists("output_dir")
)

# Defining error metric helper (error = predicted - actual)
calculate_error_metrics <- function(actual, predicted, model_name, split_name) {

  stopifnot(
    is.numeric(actual),
    is.numeric(predicted),
    length(actual) == length(predicted),
    length(actual) > 0,
    !anyNA(actual),
    !anyNA(predicted),
    all(is.finite(actual)),
    all(is.finite(predicted)),
    all(actual > 0)
  )

  err <- predicted - actual
  abs_err <- abs(err)
  pct_err <- 100 * abs_err / actual

  data.frame(
    split = split_name,
    model = model_name,
    records = length(actual),
    mean_actual_eur = mean(actual),
    median_actual_eur = median(actual),
    mean_predicted_eur = mean(predicted),
    median_predicted_eur = median(predicted),
    mae_eur = mean(abs_err),
    rmse_eur = sqrt(mean(err^2)),
    mpe_eur = mean(err),
    medae_eur = median(abs_err),
    mape_percent = mean(pct_err),
    mdape_percent = median(pct_err),
    row.names = NULL
  )
}

# Calculating 2019-2023 baseline benchmarks
dev_mean_price <- mean(train_dev_2019_2023$price_eur)
dev_median_price <- median(train_dev_2019_2023$price_eur)

# Fitting Model A and Model B on 2019-2023 records
dev_model_a <- lm(
  model_a_formula,
  data = train_dev_2019_2023
)

dev_model_b <- lm(
  model_b_formula,
  data = train_dev_2019_2023
)

dev_summary_a <- summary(dev_model_a)
dev_summary_b <- summary(dev_model_b)

# Calculating Duan smearing factor from 2019-2023 log residuals
dev_smearing_factor <- mean(exp(residuals(dev_model_b)))

cat("\nDevelopment split (2019-2023) training summaries:\n")
cat("Baseline mean (EUR):", round(dev_mean_price, 2), "\n")
cat("Baseline median (EUR):", round(dev_median_price, 2), "\n")
cat("Model A R-squared:", round(dev_summary_a$r.squared, 5), "\n")
cat("Model B R-squared (log scale):", round(dev_summary_b$r.squared, 5), "\n")
cat("Model B Duan smearing factor:", round(dev_smearing_factor, 6), "\n")

# Generating predictions on 2024 validation records
val_2024_pred_mean <- rep(dev_mean_price, nrow(val_2024))
val_2024_pred_median <- rep(dev_median_price, nrow(val_2024))
val_2024_pred_model_a <- unname(predict(dev_model_a, newdata = val_2024))

val_2024_pred_log_b <- unname(predict(dev_model_b, newdata = val_2024))
val_2024_pred_model_b_exp <- exp(val_2024_pred_log_b)
val_2024_pred_model_b_smear <- val_2024_pred_model_b_exp * dev_smearing_factor

# Comparing 2024 validation errors across candidate models
dev_validation_comparison <- rbind(
  calculate_error_metrics(
    val_2024$price_eur,
    val_2024_pred_mean,
    "Baseline (Mean)",
    "2024 validation (trained on 2019-2023)"
  ),
  calculate_error_metrics(
    val_2024$price_eur,
    val_2024_pred_median,
    "Baseline (Median)",
    "2024 validation (trained on 2019-2023)"
  ),
  calculate_error_metrics(
    val_2024$price_eur,
    val_2024_pred_model_a,
    "Model A (Linear price)",
    "2024 validation (trained on 2019-2023)"
  ),
  calculate_error_metrics(
    val_2024$price_eur,
    val_2024_pred_model_b_exp,
    "Model B (Log-linear, exp back-transform)",
    "2024 validation (trained on 2019-2023)"
  ),
  calculate_error_metrics(
    val_2024$price_eur,
    val_2024_pred_model_b_smear,
    "Model B (Log-linear, Duan smearing)",
    "2024 validation (trained on 2019-2023)"
  )
)

# Adding training R-squared values from 2019-2023 fits
dev_validation_comparison$train_r_squared <- c(
  NA_real_,
  NA_real_,
  dev_summary_a$r.squared,
  dev_summary_b$r.squared,
  dev_summary_b$r.squared
)

dev_validation_comparison$train_adj_r_squared <- c(
  NA_real_,
  NA_real_,
  dev_summary_a$adj.r.squared,
  dev_summary_b$adj.r.squared,
  dev_summary_b$adj.r.squared
)

cat("\n2024 validation results (models trained on 2019-2023):\n")
print(dev_validation_comparison, row.names = FALSE, digits = 6)

# Saving development split comparison
write.csv(
  dev_validation_comparison,
  file.path(output_dir, "07_development_split_2019_2023_vs_2024.csv"),
  row.names = FALSE
)

cat(
  "\nSection 2 completed.",
  "\n2019-2023 training records:", nrow(train_dev_2019_2023),
  "\n2024 validation records:", nrow(val_2024),
  "\n"
)


# SECTION 3: Fitting candidate models on the 2019-2024 training set

# Checking training dataset, formulas and output folder
stopifnot(
  exists("train_2019_2024"),
  exists("model_a_formula"),
  exists("model_b_formula"),
  exists("output_dir"),
  exists("alpha")
)

# Calculating 2019-2024 baseline mean and median prices
baseline_mean_price <- mean(train_2019_2024$price_eur)
baseline_median_price <- median(train_2019_2024$price_eur)

# Fitting Model A (euro scale) and Model B (log scale) on 2019-2024 records
model_a <- lm(
  model_a_formula,
  data = train_2019_2024
)

model_b <- lm(
  model_b_formula,
  data = train_2019_2024
)

summary_a <- summary(model_a)
summary_b <- summary(model_b)

confint_a <- confint(model_a, level = 1 - alpha)
confint_b <- confint(model_b, level = 1 - alpha)

# Calculating Duan smearing and normal-theory retransformation factors
smearing_factor_b <- mean(exp(residuals(model_b)))
lognormal_factor_b <- exp((summary_b$sigma^2) / 2)

cat("\nModel A summary (2019-2024 training set):\n")
print(summary_a)

cat("\nModel B summary (2019-2024 training set):\n")
print(summary_b)

# Extracting coefficient table for Model A and Model B
coef_mat_a <- summary_a$coefficients
coef_mat_b <- summary_b$coefficients

coef_table_a <- data.frame(
  model = "Model A (Linear price)",
  response_scale = "price_eur",
  term = rownames(coef_mat_a),
  estimate = coef_mat_a[, "Estimate"],
  std_error = coef_mat_a[, "Std. Error"],
  t_value = coef_mat_a[, "t value"],
  p_value = coef_mat_a[, "Pr(>|t|)"],
  ci_95_lower = confint_a[, 1],
  ci_95_upper = confint_a[, 2],
  exp_estimate = NA_real_,
  exp_ci_95_lower = NA_real_,
  exp_ci_95_upper = NA_real_,
  percent_change_estimate = NA_real_,
  row.names = NULL
)

is_slope_b <- rownames(coef_mat_b) != "(Intercept)"

coef_table_b <- data.frame(
  model = "Model B (Log-linear price)",
  response_scale = "log_price_eur",
  term = rownames(coef_mat_b),
  estimate = coef_mat_b[, "Estimate"],
  std_error = coef_mat_b[, "Std. Error"],
  t_value = coef_mat_b[, "t value"],
  p_value = coef_mat_b[, "Pr(>|t|)"],
  ci_95_lower = confint_b[, 1],
  ci_95_upper = confint_b[, 2],
  exp_estimate = exp(coef_mat_b[, "Estimate"]),
  exp_ci_95_lower = exp(confint_b[, 1]),
  exp_ci_95_upper = exp(confint_b[, 2]),
  percent_change_estimate = ifelse(
    is_slope_b,
    100 * (exp(coef_mat_b[, "Estimate"]) - 1),
    NA_real_
  ),
  row.names = NULL
)

candidate_model_coefficients <- rbind(
  coef_table_a,
  coef_table_b
)

cat("\nCandidate model coefficients (2019-2024 training set):\n")
print(candidate_model_coefficients, row.names = FALSE, digits = 6)

# Calculating overall F-test p-values for Model A and Model B
f_stat_a <- unname(summary_a$fstatistic["value"])
f_df1_a <- unname(summary_a$fstatistic["numdf"])
f_df2_a <- unname(summary_a$fstatistic["dendf"])
f_pval_a <- pf(f_stat_a, f_df1_a, f_df2_a, lower.tail = FALSE)

f_stat_b <- unname(summary_b$fstatistic["value"])
f_df1_b <- unname(summary_b$fstatistic["numdf"])
f_df2_b <- unname(summary_b$fstatistic["dendf"])
f_pval_b <- pf(f_stat_b, f_df1_b, f_df2_b, lower.tail = FALSE)

# Summarising training fit statistics across candidate specifications
candidate_model_fit_summary <- data.frame(
  model = c(
    "Baseline (Mean)",
    "Baseline (Median)",
    "Model A (Linear price)",
    "Model B (Log-linear price)"
  ),
  dataset = "train_2019_2024",
  records = nrow(train_2019_2024),
  response_scale = c(
    "price_eur",
    "price_eur",
    "price_eur",
    "log_price_eur"
  ),
  parameters = c(
    1L,
    1L,
    length(coef(model_a)),
    length(coef(model_b))
  ),
  residual_df = c(
    nrow(train_2019_2024) - 1L,
    nrow(train_2019_2024) - 1L,
    df.residual(model_a),
    df.residual(model_b)
  ),
  r_squared = c(
    0,
    NA_real_,
    summary_a$r.squared,
    summary_b$r.squared
  ),
  adj_r_squared = c(
    0,
    NA_real_,
    summary_a$adj.r.squared,
    summary_b$adj.r.squared
  ),
  residual_se = c(
    sd(train_2019_2024$price_eur),
    NA_real_,
    summary_a$sigma,
    summary_b$sigma
  ),
  f_statistic = c(
    NA_real_,
    NA_real_,
    f_stat_a,
    f_stat_b
  ),
  f_df1 = c(
    NA_real_,
    NA_real_,
    f_df1_a,
    f_df1_b
  ),
  f_df2 = c(
    NA_real_,
    NA_real_,
    f_df2_a,
    f_df2_b
  ),
  f_p_value = c(
    NA_real_,
    NA_real_,
    f_pval_a,
    f_pval_b
  ),
  aic = c(
    NA_real_,
    NA_real_,
    AIC(model_a),
    AIC(model_b)
  ),
  bic = c(
    NA_real_,
    NA_real_,
    BIC(model_a),
    BIC(model_b)
  ),
  smearing_factor = c(
    NA_real_,
    NA_real_,
    NA_real_,
    smearing_factor_b
  ),
  lognormal_factor = c(
    NA_real_,
    NA_real_,
    NA_real_,
    lognormal_factor_b
  ),
  row.names = NULL
)

cat("\nCandidate model fit summary (2019-2024 training set):\n")
print(candidate_model_fit_summary, row.names = FALSE, digits = 6)

# Saving coefficient and fit summary tables
write.csv(
  candidate_model_coefficients,
  file.path(output_dir, "08_candidate_model_coefficients.csv"),
  row.names = FALSE
)

write.csv(
  candidate_model_fit_summary,
  file.path(output_dir, "09_candidate_model_fit_summary.csv"),
  row.names = FALSE
)

cat(
  "\nSection 3 completed.",
  "\nTraining baseline mean (EUR):", round(baseline_mean_price, 2),
  "\nTraining baseline median (EUR):", round(baseline_median_price, 2),
  "\nModel A R-squared:", round(summary_a$r.squared, 5),
  "\nModel B R-squared:", round(summary_b$r.squared, 5),
  "\nModel B Duan smearing factor:", round(smearing_factor_b, 6),
  "\n"
)
