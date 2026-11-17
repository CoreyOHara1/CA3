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


# SECTION 4: Checking residual diagnostics and model assumptions

# Checking fitted models, training dataset and folders
stopifnot(
  exists("model_a"),
  exists("model_b"),
  exists("train_2019_2024"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("chart_colour"),
  exists("secondary_colour")
)

# Extracting fitted values, raw residuals and standardised residuals
fitted_a <- fitted(model_a)
resid_a <- residuals(model_a)
std_resid_a <- rstandard(model_a)

fitted_b <- fitted(model_b)
resid_b <- residuals(model_b)
std_resid_b <- rstandard(model_b)

# Defining skewness and excess kurtosis helpers
calculate_skew <- function(x) {
  mean(((x - mean(x)) / sd(x))^3)
}

calculate_excess_kurtosis <- function(x) {
  mean(((x - mean(x)) / sd(x))^4) - 3
}

# Checking sample size before running Shapiro-Wilk tests on residuals
stopifnot(
  length(resid_a) >= 3,
  length(resid_a) <= 5000,
  length(resid_b) >= 3,
  length(resid_b) <= 5000
)

shapiro_resid_a <- shapiro.test(resid_a)
shapiro_resid_b <- shapiro.test(resid_b)

# Testing scale-location correlation between sqrt(|std_resid|) and fitted values
scale_loc_cor_a <- cor.test(
  fitted_a,
  sqrt(abs(std_resid_a)),
  method = "spearman",
  exact = FALSE
)

scale_loc_cor_b <- cor.test(
  fitted_b,
  sqrt(abs(std_resid_b)),
  method = "spearman",
  exact = FALSE
)

# Summarising residual diagnostics for Model A and Model B
residual_assumptions_audit <- data.frame(
  model = c(
    "Model A (Linear price)",
    "Model B (Log-linear price)"
  ),
  response_scale = c(
    "price_eur",
    "log_price_eur"
  ),
  records = c(
    length(resid_a),
    length(resid_b)
  ),
  mean_residual = c(
    mean(resid_a),
    mean(resid_b)
  ),
  median_residual = c(
    median(resid_a),
    median(resid_b)
  ),
  sd_residual = c(
    sd(resid_a),
    sd(resid_b)
  ),
  iqr_residual = c(
    IQR(resid_a),
    IQR(resid_b)
  ),
  min_residual = c(
    min(resid_a),
    min(resid_b)
  ),
  max_residual = c(
    max(resid_a),
    max(resid_b)
  ),
  residual_skewness = c(
    calculate_skew(resid_a),
    calculate_skew(resid_b)
  ),
  residual_excess_kurtosis = c(
    calculate_excess_kurtosis(resid_a),
    calculate_excess_kurtosis(resid_b)
  ),
  shapiro_W = c(
    unname(shapiro_resid_a$statistic),
    unname(shapiro_resid_b$statistic)
  ),
  shapiro_p_value = c(
    shapiro_resid_a$p.value,
    shapiro_resid_b$p.value
  ),
  scale_location_spearman_rho = c(
    unname(scale_loc_cor_a$estimate),
    unname(scale_loc_cor_b$estimate)
  ),
  scale_location_p_value = c(
    scale_loc_cor_a$p.value,
    scale_loc_cor_b$p.value
  ),
  std_resid_gt_2_count = c(
    sum(abs(std_resid_a) > 2),
    sum(abs(std_resid_b) > 2)
  ),
  std_resid_gt_3_count = c(
    sum(abs(std_resid_a) > 3),
    sum(abs(std_resid_b) > 3)
  ),
  row.names = NULL
)

cat("\nResidual diagnostics summary (2019-2024 training set):\n")
print(residual_assumptions_audit, row.names = FALSE, digits = 6)

# Saving residual diagnostics table
write.csv(
  residual_assumptions_audit,
  file.path(output_dir, "10_residual_assumptions_audit.csv"),
  row.names = FALSE
)


# Creating Model A residual diagnostic chart

fig4_path <- file.path(
  figures_dir,
  "04_model_a_residual_diagnostics.png"
)

png(
  fig4_path,
  width = 2200,
  height = 1700,
  res = 180
)

par(
  mfrow = c(2, 2),
  mar = c(4.5, 4.5, 3.5, 1.5),
  las = 1
)

fitted_a_k <- fitted_a / 1000
resid_a_k <- resid_a / 1000

plot(
  x = fitted_a_k,
  y = resid_a_k,
  pch = 16,
  cex = 0.45,
  col = adjustcolor(chart_colour, alpha.f = 0.4),
  main = "Model A: Residuals vs fitted values",
  xlab = "Fitted price (EUR thousands)",
  ylab = "Residual (EUR thousands)"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)
lines(
  lowess(fitted_a_k, resid_a_k),
  col = secondary_colour,
  lwd = 2
)

qqnorm(
  std_resid_a,
  pch = 16,
  cex = 0.45,
  col = adjustcolor(chart_colour, alpha.f = 0.45),
  main = "Model A: Normal Q-Q plot of standardised residuals",
  xlab = "Normal quantile",
  ylab = "Standardised residual"
)
qqline(std_resid_a, col = "black", lwd = 2)

plot(
  x = fitted_a_k,
  y = sqrt(abs(std_resid_a)),
  pch = 16,
  cex = 0.45,
  col = adjustcolor(chart_colour, alpha.f = 0.4),
  main = "Model A: Scale-location plot",
  xlab = "Fitted price (EUR thousands)",
  ylab = "Sqrt(|Standardised residual|)"
)
lines(
  lowess(fitted_a_k, sqrt(abs(std_resid_a))),
  col = "black",
  lwd = 2
)

hist(
  resid_a_k,
  breaks = 45,
  col = chart_colour,
  border = "white",
  main = "Model A: Residual distribution (EUR thousands)",
  xlab = "Residual (EUR thousands)",
  ylab = "Records"
)
abline(v = 0, col = "black", lwd = 2, lty = 2)

dev.off()


# Creating Model B residual diagnostic chart

fig5_path <- file.path(
  figures_dir,
  "05_model_b_residual_diagnostics.png"
)

png(
  fig5_path,
  width = 2200,
  height = 1700,
  res = 180
)

par(
  mfrow = c(2, 2),
  mar = c(4.5, 4.5, 3.5, 1.5),
  las = 1
)

plot(
  x = fitted_b,
  y = resid_b,
  pch = 16,
  cex = 0.45,
  col = adjustcolor(secondary_colour, alpha.f = 0.4),
  main = "Model B: Residuals vs fitted values (log scale)",
  xlab = "Fitted log price, log(EUR)",
  ylab = "Residual, log(EUR)"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)
lines(
  lowess(fitted_b, resid_b),
  col = chart_colour,
  lwd = 2
)

qqnorm(
  std_resid_b,
  pch = 16,
  cex = 0.45,
  col = adjustcolor(secondary_colour, alpha.f = 0.45),
  main = "Model B: Normal Q-Q plot of standardised residuals",
  xlab = "Normal quantile",
  ylab = "Standardised residual"
)
qqline(std_resid_b, col = "black", lwd = 2)

plot(
  x = fitted_b,
  y = sqrt(abs(std_resid_b)),
  pch = 16,
  cex = 0.45,
  col = adjustcolor(secondary_colour, alpha.f = 0.4),
  main = "Model B: Scale-location plot (log scale)",
  xlab = "Fitted log price, log(EUR)",
  ylab = "Sqrt(|Standardised residual|)"
)
lines(
  lowess(fitted_b, sqrt(abs(std_resid_b))),
  col = "black",
  lwd = 2
)

hist(
  resid_b,
  breaks = 45,
  col = secondary_colour,
  border = "white",
  main = "Model B: Residual distribution (log scale)",
  xlab = "Residual, log(EUR)",
  ylab = "Records"
)
abline(v = 0, col = "black", lwd = 2, lty = 2)

dev.off()

# Checking saved diagnostic charts
diagnostic_charts <- c(fig4_path, fig5_path)

stopifnot(
  all(file.exists(diagnostic_charts)),
  all(file.info(diagnostic_charts)$size > 0)
)

cat(
  "\nSection 4 completed.",
  "\nModel A residual skewness:", round(residual_assumptions_audit$residual_skewness[1], 3),
  "\nModel B residual skewness:", round(residual_assumptions_audit$residual_skewness[2], 3),
  "\nModel A Shapiro-Wilk W:", round(residual_assumptions_audit$shapiro_W[1], 4),
  "\nModel B Shapiro-Wilk W:", round(residual_assumptions_audit$shapiro_W[2], 4),
  "\nDiagnostic charts saved:", length(diagnostic_charts),
  "\n"
)


# SECTION 5: Checking influential records and residuals over time

# Checking fitted models, training dataset and folders
stopifnot(
  exists("model_a"),
  exists("model_b"),
  exists("train_2019_2024"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("chart_colour"),
  exists("secondary_colour")
)

# Calculating leverage and Cook's distance for Model A and Model B
hat_a <- hatvalues(model_a)
cooks_a <- cooks.distance(model_a)

hat_b <- hatvalues(model_b)
cooks_b <- cooks.distance(model_b)

# Setting standard 4/n threshold for Cook's distance
n_train <- nrow(train_2019_2024)
cooks_cutoff <- 4 / n_train

# Checking lag-1 residual correlation in chronological sale-date order
date_order <- order(train_2019_2024$sale_date)
lag1_acf_a <- acf(residuals(model_a)[date_order], lag.max = 1, plot = FALSE)$acf[2]
lag1_acf_b <- acf(residuals(model_b)[date_order], lag.max = 1, plot = FALSE)$acf[2]

cat("\nInfluence and temporal residual checks (2019-2024 training set):\n")
cat("Cook's distance 4/n threshold:", round(cooks_cutoff, 6), "\n")
cat("Model A records with Cook's D > 4/n:", sum(cooks_a > cooks_cutoff), "\n")
cat("Model A max Cook's D:", round(max(cooks_a), 5), "\n")
cat("Model B records with Cook's D > 4/n:", sum(cooks_b > cooks_cutoff), "\n")
cat("Model B max Cook's D:", round(max(cooks_b), 5), "\n")
cat("Model A lag-1 residual ACF:", round(lag1_acf_a, 4), "\n")
cat("Model B lag-1 residual ACF:", round(lag1_acf_b, 4), "\n")

# Extracting top 10 influential observations for Model A and Model B
top_idx_a <- head(order(cooks_a, decreasing = TRUE), 10)
top_idx_b <- head(order(cooks_b, decreasing = TRUE), 10)

build_influence_table <- function(indices, model_label, fit_vals, res_vals, std_res, hat_vals, cooks_vals) {
  data.frame(
    model = model_label,
    rank = seq_along(indices),
    train_row = indices,
    sale_date = format(train_2019_2024$sale_date[indices], "%Y-%m-%d"),
    year = train_2019_2024$year[indices],
    quarter = as.character(train_2019_2024$quarter[indices]),
    time_years = train_2019_2024$time_years[indices],
    price_eur = train_2019_2024$price_eur[indices],
    log_price_eur = train_2019_2024$log_price_eur[indices],
    fitted_value = fit_vals[indices],
    residual = res_vals[indices],
    std_residual = std_res[indices],
    leverage = hat_vals[indices],
    cooks_distance = cooks_vals[indices],
    exceeds_4_over_n = cooks_vals[indices] > cooks_cutoff,
    row.names = NULL
  )
}

influential_observations_summary <- rbind(
  build_influence_table(
    top_idx_a,
    "Model A (Linear price)",
    fitted(model_a),
    residuals(model_a),
    rstandard(model_a),
    hat_a,
    cooks_a
  ),
  build_influence_table(
    top_idx_b,
    "Model B (Log-linear price)",
    fitted(model_b),
    residuals(model_b),
    rstandard(model_b),
    hat_b,
    cooks_b
  )
)

cat("\nTop 10 influential training records per model:\n")
print(influential_observations_summary, row.names = FALSE, digits = 6)

# Saving influential observations table
write.csv(
  influential_observations_summary,
  file.path(output_dir, "11_influential_observations_summary.csv"),
  row.names = FALSE
)


# Plotting Cook's distance for Model A and Model B

fig6_path <- file.path(
  figures_dir,
  "06_cooks_distance_and_influence.png"
)

png(
  fig6_path,
  width = 2400,
  height = 1800,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(4.5, 5, 3.5, 1.5),
  las = 1
)

plot(
  seq_len(n_train),
  cooks_a,
  type = "h",
  col = adjustcolor(chart_colour, alpha.f = 0.75),
  main = paste0(
    "Model A: Cook's distance across training records (4/n = ",
    round(cooks_cutoff, 5),
    ")"
  ),
  xlab = "Training observation index (2019-2024)",
  ylab = "Cook's distance"
)
abline(h = cooks_cutoff, col = "black", lwd = 2, lty = 2)

plot(
  seq_len(n_train),
  cooks_b,
  type = "h",
  col = adjustcolor(secondary_colour, alpha.f = 0.75),
  main = paste0(
    "Model B: Cook's distance across training records (4/n = ",
    round(cooks_cutoff, 5),
    ")"
  ),
  xlab = "Training observation index (2019-2024)",
  ylab = "Cook's distance"
)
abline(h = cooks_cutoff, col = "black", lwd = 2, lty = 2)

dev.off()


# Plotting standardised residuals over sale date for Model A and Model B

fig7_path <- file.path(
  figures_dir,
  "07_residuals_over_sale_date.png"
)

png(
  fig7_path,
  width = 2400,
  height = 1800,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(4.5, 5, 3.5, 1.5),
  las = 1
)

std_res_a <- rstandard(model_a)
std_res_b <- rstandard(model_b)

plot(
  x = train_2019_2024$sale_date,
  y = std_res_a,
  pch = 16,
  cex = 0.45,
  col = adjustcolor(chart_colour, alpha.f = 0.4),
  main = "Model A: Standardised residuals over sale date (2019-2024)",
  xlab = "Sale date",
  ylab = "Standardised residual"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)
lines(
  lowess(as.numeric(train_2019_2024$sale_date), std_res_a),
  col = secondary_colour,
  lwd = 2
)

plot(
  x = train_2019_2024$sale_date,
  y = std_res_b,
  pch = 16,
  cex = 0.45,
  col = adjustcolor(secondary_colour, alpha.f = 0.4),
  main = "Model B: Standardised residuals over sale date (2019-2024)",
  xlab = "Sale date",
  ylab = "Standardised residual"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)
lines(
  lowess(as.numeric(train_2019_2024$sale_date), std_res_b),
  col = chart_colour,
  lwd = 2
)

dev.off()

# Checking saved influence and temporal charts
influence_charts <- c(fig6_path, fig7_path)

stopifnot(
  all(file.exists(influence_charts)),
  all(file.info(influence_charts)$size > 0)
)

cat(
  "\nSection 5 completed.",
  "\nModel A records with Cook's D > 4/n:", sum(cooks_a > cooks_cutoff),
  "\nModel B records with Cook's D > 4/n:", sum(cooks_b > cooks_cutoff),
  "\nInfluence and temporal charts saved:", length(influence_charts),
  "\n"
)


# SECTION 6: Checking sensitivity to influential records and duplicate policy

# Checking fitted models, training datasets and Cook's distance objects
stopifnot(
  exists("model_a"),
  exists("model_b"),
  exists("model_a_formula"),
  exists("model_b_formula"),
  exists("train_2019_2024"),
  exists("train_2019_2024_all_records"),
  exists("cooks_a"),
  exists("cooks_b"),
  exists("cooks_cutoff"),
  exists("top_idx_a"),
  exists("top_idx_b"),
  exists("output_dir")
)

# Refitting Model A and Model B without top 10 Cook's distance records
model_a_no_top10 <- lm(
  model_a_formula,
  data = train_2019_2024[-top_idx_a, ]
)

model_b_no_top10 <- lm(
  model_b_formula,
  data = train_2019_2024[-top_idx_b, ]
)

# Refitting Model A and Model B without records exceeding 4/n
model_a_no_4n <- lm(
  model_a_formula,
  data = train_2019_2024[cooks_a <= cooks_cutoff, ]
)

model_b_no_4n <- lm(
  model_b_formula,
  data = train_2019_2024[cooks_b <= cooks_cutoff, ]
)

# Refitting Model A and Model B with all exact copies retained
model_a_all_records <- lm(
  model_a_formula,
  data = train_2019_2024_all_records
)

model_b_all_records <- lm(
  model_b_formula,
  data = train_2019_2024_all_records
)

# Defining helper to extract sensitivity comparison row
extract_sensitivity_row <- function(fit_obj, model_label, spec_label, is_log_model) {
  fit_sum <- summary(fit_obj)
  coefs <- fit_sum$coefficients

  data.frame(
    model = model_label,
    specification = spec_label,
    records = length(residuals(fit_obj)),
    r_squared = fit_sum$r.squared,
    adj_r_squared = fit_sum$adj.r.squared,
    residual_se = fit_sum$sigma,
    intercept = unname(coefs["(Intercept)", "Estimate"]),
    slope_time_years = unname(coefs["time_years", "Estimate"]),
    time_years_p_value = unname(coefs["time_years", "Pr(>|t|)"]),
    coef_quarterQ2 = unname(coefs["quarterQ2", "Estimate"]),
    coef_quarterQ3 = unname(coefs["quarterQ3", "Estimate"]),
    coef_quarterQ4 = unname(coefs["quarterQ4", "Estimate"]),
    smearing_factor = ifelse(
      is_log_model,
      mean(exp(residuals(fit_obj))),
      NA_real_
    ),
    row.names = NULL
  )
}

# Combining sensitivity fits across Model A and Model B
influence_and_duplicate_sensitivity <- rbind(
  extract_sensitivity_row(
    model_a,
    "Model A (Linear price)",
    "Main training set (first exact copy retained)",
    FALSE
  ),
  extract_sensitivity_row(
    model_a_no_top10,
    "Model A (Linear price)",
    "Excluding top 10 Cook's D records",
    FALSE
  ),
  extract_sensitivity_row(
    model_a_no_4n,
    "Model A (Linear price)",
    "Excluding all Cook's D > 4/n records",
    FALSE
  ),
  extract_sensitivity_row(
    model_a_all_records,
    "Model A (Linear price)",
    "All formatted records (exact copies retained)",
    FALSE
  ),
  extract_sensitivity_row(
    model_b,
    "Model B (Log-linear price)",
    "Main training set (first exact copy retained)",
    TRUE
  ),
  extract_sensitivity_row(
    model_b_no_top10,
    "Model B (Log-linear price)",
    "Excluding top 10 Cook's D records",
    TRUE
  ),
  extract_sensitivity_row(
    model_b_no_4n,
    "Model B (Log-linear price)",
    "Excluding all Cook's D > 4/n records",
    TRUE
  ),
  extract_sensitivity_row(
    model_b_all_records,
    "Model B (Log-linear price)",
    "All formatted records (exact copies retained)",
    TRUE
  )
)

cat("\nSensitivity analysis: influential records and duplicate policy:\n")
print(influence_and_duplicate_sensitivity, row.names = FALSE, digits = 6)

# Saving sensitivity comparison table
write.csv(
  influence_and_duplicate_sensitivity,
  file.path(output_dir, "12_influence_and_duplicate_sensitivity.csv"),
  row.names = FALSE
)

cat(
  "\nSection 6 completed.",
  "\nSensitivity specifications compared:", nrow(influence_and_duplicate_sensitivity),
  "\n"
)


# SECTION 7: Validating candidate models on held-out 2025 records

# Checking held-out 2025 datasets, fitted models and helpers
stopifnot(
  exists("test_2025"),
  exists("test_2025_all_records"),
  exists("baseline_mean_price"),
  exists("baseline_median_price"),
  exists("model_a"),
  exists("model_b"),
  exists("smearing_factor_b"),
  exists("model_a_all_records"),
  exists("model_b_all_records"),
  exists("calculate_error_metrics"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("chart_colour"),
  exists("secondary_colour")
)

# Generating 2025 out-of-sample predictions from 2019-2024 models
test_2025_pred_mean <- rep(baseline_mean_price, nrow(test_2025))
test_2025_pred_median <- rep(baseline_median_price, nrow(test_2025))
test_2025_pred_model_a <- unname(predict(model_a, newdata = test_2025))

test_2025_pred_log_b <- unname(predict(model_b, newdata = test_2025))
test_2025_pred_model_b_exp <- exp(test_2025_pred_log_b)
test_2025_pred_model_b_smear <- test_2025_pred_model_b_exp * smearing_factor_b

# Generating 2025 duplicate-sensitivity predictions on all 2025 records
smearing_factor_b_all <- mean(exp(residuals(model_b_all_records)))
test_2025_all_pred_model_a <- unname(
  predict(model_a_all_records, newdata = test_2025_all_records)
)
test_2025_all_pred_log_b <- unname(
  predict(model_b_all_records, newdata = test_2025_all_records)
)
test_2025_all_pred_model_b_exp <- exp(test_2025_all_pred_log_b)
test_2025_all_pred_model_b_smear <- (
  test_2025_all_pred_model_b_exp * smearing_factor_b_all
)

# Checking 2025 prediction vectors
stopifnot(
  length(test_2025_pred_model_a) == nrow(test_2025),
  length(test_2025_pred_model_b_exp) == nrow(test_2025),
  length(test_2025_pred_model_b_smear) == nrow(test_2025),
  !anyNA(test_2025_pred_model_a),
  !anyNA(test_2025_pred_model_b_exp),
  !anyNA(test_2025_pred_model_b_smear),
  all(test_2025_pred_model_a > 0),
  all(test_2025_pred_model_b_exp > 0),
  all(test_2025_pred_model_b_smear > 0)
)

# Comparing held-out 2025 validation metrics across candidate models
heldout_2025_validation_metrics <- rbind(
  calculate_error_metrics(
    test_2025$price_eur,
    test_2025_pred_mean,
    "Baseline (Mean)",
    "2025 held-out test (main, n = 675)"
  ),
  calculate_error_metrics(
    test_2025$price_eur,
    test_2025_pred_median,
    "Baseline (Median)",
    "2025 held-out test (main, n = 675)"
  ),
  calculate_error_metrics(
    test_2025$price_eur,
    test_2025_pred_model_a,
    "Model A (Linear price)",
    "2025 held-out test (main, n = 675)"
  ),
  calculate_error_metrics(
    test_2025$price_eur,
    test_2025_pred_model_b_exp,
    "Model B (Log-linear, exp back-transform)",
    "2025 held-out test (main, n = 675)"
  ),
  calculate_error_metrics(
    test_2025$price_eur,
    test_2025_pred_model_b_smear,
    "Model B (Log-linear, Duan smearing)",
    "2025 held-out test (main, n = 675)"
  ),
  calculate_error_metrics(
    test_2025_all_records$price_eur,
    test_2025_all_pred_model_a,
    "Model A (Linear price, all records)",
    "2025 held-out test (exact copies retained, n = 676)"
  ),
  calculate_error_metrics(
    test_2025_all_records$price_eur,
    test_2025_all_pred_model_b_exp,
    "Model B (Log-linear, exp, all records)",
    "2025 held-out test (exact copies retained, n = 676)"
  ),
  calculate_error_metrics(
    test_2025_all_records$price_eur,
    test_2025_all_pred_model_b_smear,
    "Model B (Log-linear, Duan smearing, all records)",
    "2025 held-out test (exact copies retained, n = 676)"
  )
)

# Adding log-scale MAE and RMSE on held-out 2025 records
pred_list_2025 <- list(
  test_2025_pred_mean,
  test_2025_pred_median,
  test_2025_pred_model_a,
  test_2025_pred_model_b_exp,
  test_2025_pred_model_b_smear,
  test_2025_all_pred_model_a,
  test_2025_all_pred_model_b_exp,
  test_2025_all_pred_model_b_smear
)

actual_log_list_2025 <- list(
  test_2025$log_price_eur,
  test_2025$log_price_eur,
  test_2025$log_price_eur,
  test_2025$log_price_eur,
  test_2025$log_price_eur,
  test_2025_all_records$log_price_eur,
  test_2025_all_records$log_price_eur,
  test_2025_all_records$log_price_eur
)

heldout_2025_validation_metrics$log_mae <- mapply(
  function(pred_vec, actual_log_vec) {
    mean(abs(log(pred_vec) - actual_log_vec))
  },
  pred_list_2025,
  actual_log_list_2025
)

heldout_2025_validation_metrics$log_rmse <- mapply(
  function(pred_vec, actual_log_vec) {
    sqrt(mean((log(pred_vec) - actual_log_vec)^2))
  },
  pred_list_2025,
  actual_log_list_2025
)

cat("\nHeld-out 2025 validation metrics:\n")
print(heldout_2025_validation_metrics, row.names = FALSE, digits = 6)

# Saving held-out 2025 validation metrics
write.csv(
  heldout_2025_validation_metrics,
  file.path(output_dir, "13_heldout_2025_validation_metrics.csv"),
  row.names = FALSE
)

# Calculating quarterly 2025 validation errors across candidate models
quarters_2025 <- levels(test_2025$quarter)
quarterly_rows_list <- list()

for (q_lvl in quarters_2025) {
  q_idx <- which(test_2025$quarter == q_lvl)
  q_actual <- test_2025$price_eur[q_idx]
  q_label <- paste0("2025 ", q_lvl)

  q_table <- rbind(
    calculate_error_metrics(
      q_actual,
      test_2025_pred_mean[q_idx],
      "Baseline (Mean)",
      q_label
    ),
    calculate_error_metrics(
      q_actual,
      test_2025_pred_median[q_idx],
      "Baseline (Median)",
      q_label
    ),
    calculate_error_metrics(
      q_actual,
      test_2025_pred_model_a[q_idx],
      "Model A (Linear price)",
      q_label
    ),
    calculate_error_metrics(
      q_actual,
      test_2025_pred_model_b_exp[q_idx],
      "Model B (Log-linear, exp back-transform)",
      q_label
    ),
    calculate_error_metrics(
      q_actual,
      test_2025_pred_model_b_smear[q_idx],
      "Model B (Log-linear, Duan smearing)",
      q_label
    )
  )

  q_table$quarter <- q_lvl
  quarterly_rows_list[[q_lvl]] <- q_table
}

heldout_2025_quarterly_errors <- do.call(rbind, quarterly_rows_list)
rownames(heldout_2025_quarterly_errors) <- NULL

cat("\nHeld-out 2025 quarterly validation errors:\n")
print(heldout_2025_quarterly_errors, row.names = FALSE, digits = 6)

# Saving quarterly 2025 validation errors
write.csv(
  heldout_2025_quarterly_errors,
  file.path(output_dir, "14_heldout_2025_quarterly_errors.csv"),
  row.names = FALSE
)

# Saving Chart 8: Held-out 2025 actual vs predicted prices
fig8_path <- file.path(
  figures_dir,
  "08_validation_actual_vs_predicted_2025.png"
)

png(
  fig8_path,
  width = 2400,
  height = 1100,
  res = 180
)

par(
  mfrow = c(1, 2),
  mar = c(4.5, 5.5, 3.5, 1.5),
  las = 1
)

ord_2025 <- order(test_2025$sale_date)

plot(
  x = test_2025$sale_date[ord_2025],
  y = test_2025$price_eur[ord_2025] / 1000,
  pch = 16,
  cex = 0.55,
  col = adjustcolor("grey45", alpha.f = 0.45),
  ylim = c(0, 650),
  main = "Held-out 2025 sales and model trajectories (<= 650k EUR)",
  xlab = "Sale date in 2025",
  ylab = "Price (thousand EUR)"
)
lines(
  x = test_2025$sale_date[ord_2025],
  y = test_2025_pred_model_a[ord_2025] / 1000,
  col = chart_colour,
  lwd = 2.5
)
lines(
  x = test_2025$sale_date[ord_2025],
  y = test_2025_pred_model_b_smear[ord_2025] / 1000,
  col = secondary_colour,
  lwd = 2.5,
  lty = 2
)
lines(
  x = test_2025$sale_date[ord_2025],
  y = test_2025_pred_model_b_exp[ord_2025] / 1000,
  col = "black",
  lwd = 2.2,
  lty = 3
)
legend(
  "topleft",
  legend = c(
    "Actual 2025 sales",
    "Model A (Linear price)",
    "Model B (Duan smearing)",
    "Model B (exp back-transform)"
  ),
  col = c(
    adjustcolor("grey45", alpha.f = 0.7),
    chart_colour,
    secondary_colour,
    "black"
  ),
  pch = c(16, NA, NA, NA),
  lty = c(NA, 1, 2, 3),
  lwd = c(NA, 2.5, 2.5, 2.2),
  bty = "n",
  cex = 0.82
)

q_nums <- 1:4
q_actual_means <- tapply(test_2025$price_eur, test_2025$quarter, mean) / 1000
q_actual_medians <- tapply(test_2025$price_eur, test_2025$quarter, median) / 1000
q_pred_a_means <- tapply(test_2025_pred_model_a, test_2025$quarter, mean) / 1000
q_pred_b_smear_means <- tapply(
  test_2025_pred_model_b_smear,
  test_2025$quarter,
  mean
) / 1000
q_pred_b_exp_means <- tapply(
  test_2025_pred_model_b_exp,
  test_2025$quarter,
  mean
) / 1000

plot(
  x = q_nums,
  y = q_actual_means,
  type = "b",
  pch = 16,
  lwd = 2.2,
  col = "grey25",
  xaxt = "n",
  ylim = c(175, 285),
  main = "2025 quarterly actual vs predicted prices",
  xlab = "2025 quarter",
  ylab = "Price (thousand EUR)"
)
axis(1, at = q_nums, labels = quarters_2025)
lines(
  x = q_nums,
  y = q_actual_medians,
  type = "b",
  pch = 17,
  lwd = 2,
  lty = 2,
  col = "grey45"
)
lines(
  x = q_nums,
  y = q_pred_a_means,
  type = "b",
  pch = 15,
  lwd = 2.2,
  col = chart_colour
)
lines(
  x = q_nums,
  y = q_pred_b_smear_means,
  type = "b",
  pch = 18,
  lwd = 2.2,
  lty = 2,
  col = secondary_colour
)
lines(
  x = q_nums,
  y = q_pred_b_exp_means,
  type = "b",
  pch = 4,
  lwd = 2,
  lty = 3,
  col = "black"
)
legend(
  "topleft",
  legend = c(
    "Actual quarterly mean",
    "Actual quarterly median",
    "Model A mean prediction",
    "Model B (Duan smearing)",
    "Model B (exp back-transform)"
  ),
  col = c("grey25", "grey45", chart_colour, secondary_colour, "black"),
  pch = c(16, 17, 15, 18, 4),
  lty = c(1, 2, 1, 2, 3),
  lwd = 2,
  bty = "n",
  cex = 0.78
)

dev.off()

# Saving Chart 9: Held-out 2025 prediction errors over sale date
fig9_path <- file.path(
  figures_dir,
  "09_validation_errors_over_2025_dates.png"
)

err_2025_model_a <- test_2025_pred_model_a - test_2025$price_eur
err_2025_model_b_exp <- test_2025_pred_model_b_exp - test_2025$price_eur
err_2025_model_b_smear <- test_2025_pred_model_b_smear - test_2025$price_eur

png(
  fig9_path,
  width = 2400,
  height = 1800,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(4.5, 5.5, 3.5, 1.5),
  las = 1
)

plot(
  x = test_2025$sale_date,
  y = err_2025_model_a / 1000,
  pch = 16,
  cex = 0.55,
  col = adjustcolor(chart_colour, alpha.f = 0.5),
  main = "Model A: Held-out 2025 prediction error (predicted - actual) over date",
  xlab = "Sale date in 2025",
  ylab = "Prediction error (thousand EUR)"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)
lines(
  lowess(as.numeric(test_2025$sale_date), err_2025_model_a / 1000),
  col = secondary_colour,
  lwd = 2.2
)

plot(
  x = test_2025$sale_date,
  y = err_2025_model_b_exp / 1000,
  pch = 16,
  cex = 0.55,
  col = adjustcolor(secondary_colour, alpha.f = 0.5),
  main = "Model B (exp back-transform): Held-out 2025 prediction error over date",
  xlab = "Sale date in 2025",
  ylab = "Prediction error (thousand EUR)"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)
lines(
  lowess(as.numeric(test_2025$sale_date), err_2025_model_b_exp / 1000),
  col = chart_colour,
  lwd = 2.2
)

dev.off()

# Saving Chart 10: Held-out 2025 prediction errors by quarter
fig10_path <- file.path(
  figures_dir,
  "10_validation_errors_by_2025_quarter.png"
)

png(
  fig10_path,
  width = 2400,
  height = 1100,
  res = 180
)

par(
  mfrow = c(1, 2),
  mar = c(4.5, 5.5, 3.5, 1.5),
  las = 1
)

boxplot(
  I(err_2025_model_a / 1000) ~ test_2025$quarter,
  col = adjustcolor(chart_colour, alpha.f = 0.55),
  border = "grey20",
  pch = 16,
  cex = 0.5,
  main = "Model A: 2025 prediction error by quarter",
  xlab = "2025 quarter",
  ylab = "Prediction error (thousand EUR)"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)

boxplot(
  I(err_2025_model_b_exp / 1000) ~ test_2025$quarter,
  col = adjustcolor(secondary_colour, alpha.f = 0.55),
  border = "grey20",
  pch = 16,
  cex = 0.5,
  main = "Model B (exp): 2025 prediction error by quarter",
  xlab = "2025 quarter",
  ylab = "Prediction error (thousand EUR)"
)
abline(h = 0, col = "black", lwd = 2, lty = 2)

dev.off()

# Checking saved 2025 validation charts
validation_charts_2025 <- c(fig8_path, fig9_path, fig10_path)

stopifnot(
  all(file.exists(validation_charts_2025)),
  all(file.info(validation_charts_2025)$size > 0)
)

cat(
  "\nSection 7 completed.",
  "\nHeld-out 2025 records evaluated:", nrow(test_2025),
  "\nValidation charts saved:", length(validation_charts_2025),
  "\n"
)


# SECTION 8: Appraising systematic test cases from held-out 2025 records

# Checking held-out 2025 dataset, fitted models and parameters
stopifnot(
  exists("test_2025"),
  exists("model_a"),
  exists("model_b"),
  exists("smearing_factor_b"),
  exists("alpha"),
  exists("output_dir")
)

# Calculating 95% PI coverage across all 675 held-out 2025 records
pi_all_2025_a <- predict(
  model_a,
  newdata = test_2025,
  interval = "prediction",
  level = 1 - alpha
)

pi_all_2025_log_b <- predict(
  model_b,
  newdata = test_2025,
  interval = "prediction",
  level = 1 - alpha
)
pi_all_2025_b_exp <- exp(pi_all_2025_log_b)

pi_cov_2025_a <- mean(
  test_2025$price_eur >= pi_all_2025_a[, "lwr"] &
    test_2025$price_eur <= pi_all_2025_a[, "upr"]
)

pi_cov_2025_b <- mean(
  test_2025$price_eur >= pi_all_2025_b_exp[, "lwr"] &
    test_2025$price_eur <= pi_all_2025_b_exp[, "upr"]
)

cat("\nFull 2025 held-out 95% prediction interval coverage (n = 675):\n")
cat("Model A 95% PI coverage:", round(100 * pi_cov_2025_a, 2), "%\n")
cat("Model B (exp) 95% PI coverage:", round(100 * pi_cov_2025_b, 2), "%\n")

# Selecting 5 percentile records per quarter across held-out 2025 sales
percentile_probs <- c(0.01, 0.25, 0.50, 0.75, 0.99)
percentile_labels <- c(
  "P01 (Lower tail)",
  "P25 (Lower quartile)",
  "P50 (Median)",
  "P75 (Upper quartile)",
  "P99 (Upper tail)"
)

case_rows_list <- list()

for (q_lvl in levels(test_2025$quarter)) {
  q_sub <- subset(test_2025, quarter == q_lvl)
  q_ord <- order(q_sub$price_eur, q_sub$sale_date)
  q_sub_sorted <- q_sub[q_ord, ]

  n_q <- nrow(q_sub_sorted)
  pick_pos <- round(1 + percentile_probs * (n_q - 1))

  stopifnot(
    length(unique(pick_pos)) == length(percentile_probs),
    all(pick_pos >= 1),
    all(pick_pos <= n_q)
  )

  picked_df <- q_sub_sorted[pick_pos, ]
  picked_df$percentile_band <- percentile_labels
  picked_df$quarter_records <- n_q
  picked_df$quarter_rank <- pick_pos

  case_rows_list[[q_lvl]] <- picked_df
}

selected_2025_cases <- do.call(rbind, case_rows_list)
rownames(selected_2025_cases) <- NULL

stopifnot(
  nrow(selected_2025_cases) == 20
)

# Computing Model A predictions, 95% CIs and 95% PIs on selected 2025 cases
ci_cases_a <- predict(
  model_a,
  newdata = selected_2025_cases,
  interval = "confidence",
  level = 1 - alpha
)

pi_cases_a <- predict(
  model_a,
  newdata = selected_2025_cases,
  interval = "prediction",
  level = 1 - alpha
)

# Computing Model B predictions, 95% CIs and 95% PIs on selected 2025 cases
ci_cases_log_b <- predict(
  model_b,
  newdata = selected_2025_cases,
  interval = "confidence",
  level = 1 - alpha
)

pi_cases_log_b <- predict(
  model_b,
  newdata = selected_2025_cases,
  interval = "prediction",
  level = 1 - alpha
)

ci_cases_b_exp <- exp(ci_cases_log_b)
pi_cases_b_exp <- exp(pi_cases_log_b)

pred_cases_a <- unname(ci_cases_a[, "fit"])
pred_cases_b_exp <- unname(ci_cases_b_exp[, "fit"])
pred_cases_b_smear <- pred_cases_b_exp * smearing_factor_b

actual_cases_eur <- selected_2025_cases$price_eur

# Building 2025 systematic test-case comparison table
forecast_2025_observed_test_cases <- data.frame(
  case_id = seq_len(nrow(selected_2025_cases)),
  quarter = as.character(selected_2025_cases$quarter),
  percentile_band = selected_2025_cases$percentile_band,
  quarter_rank = selected_2025_cases$quarter_rank,
  quarter_records = selected_2025_cases$quarter_records,
  sale_date = format(selected_2025_cases$sale_date, "%Y-%m-%d"),
  time_years = selected_2025_cases$time_years,
  actual_price_eur = actual_cases_eur,
  model_a_pred_eur = pred_cases_a,
  model_a_ci_95_lower = unname(ci_cases_a[, "lwr"]),
  model_a_ci_95_upper = unname(ci_cases_a[, "upr"]),
  model_a_pi_95_lower = unname(pi_cases_a[, "lwr"]),
  model_a_pi_95_upper = unname(pi_cases_a[, "upr"]),
  model_a_error_eur = pred_cases_a - actual_cases_eur,
  model_a_abs_pct_error = 100 * abs(pred_cases_a - actual_cases_eur) / actual_cases_eur,
  model_a_in_pi_95 = (
    actual_cases_eur >= pi_cases_a[, "lwr"] &
      actual_cases_eur <= pi_cases_a[, "upr"]
  ),
  model_b_exp_pred_eur = pred_cases_b_exp,
  model_b_exp_ci_95_lower = unname(ci_cases_b_exp[, "lwr"]),
  model_b_exp_ci_95_upper = unname(ci_cases_b_exp[, "upr"]),
  model_b_exp_pi_95_lower = unname(pi_cases_b_exp[, "lwr"]),
  model_b_exp_pi_95_upper = unname(pi_cases_b_exp[, "upr"]),
  model_b_exp_error_eur = pred_cases_b_exp - actual_cases_eur,
  model_b_exp_abs_pct_error = (
    100 * abs(pred_cases_b_exp - actual_cases_eur) / actual_cases_eur
  ),
  model_b_in_pi_95 = (
    actual_cases_eur >= pi_cases_b_exp[, "lwr"] &
      actual_cases_eur <= pi_cases_b_exp[, "upr"]
  ),
  model_b_smear_pred_eur = pred_cases_b_smear,
  model_b_smear_error_eur = pred_cases_b_smear - actual_cases_eur,
  model_b_smear_abs_pct_error = (
    100 * abs(pred_cases_b_smear - actual_cases_eur) / actual_cases_eur
  ),
  row.names = NULL
)

cat("\nObserved 2025 test cases appraisal (20 systematic percentile cases):\n")
print(forecast_2025_observed_test_cases, row.names = FALSE, digits = 6)

# Saving 2025 observed test-cases table
write.csv(
  forecast_2025_observed_test_cases,
  file.path(output_dir, "15_forecast_2025_observed_test_cases.csv"),
  row.names = FALSE
)

cat(
  "\nSection 8 completed.",
  "\nSystematic 2025 test cases appraised:", nrow(forecast_2025_observed_test_cases),
  "\nModel A cases within 95% PI:", sum(forecast_2025_observed_test_cases$model_a_in_pi_95), "of 20",
  "\nModel B cases within 95% PI:", sum(forecast_2025_observed_test_cases$model_b_in_pi_95), "of 20",
  "\n"
)
