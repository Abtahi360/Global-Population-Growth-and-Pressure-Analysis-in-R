
# Auto-install any missing packages
required_pkgs <- c(
  "tidyverse",   
  "caret",      
  "randomForest", 
  "rpart",       
  "e1071",        
  "reshape2",    
  "gridExtra",   
  "scales",      
  "nnet"         
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# XGBoost – optional, safe fallback
XGBOOST_AVAILABLE <- requireNamespace("xgboost", quietly = TRUE)
if (!XGBOOST_AVAILABLE) {
  tryCatch(
    install.packages("xgboost", repos = "https://cloud.r-project.org", quiet = TRUE),
    error = function(e) NULL
  )
  XGBOOST_AVAILABLE <- requireNamespace("xgboost", quietly = TRUE)
}
if (XGBOOST_AVAILABLE) suppressPackageStartupMessages(library(xgboost))

SEED <- 42
set.seed(SEED)

# Output directory for all figures
OUT <- "project_outputs_R"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# Helper: save the last ggplot
savefig <- function(name, width = 12, height = 7) {
  ggsave(
    filename = file.path(OUT, name),
    width  = width,
    height = height,
    dpi    = 150,
    bg     = "white"
  )
}

cat(strrep("=", 62), "\n")
cat("  FINAL-TERM DATA SCIENCE PROJECT  - INITIALISED\n")
cat(strrep("=", 62), "\n")


cat("

\n")


# Locate the CSV file
possible_paths <- c(
  "worldometers_population.csv",
  "/mnt/user-data/uploads/worldometers_population.csv"
)
DATA_PATH <- NULL
for (p in possible_paths) {
  if (file.exists(p)) { DATA_PATH <- p; break }
}
if (is.null(DATA_PATH)) {
  stop("ERROR: Dataset not found. Place worldometers_population.csv in the working directory.")
}

# Read CSV – treat "NA", "N/A", "", " " as missing
raw <- read.csv(
  DATA_PATH,
  na.strings    = c("NA", "N/A", "", " "),
  stringsAsFactors = FALSE,
  check.names   = FALSE
)

cat(sprintf("Dataset loaded from : %s\n",   DATA_PATH))
cat(sprintf("Shape (rows x cols) : %d x %d\n\n", nrow(raw), ncol(raw)))

cat("-- Column names & types --\n")
print(sapply(raw, class))

cat("\n-- First 5 rows --\n")
print(head(raw, 5))

cat("\n-- Missing-value count per column --\n")
mv_raw <- colSums(is.na(raw))
print(mv_raw)

cat(sprintf("\nDuplicate rows      : %d\n",
            sum(duplicated(raw))))


df <- raw

# 3-a  Drop non-informative / metadata columns
df <- df[ , !(names(df) %in% c("Scraped_At", "Source_URL"))]
cat("[Step 1] Dropped metadata columns: Scraped_At, Source_URL\n")

# 3-b  Force-convert all numeric columns (handles "1.00E+06" style)
numeric_cols <- c(
  "Rank", "Population_2026", "Yearly_Change", "Net_Change",
  "Density_per_km2", "Land_Area_km2", "Migrants",
  "Fert_Rate", "Median_Age", "Urban_Pop_Pct", "World_Share"
)
for (col in numeric_cols) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}
cat("[Step 2] Converted all numeric columns (including scientific notation).\n")

# 3-c  Remove duplicate countries
before <- nrow(df)
df <- df[!duplicated(df$Country), ]
cat(sprintf("[Step 3] Duplicates removed: %d rows dropped.\n", before - nrow(df)))

cat("\n-- Summary statistics --\n")
print(summary(df[, numeric_cols]))

cat("\n-- Missing values after conversion --\n")
mv_post <- colSums(is.na(df))
print(mv_post[mv_post > 0])


median_impute_cols <- c(
  "Yearly_Change", "Net_Change", "Density_per_km2",
  "Fert_Rate", "Median_Age", "Urban_Pop_Pct", "World_Share"
)
for (col in median_impute_cols) {
  med      <- median(df[[col]], na.rm = TRUE)
  n_filled <- sum(is.na(df[[col]]))
  df[[col]][is.na(df[[col]])] <- med
  if (n_filled > 0) {
    cat(sprintf("[Step 4] %s: %d NAs -> median (%.3f)\n", col, n_filled, med))
  }
}
df$Migrants[is.na(df$Migrants)] <- 0
cat("[Step 4] Migrants: remaining NAs -> 0 (no recorded migration = zero net flow)\n")

winsorise <- function(x, factor = 3) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  pmax(pmin(x, q3 + factor * iqr), q1 - factor * iqr)
}

cap_cols <- c("Density_per_km2", "Land_Area_km2", "Migrants",
              "Net_Change", "Population_2026")
for (col in cap_cols) {
  before_range <- range(df[[col]], na.rm = TRUE)
  df[[col]]    <- winsorise(df[[col]])
  after_range  <- range(df[[col]], na.rm = TRUE)
  cat(sprintf("[Step 5] Winsorise %s: [%.0f, %.0f] -> [%.0f, %.0f]\n",
              col,
              before_range[1], before_range[2],
              after_range[1],  after_range[2]))
}

# 3-h  Feature engineering
df$Dependency_Pressure_Index <- df$Fert_Rate * df$Density_per_km2
df$Urban_Stress_Index        <- df$Urban_Pop_Pct * df$Yearly_Change
df$Pop_Density_Log           <- log1p(df$Density_per_km2)
df$Population_Log            <- log1p(df$Population_2026)

cat("\n[Step 6] Feature engineering complete:\n")
cat("          Dependency_Pressure_Index = Fert_Rate * Density_per_km2\n")
cat("          Urban_Stress_Index        = Urban_Pop_Pct * Yearly_Change\n")
cat("          Pop_Density_Log           = log1p(Density_per_km2)\n")
cat("          Population_Log            = log1p(Population_2026)\n")
# 3-i  Sanitise Inf / NaN introduced by feature engineering
#       (e.g. Urban_Stress_Index = Urban_Pop_Pct * Yearly_Change can produce
#        extreme values that scale() turns into NaN/Inf)
sanitise_col <- function(x) {
  x[!is.finite(x)] <- NA
  x[is.na(x)]      <- median(x, na.rm = TRUE)
  x
}
engineered_cols <- c("Dependency_Pressure_Index", "Urban_Stress_Index",
                     "Pop_Density_Log", "Population_Log")
for (col in engineered_cols) df[[col]] <- sanitise_col(df[[col]])
cat("[Step 7] Inf/NaN sanitised in engineered columns.\n")

cat(sprintf("\nFinal clean dataset shape: %d x %d\n", nrow(df), ncol(df)))


theme_project <- theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "#4C72B0"),
    strip.text       = element_text(colour = "white", face = "bold")
  )

# ── 4-1  Distribution plots
dist_features <- c("Yearly_Change", "Fert_Rate", "Median_Age",
                   "Urban_Pop_Pct", "Density_per_km2", "Migrants")

dist_data <- df[ , dist_features] |>
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value")

ggplot(dist_data, aes(x = Value)) +
  geom_histogram(bins = 25, fill = "#4C72B0", colour = "white", alpha = 0.85) +
  facet_wrap(~Feature, scales = "free", ncol = 3) +
  labs(title = "Figure 1 - Feature Distributions",
       x = "Value", y = "Frequency") +
  theme_project
savefig("fig1_distributions.png", width = 14, height = 8)
cat("[EDA] Figure 1 saved - distribution plots\n")

# ── 4-2  Boxplots
ggplot(dist_data, aes(x = Feature, y = Value)) +
  geom_boxplot(fill = "#4C72B0", alpha = 0.7, outlier.colour = "#C44E52") +
  facet_wrap(~Feature, scales = "free", ncol = 3) +
  labs(title = "Figure 2 - Boxplots (Outlier Detection)", y = "Value") +
  theme_project +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
savefig("fig2_boxplots.png", width = 14, height = 8)
cat("[EDA] Figure 2 saved - boxplots\n")

# ── 4-3  Correlation heatmap
corr_features <- c(
  "Yearly_Change", "Fert_Rate", "Median_Age", "Urban_Pop_Pct",
  "Density_per_km2", "Migrants", "Net_Change", "Population_2026",
  "Land_Area_km2", "Dependency_Pressure_Index", "Urban_Stress_Index"
)
corr_matrix <- cor(df[, corr_features], use = "complete.obs")
corr_melt   <- melt(corr_matrix)
# Mask upper triangle
corr_melt$value[upper.tri(corr_matrix, diag = FALSE)[
  cbind(match(corr_melt$Var1, rownames(corr_matrix)),
        match(corr_melt$Var2, colnames(corr_matrix)))
]] <- NA

ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(!is.na(value), sprintf("%.2f", value), "")),
            size = 2.8, colour = "black") +
  scale_fill_gradient2(
    low = "#4C72B0", mid = "white", high = "#C44E52",
    midpoint = 0, limits = c(-1, 1), na.value = "grey95",
    name = "r"
  ) +
  labs(title = "Figure 3 - Correlation Heatmap",
       x = NULL, y = NULL) +
  theme_project +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
savefig("fig3_correlation_heatmap.png", width = 12, height = 10)
cat("[EDA] Figure 3 saved - correlation heatmap\n")

# ── 4-4  Scatter plots vs Yearly_Change
scatter_features <- c("Fert_Rate", "Median_Age", "Urban_Pop_Pct", "Density_per_km2")
sc_data <- df[ , c("Yearly_Change", scatter_features)] |>
  pivot_longer(cols = -Yearly_Change, names_to = "Feature", values_to = "Value")

ggplot(sc_data, aes(x = Value, y = Yearly_Change)) +
  geom_point(alpha = 0.5, colour = "#DD8452", size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "#2E4A7A", linewidth = 1.2) +
  facet_wrap(~Feature, scales = "free_x", ncol = 4) +
  labs(title = "Figure 4 - Scatter: Features vs Yearly_Change",
       x = "Feature Value", y = "Yearly_Change (%)") +
  theme_project
savefig("fig4_scatterplots.png", width = 16, height = 5)
cat("[EDA] Figure 4 saved - scatter plots\n")

# ── 4-5  Top 20 fastest-growing countries
top20 <- df[order(-df$Yearly_Change), ][1:20, c("Country", "Yearly_Change")]
top20$Country <- factor(top20$Country, levels = rev(top20$Country))

ggplot(top20, aes(x = Yearly_Change, y = Country, fill = Yearly_Change)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FDAE61", high = "#D7191C", guide = "none") +
  labs(title = "Figure 5 - Top 20 Fastest-Growing Countries",
       x = "Annual Growth Rate (%)", y = NULL) +
  theme_project
savefig("fig5_top20_growth.png", width = 10, height = 7)
cat("[EDA] Figure 5 saved - top 20 countries\n")

cat(sprintf("\n[EDA] Key findings:\n"))
cat(sprintf("  * Yearly_Change range : %.2f%% - %.2f%%\n",
            min(df$Yearly_Change), max(df$Yearly_Change)))
cat(sprintf("  * Fert_Rate vs Yearly_Change correlation : %.3f\n",
            cor(df$Fert_Rate, df$Yearly_Change, use = "complete.obs")))
cat(sprintf("  * Median_Age vs Yearly_Change correlation: %.3f\n",
            cor(df$Median_Age, df$Yearly_Change, use = "complete.obs")))

# Min-max normalisation helper
min_max_norm <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}


df$CPPS <- (
    0.25 * min_max_norm(df$Yearly_Change)
  + 0.25 * min_max_norm(df$Fert_Rate)
  + 0.20 * min_max_norm(df$Density_per_km2)
  + 0.15 * min_max_norm(df$Urban_Pop_Pct)
  + 0.10 * (1 - min_max_norm(df$Median_Age))
  + 0.05 * min_max_norm(pmax(df$Migrants, 0))
)

# Tertile-based labelling
t33 <- quantile(df$CPPS, 0.333)
t66 <- quantile(df$CPPS, 0.666)
df$Population_Pressure <- cut(
  df$CPPS,
  breaks = c(-Inf, t33, t66, Inf),
  labels = c("Low", "Moderate", "High"),
  right  = TRUE
)

cat("Population Pressure label distribution:\n")
print(table(df$Population_Pressure))
cat(sprintf("\nCPPS tertile thresholds: Low <= %.3f < Moderate <= %.3f < High\n", t33, t66))

# ── Class distribution plot
label_counts <- as.data.frame(table(df$Population_Pressure))
names(label_counts) <- c("Category", "Count")
label_counts$Category <- factor(label_counts$Category, levels = c("Low","Moderate","High"))

ggplot(label_counts, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", colour = "white", linewidth = 1) +
  geom_text(aes(label = Count), vjust = -0.4, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Low" = "#2ecc71", "Moderate" = "#f39c12", "High" = "#e74c3c")) +
  labs(title = "Figure 6 - Class Distribution: Population Pressure",
       x = "Population Pressure Category", y = "Number of Countries") +
  theme_project + theme(legend.position = "none")
savefig("fig6_class_distribution.png", width = 7, height = 5)
cat("[EDA] Figure 6 saved - class distribution\n")


REG_FEATURES <- c(
  "Fert_Rate", "Median_Age", "Urban_Pop_Pct", "Density_per_km2",
  "Migrants", "Land_Area_km2", "Net_Change", "Population_Log",
  "Dependency_Pressure_Index", "Urban_Stress_Index"
)
CLF_FEATURES <- c(
  "Fert_Rate", "Median_Age", "Urban_Pop_Pct", "Density_per_km2",
  "Migrants", "Yearly_Change", "Land_Area_km2", "Population_Log",
  "Dependency_Pressure_Index", "Urban_Stress_Index"
)

# Target vectors
y_reg <- df$Yearly_Change
y_clf <- df$Population_Pressure   # factor

# Feature matrices (any remaining NA gets median-imputed row-wise)
impute_median_mat <- function(mat) {
  for (j in seq_len(ncol(mat))) {
    na_idx <- is.na(mat[, j])
    if (any(na_idx)) mat[na_idx, j] <- median(mat[!na_idx, j])
  }
  mat
}

X_reg_raw <- as.matrix(df[, REG_FEATURES])
X_clf_raw <- as.matrix(df[, CLF_FEATURES])
X_reg_raw <- impute_median_mat(X_reg_raw)
X_clf_raw <- impute_median_mat(X_clf_raw)

X_reg_sc <- scale(X_reg_raw)
X_clf_sc <- scale(X_clf_raw)

clean_scaled_mat <- function(mat) {
  for (j in seq_len(ncol(mat))) {
    bad <- !is.finite(mat[, j])
    if (any(bad)) mat[bad, j] <- 0   # 0 == column mean in z-score space
  }
  mat
}
X_reg_sc <- clean_scaled_mat(X_reg_sc)
X_clf_sc <- clean_scaled_mat(X_clf_sc)
cat("[Scaling] NaN/Inf replaced with 0 (column mean) in scaled matrices.\n")

# Train / test split (80/20)
# Regression: random split
set.seed(SEED)
n <- nrow(df)
reg_train_idx <- sample(n, floor(0.80 * n))
reg_test_idx  <- setdiff(seq_len(n), reg_train_idx)

# Classification: stratified split via caret
set.seed(SEED)
clf_train_idx <- createDataPartition(y_clf, p = 0.80, list = FALSE)[, 1]
clf_test_idx  <- setdiff(seq_len(n), clf_train_idx)

X_rtr <- X_reg_sc[reg_train_idx, ];  X_rte <- X_reg_sc[reg_test_idx, ]
y_rtr <- y_reg[reg_train_idx];       y_rte <- y_reg[reg_test_idx]

X_ctr <- X_clf_sc[clf_train_idx, ];  X_cte <- X_clf_sc[clf_test_idx, ]
y_ctr <- y_clf[clf_train_idx];       y_cte <- y_clf[clf_test_idx]

cat(sprintf("Regression     - Train: %d | Test: %d\n", length(y_rtr), length(y_rte)))
cat(sprintf("Classification - Train: %d | Test: %d\n", length(y_ctr), length(y_cte)))

# Convert to data frames for caret::train()
train_df_reg <- data.frame(X_rtr, Yearly_Change = y_rtr)
test_df_reg  <- data.frame(X_rte)

train_df_clf <- data.frame(X_ctr, Population_Pressure = y_ctr)
test_df_clf  <- data.frame(X_cte)

# Full scaled data frames for cross-validation
full_df_reg <- data.frame(X_reg_sc, Yearly_Change = y_reg)
full_df_clf <- data.frame(X_clf_sc, Population_Pressure = y_clf)

tc_reg <- trainControl(
  method        = "cv",
  number        = 5,
  verboseIter   = FALSE,
  allowParallel = FALSE
)
tc_clf <- trainControl(
  method        = "cv",
  number        = 5,
  verboseIter   = FALSE,
  allowParallel = FALSE,
  classProbs    = TRUE
)


 on test set
reg_metrics <- function(model, test_x, test_y, model_name, cv_r2) {
  pred  <- predict(model, newdata = test_x)
  mae   <- mean(abs(test_y - pred))
  mse   <- mean((test_y - pred)^2)
  rmse  <- sqrt(mse)
  ss_res <- sum((test_y - pred)^2)
  ss_tot <- sum((test_y - mean(test_y))^2)
  r2    <- 1 - ss_res / ss_tot
  cat(sprintf("\n  %s\n    MAE=%.4f  RMSE=%.4f  R2=%.4f  CV-R2=%.4f\n",
              model_name, mae, rmse, r2, cv_r2))
  list(Model = model_name, MAE = mae, MSE = mse, RMSE = rmse,
       R2 = r2, CV_R2 = cv_r2, predictions = pred)
}

set.seed(SEED)
# 7-a  Linear Regression
lm_model <- train(Yearly_Change ~ ., data = train_df_reg, method = "lm",
                  trControl = tc_reg, na.action = na.omit)
lm_cv_r2 <- { v <- lm_model$results$Rsquared; if(all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) }
lm_res   <- reg_metrics(lm_model, test_df_reg, y_rte, "Linear Regression", lm_cv_r2)

# 7-b  Decision Tree Regressor
set.seed(SEED)
dt_reg_model <- train(Yearly_Change ~ ., data = train_df_reg, method = "rpart",
                      trControl = tc_reg,
                      tuneGrid  = data.frame(cp = 0.01), na.action = na.omit)
dt_reg_cv_r2 <- { v <- dt_reg_model$results$Rsquared; if(all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) }
dt_reg_res   <- reg_metrics(dt_reg_model, test_df_reg, y_rte,
                             "Decision Tree Regressor", dt_reg_cv_r2)

# 7-c  Random Forest Regressor
set.seed(SEED)
rf_reg_model <- train(Yearly_Change ~ ., data = train_df_reg, method = "rf",
                      trControl  = tc_reg,
                      ntree      = 200,
                      tuneGrid   = data.frame(mtry = floor(sqrt(length(REG_FEATURES)))),
                      importance = TRUE, na.action = na.omit)
rf_reg_cv_r2 <- { v <- rf_reg_model$results$Rsquared; if(all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) }
rf_reg_res   <- reg_metrics(rf_reg_model, test_df_reg, y_rte,
                             "Random Forest Regressor", rf_reg_cv_r2)

xgb_reg_res   <- NULL
xgb_reg_model <- NULL
if (XGBOOST_AVAILABLE) {
  tryCatch({
    set.seed(SEED)
    xgb_reg_grid <- expand.grid(
      nrounds          = 150L,
      max_depth        = 4L,
      eta              = 0.05,
      gamma            = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1L,
      subsample        = 0.8
    )
    xgb_reg_model <<- train(
      Yearly_Change ~ .,
      data      = train_df_reg,
      method    = "xgbTree",
      trControl = tc_reg,
      tuneGrid  = xgb_reg_grid,
      verbosity = 0,
      nthread   = 1L,
      na.action = na.omit
    )
    # Safe CV R2 extraction: guard against all-NA fold results
    r2_vals        <- xgb_reg_model$results$Rsquared
    xgb_reg_cv_r2  <- if (all(is.na(r2_vals))) NA_real_ else max(r2_vals, na.rm = TRUE)
    xgb_reg_res   <<- reg_metrics(xgb_reg_model, test_df_reg, y_rte,
                                  "XGBoost Regressor", xgb_reg_cv_r2)
  }, error = function(e) {
    cat(sprintf("[WARNING] XGBoost Regressor skipped: %s\n         Continuing without it.\n",
                conditionMessage(e)))
  })
}

# Build comparison table
reg_results_list <- list(lm_res, dt_reg_res, rf_reg_res)
if (!is.null(xgb_reg_res)) reg_results_list <- c(reg_results_list, list(xgb_reg_res))

reg_df_table <- do.call(rbind, lapply(reg_results_list, function(x) {
  data.frame(Model  = x$Model,
             MAE    = round(x$MAE,  4),
             MSE    = round(x$MSE,  4),
             RMSE   = round(x$RMSE, 4),
             R2     = round(x$R2,   4),
             CV_R2  = round(x$CV_R2, 4),
             stringsAsFactors = FALSE)
}))
rownames(reg_df_table) <- reg_df_table$Model

cat("\n-- Regression Model Comparison Table --\n")
print(reg_df_table[ , -1])

# Best model by R²
best_reg_idx  <- which.max(reg_df_table$R2)
best_reg_name <- reg_df_table$Model[best_reg_idx]
best_reg_r2   <- reg_df_table$R2[best_reg_idx]
cat(sprintf("\n* Best Regression Model: %s  (R2=%.4f)\n", best_reg_name, best_reg_r2))

# Select best model object
best_reg_model <- list(
  "Linear Regression"        = lm_model,
  "Decision Tree Regressor"  = dt_reg_model,
  "Random Forest Regressor"  = rf_reg_model
)
# Only add XGBoost if it successfully trained (not NULL)
if (!is.null(xgb_reg_model)) best_reg_model[["XGBoost Regressor"]] <- xgb_reg_model
best_reg_model <- best_reg_model[[best_reg_name]]

# ── Figure 7: Regression metric bar chart
reg_plot_data <- reg_df_table |>
  pivot_longer(cols = c(MAE, RMSE, R2, CV_R2),
               names_to = "Metric", values_to = "Value")

p7a <- ggplot(reg_plot_data[reg_plot_data$Metric %in% c("MAE","RMSE"), ],
              aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", colour = "white") +
  scale_fill_manual(values = c("MAE" = "#4C72B0", "RMSE" = "#DD8452")) +
  labs(title = "MAE & RMSE by Model", x = NULL, y = "Error") +
  theme_project + theme(axis.text.x = element_text(angle = 20, hjust = 1))

p7b <- ggplot(reg_plot_data[reg_plot_data$Metric %in% c("R2","CV_R2"), ],
              aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", colour = "white") +
  scale_fill_manual(values = c("R2" = "#55A868", "CV_R2" = "#C44E52")) +
  labs(title = "R2 & CV-R2 by Model", x = NULL, y = "Score") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_project + theme(axis.text.x = element_text(angle = 20, hjust = 1))

grid.arrange(p7a, p7b, ncol = 2,
             top = "Figure 7 - Regression Model Comparison")
savefig("fig7_regression_comparison.png", width = 14, height = 6)
cat("[Plot] Figure 7 saved - regression comparison\n")

# ── Figure 8: Predicted vs Actual (best model)
best_preds_r <- predict(best_reg_model, newdata = test_df_reg)
pred_actual  <- data.frame(Actual = y_rte, Predicted = best_preds_r)
lims         <- range(c(y_rte, best_preds_r)) + c(-0.1, 0.1)

ggplot(pred_actual, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, colour = "#4C72B0", size = 2.5) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed", linewidth = 1.2) +
  coord_cartesian(xlim = lims, ylim = lims) +
  labs(title   = sprintf("Figure 8 - %s: Actual vs Predicted", best_reg_name),
       x = "Actual Yearly_Change",
       y = "Predicted Yearly_Change") +
  theme_project
savefig("fig8_actual_vs_predicted.png", width = 7, height = 6)
cat("[Plot] Figure 8 saved - actual vs predicted\n")

# ── Figure 9: Feature importance (Random Forest)
fi_reg <- varImp(rf_reg_model)$importance
fi_reg_df <- data.frame(
  Feature    = rownames(fi_reg),
  Importance = fi_reg$Overall,
  stringsAsFactors = FALSE
)
fi_reg_df <- fi_reg_df[order(fi_reg_df$Importance), ]
fi_reg_df$Feature <- factor(fi_reg_df$Feature, levels = fi_reg_df$Feature)

ggplot(fi_reg_df, aes(x = Importance, y = Feature)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  labs(title = sprintf("Figure 9 - Feature Importance (%s)", best_reg_name),
       x = "Importance Score", y = NULL) +
  theme_project
savefig("fig9_reg_feature_importance.png", width = 9, height = 6)
cat("[Plot] Figure 9 saved - regression feature importance\n")

cat("\nTop-3 most important features for population growth prediction:\n")
print(tail(fi_reg_df[order(fi_reg_df$Importance), ], 3))


weighted_metric <- function(cm_obj, metric_col) {
  by_class   <- cm_obj$byClass           # matrix: rows = classes
  freq_table <- table(y_cte)
  class_labels <- levels(y_cte)
  # caret names rows as "Class: Low", etc.
  row_names  <- paste0("Class: ", class_labels)
  weights    <- as.numeric(freq_table[class_labels]) / sum(freq_table)
  vals       <- by_class[row_names, metric_col]
  vals[is.na(vals)] <- 0
  sum(vals * weights)
}

clf_eval <- function(model, test_x_df, test_y, model_name, cv_acc) {
  pred <- predict(model, newdata = test_x_df)
  cm   <- confusionMatrix(pred, test_y)
  acc  <- cm$overall["Accuracy"]
  prec <- weighted_metric(cm, "Precision")
  rec  <- weighted_metric(cm, "Recall")
  f1   <- weighted_metric(cm, "F1")
  cat(sprintf("\n  %s\n    Acc=%.4f  Prec=%.4f  Rec=%.4f  F1=%.4f  CV-Acc=%.4f\n",
              model_name, acc, prec, rec, f1, cv_acc))
  list(Model = model_name, Accuracy = acc, Precision = prec,
       Recall = rec, F1_Score = f1, CV_Acc = cv_acc,
       cm_obj = cm, predictions = pred)
}

set.seed(SEED)
lr_clf_model <- train(Population_Pressure ~ ., data = train_df_clf,
                      method    = "multinom",
                      trControl = tc_clf,
                      trace     = FALSE, na.action = na.omit)
lr_cv_acc <- { v <- lr_clf_model$results$Accuracy; if(all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) }
lr_clf_res <- clf_eval(lr_clf_model, test_df_clf, y_cte,
                       "Logistic Regression", lr_cv_acc)

# 8-b  Decision Tree Classifier
set.seed(SEED)
dt_clf_model <- train(Population_Pressure ~ ., data = train_df_clf,
                      method    = "rpart",
                      trControl = tc_clf,
                      tuneGrid  = data.frame(cp = 0.01), na.action = na.omit)
dt_cv_acc <- { v <- dt_clf_model$results$Accuracy; if(all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) }
dt_clf_res <- clf_eval(dt_clf_model, test_df_clf, y_cte,
                       "Decision Tree Classifier", dt_cv_acc)

# 8-c  Random Forest Classifier
set.seed(SEED)
rf_clf_model <- train(Population_Pressure ~ ., data = train_df_clf,
                      method    = "rf",
                      trControl = tc_clf,
                      ntree     = 200,
                      tuneGrid  = data.frame(mtry = floor(sqrt(length(CLF_FEATURES)))),
                      importance = TRUE, na.action = na.omit)
rf_clf_cv_acc <- { v <- rf_clf_model$results$Accuracy; if(all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) }
rf_clf_res <- clf_eval(rf_clf_model, test_df_clf, y_cte,
                       "Random Forest Classifier", rf_clf_cv_acc)

# 8-d  SVM (RBF kernel)
set.seed(SEED)
svm_clf_model <- train(Population_Pressure ~ ., data = train_df_clf,
                       method    = "svmRadial",
                       trControl = tc_clf,
                       tuneGrid  = expand.grid(C = 1, sigma = 0.1),
                       na.action = na.omit)
svm_cv_acc <- { v <- svm_clf_model$results$Accuracy; if(all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) }
svm_clf_res <- clf_eval(svm_clf_model, test_df_clf, y_cte,
                        "SVM (RBF)", svm_cv_acc)

# 8-e  XGBoost Classifier (if available) - wrapped in tryCatch for safety
xgb_clf_res   <- NULL
xgb_clf_model <- NULL
if (XGBOOST_AVAILABLE) {
  tryCatch({
    set.seed(SEED)
    xgb_clf_grid <- expand.grid(
      nrounds          = 150L,
      max_depth        = 4L,
      eta              = 0.05,
      gamma            = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1L,
      subsample        = 0.8
    )
    xgb_clf_model <<- train(
      Population_Pressure ~ .,
      data      = train_df_clf,
      method    = "xgbTree",
      trControl = tc_clf,
      tuneGrid  = xgb_clf_grid,
      verbosity = 0,
      nthread   = 1L,
      na.action = na.omit
    )
    # Safe CV Accuracy extraction
    acc_vals       <- xgb_clf_model$results$Accuracy
    xgb_clf_cv_acc <- if (all(is.na(acc_vals))) NA_real_ else max(acc_vals, na.rm = TRUE)
    xgb_clf_res   <<- clf_eval(xgb_clf_model, test_df_clf, y_cte,
                               "XGBoost Classifier", xgb_clf_cv_acc)
  }, error = function(e) {
    cat(sprintf("[WARNING] XGBoost Classifier skipped: %s\n         Continuing without it.\n",
                conditionMessage(e)))
  })
}

# Build comparison table
clf_list <- list(lr_clf_res, dt_clf_res, rf_clf_res, svm_clf_res)
if (!is.null(xgb_clf_res)) clf_list <- c(clf_list, list(xgb_clf_res))

clf_df_table <- do.call(rbind, lapply(clf_list, function(x) {
  data.frame(Model     = x$Model,
             Accuracy  = round(x$Accuracy,  4),
             Precision = round(x$Precision, 4),
             Recall    = round(x$Recall,    4),
             F1_Score  = round(x$F1_Score,  4),
             CV_Acc    = round(x$CV_Acc,    4),
             stringsAsFactors = FALSE)
}))
rownames(clf_df_table) <- clf_df_table$Model

cat("\n-- Classification Model Comparison Table --\n")
print(clf_df_table[ , -1])

# Best model by weighted F1
best_clf_idx  <- which.max(clf_df_table$F1_Score)
best_clf_name <- clf_df_table$Model[best_clf_idx]
best_clf_f1   <- clf_df_table$F1_Score[best_clf_idx]
cat(sprintf("\n* Best Classification Model: %s  (F1=%.4f)\n",
            best_clf_name, best_clf_f1))

# Select best model result
best_clf_res_obj <- list(
  "Logistic Regression"       = lr_clf_res,
  "Decision Tree Classifier"  = dt_clf_res,
  "Random Forest Classifier"  = rf_clf_res,
  "SVM (RBF)"                 = svm_clf_res
)
# Only add XGBoost if it successfully trained (not NULL)
if (!is.null(xgb_clf_model)) best_clf_res_obj[["XGBoost Classifier"]] <- xgb_clf_res
best_clf_res_obj <- best_clf_res_obj[[best_clf_name]]

cat(sprintf("\n-- Detailed Classification Report: %s --\n", best_clf_name))
print(best_clf_res_obj$cm_obj)

# ── Figure 10: Confusion Matrix heatmap
cm_table <- as.data.frame(best_clf_res_obj$cm_obj$table)
names(cm_table) <- c("Predicted", "Actual", "Freq")
cm_table$Predicted <- factor(cm_table$Predicted, levels = rev(c("Low","Moderate","High")))
cm_table$Actual    <- factor(cm_table$Actual,    levels =     c("Low","Moderate","High"))

ggplot(cm_table, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#DEEBF7", high = "#08519C", name = "Count") +
  labs(title   = sprintf("Figure 10 - Confusion Matrix (%s)", best_clf_name),
       x = "Actual Class", y = "Predicted Class") +
  theme_project
savefig("fig10_confusion_matrix.png", width = 7, height = 6)
cat("[Plot] Figure 10 saved - confusion matrix\n")

# ── Figure 11: Classification metric comparison
clf_plot_data <- clf_df_table |>
  pivot_longer(cols = c(Accuracy, F1_Score, Precision, Recall),
               names_to = "Metric", values_to = "Value")

p11a <- ggplot(clf_plot_data[clf_plot_data$Metric %in% c("Accuracy","F1_Score"), ],
               aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", colour = "white") +
  scale_fill_manual(values = c("Accuracy" = "#4C72B0", "F1_Score" = "#55A868")) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(title = "Accuracy & F1 by Model", x = NULL, y = "Score") +
  theme_project + theme(axis.text.x = element_text(angle = 20, hjust = 1))

p11b <- ggplot(clf_plot_data[clf_plot_data$Metric %in% c("Precision","Recall"), ],
               aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", colour = "white") +
  scale_fill_manual(values = c("Precision" = "#DD8452", "Recall" = "#C44E52")) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(title = "Precision & Recall by Model", x = NULL, y = "Score") +
  theme_project + theme(axis.text.x = element_text(angle = 20, hjust = 1))

grid.arrange(p11a, p11b, ncol = 2,
             top = "Figure 11 - Classification Model Comparison")
savefig("fig11_classification_comparison.png", width = 14, height = 6)
cat("[Plot] Figure 11 saved - classification comparison\n")

# ── Figure 12: Classification feature importance (Random Forest)
fi_clf <- varImp(rf_clf_model)$importance
fi_clf_df <- data.frame(
  Feature    = rownames(fi_clf),
  Importance = rowMeans(fi_clf),    # mean across classes
  stringsAsFactors = FALSE
)
fi_clf_df <- fi_clf_df[order(fi_clf_df$Importance), ]
fi_clf_df$Feature <- factor(fi_clf_df$Feature, levels = fi_clf_df$Feature)

ggplot(fi_clf_df, aes(x = Importance, y = Feature)) +
  geom_bar(stat = "identity", fill = "#55A868") +
  labs(title = sprintf("Figure 12 - Feature Importance (Random Forest Classifier)"),
       x = "Importance Score", y = NULL) +
  theme_project
savefig("fig12_clf_feature_importance.png", width = 9, height = 6)
cat("[Plot] Figure 12 saved - classification feature importance\n")

cat("\nTop-3 most important features for population pressure classification:\n")
print(tail(fi_clf_df[order(fi_clf_df$Importance), ], 3))

cat("-- Sample countries per pressure class --\n")
for (lbl in c("Low", "Moderate", "High")) {
  sub <- df[df$Population_Pressure == lbl,
            c("Country", "CPPS", "Yearly_Change", "Fert_Rate", "Density_per_km2")]
  cat(sprintf("\n  %s Pressure:\n", lbl))
  print(head(sub, 5), row.names = FALSE)
}

# Correlation between CPPS and key features (interpretability)
cat("\n-- CPPS correlation with input features --\n")
cpps_corr <- cor(df[, c("CPPS","Fert_Rate","Yearly_Change","Density_per_km2",
                         "Urban_Pop_Pct","Median_Age")], use = "complete.obs")
print(round(cpps_corr["CPPS", ], 3))


cat("-- Regression Results --\n")
print(reg_df_table[ , -1])
cat(sprintf(
  "\nObjective 1 ACHIEVED: %s explains %.1f%% of variance in annual population growth.\n",
  best_reg_name, best_reg_r2 * 100
))

cat("\n-- Classification Results --\n")
print(clf_df_table[ , -1])
cat(sprintf(
  "\nObjective 2 ACHIEVED: %s classifies population pressure with weighted F1 = %.4f.\n",
  best_clf_name, best_clf_f1
))

out_csv <- file.path(OUT, "cleaned_dataset_with_labels.csv")
write.csv(df, out_csv, row.names = FALSE)
cat(sprintf("[Export] Clean dataset saved -> %s\n", out_csv))

cat("\n", strrep("=", 62), "\n", sep = "")
cat(sprintf("  ALL OUTPUTS SAVED TO: ./%s/\n", OUT))
cat("  Figures : fig1 - fig12\n")
cat("  Dataset : cleaned_dataset_with_labels.csv\n")
cat(strrep("=", 62), "\n")
cat("  PROJECT COMPLETE\n")
cat(strrep("=", 62), "\n\n")
