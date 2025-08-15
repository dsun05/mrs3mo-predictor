# This script defines the data cleaning and feature selection pipeline.
# It is called by master_script.R.

run_cleaning_pipeline <- function(config) {
  
  # --- Data Loading and Initial Cleaning ---
  cat("--- Starting Data Cleaning ---\n")
  raw_data <- read.csv(config$input_file, na.strings = c("N/A", ""))
  
  cat("Removing columns by keyword...\n")
  data_step1 <- raw_data %>%
    select(!contains(config$keywords_to_remove, ignore.case = TRUE))
  cat("Removing columns by name...\n")
  data_step2 <- data_step1 %>%
    select(-any_of(config$cols_to_remove_by_name))
  
  # --- Data Fixing and Pre-processing ---
  cat("Fixing data types...\n")
  data_step3 <- data_step2 %>%
    mutate(across(any_of(config$cols_to_make_numeric), ~as.numeric(as.character(.))))
  cat("Saving cleaned data to:", config$output_file_cleaned, "\n")
  write.csv(data_step3, config$output_file_cleaned, row.names = FALSE, na = "")
  cat("Filtering rows with missing required data...\n")
  original_n <- nrow(data_step3)
  df <- data_step3 %>% drop_na(all_of(config$required_cols_for_analysis))
  filtered_n <- nrow(df)
  cat("Patients remaining after filtering:", filtered_n, "\n\n")
  
  # --- Feature Engineering and Selection ---
  cat("--- Starting Feature Selection ---\n")
  cat("Step A: Removing near-zero variance columns...\n")
  nzv <- nearZeroVar(df, saveMetrics = TRUE)
  cols_to_remove_nzv <- rownames(nzv[nzv$nzv == TRUE, ])
  df_filtered_1 <- df[, !names(df) %in% cols_to_remove_nzv]
  cat("Step B: Removing highly correlated columns...\n")
  numeric_cols <- df_filtered_1 %>% select(where(is.numeric))
  
  # FIX IS HERE: We now explicitly call stats::var to avoid any conflicts.
  variances <- sapply(numeric_cols, stats::var, na.rm = TRUE)
  
  zero_variance_cols <- names(variances[variances == 0 | is.na(variances)])
  if(length(zero_variance_cols) > 0){
    cat("Found and removed", length(zero_variance_cols), "constant columns before correlation check.\n")
    df_filtered_1 <- df_filtered_1[, !names(df_filtered_1) %in% zero_variance_cols]
    numeric_cols <- numeric_cols[, !names(numeric_cols) %in% zero_variance_cols]
  }
  correlation_matrix <- cor(numeric_cols, use = "pairwise.complete.obs")
  highly_correlated_cols <- findCorrelation(correlation_matrix, cutoff = 0.80)
  cols_to_remove_corr <- colnames(numeric_cols)[highly_correlated_cols]
  df_filtered_2 <- df_filtered_1[, !names(df_filtered_1) %in% cols_to_remove_corr]
  cat("Removed", length(cols_to_remove_corr), "highly correlated columns.\n\n")
  
  return(df_filtered_2)
}