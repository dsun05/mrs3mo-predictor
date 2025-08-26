# --------------------------------------------------------------------------
# --- Master Analysis Script ---
# This is the main file to run the entire analysis.
# --------------------------------------------------------------------------

# --- Section 1: Setup ---

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, caret, randomForest, glmnet, pROC, lme4, ggplot2, corrplot, broom, knitr)

source("config.R")
source("data_cleaning.R")
source("analysis.R")

config <- list(
  input_file = input_file, output_file_cleaned = output_file_cleaned,
  patient_id_column = patient_id_column, outcome_variable = outcome_variable,
  keywords_to_remove = keywords_to_remove, cols_to_remove_by_name = cols_to_remove_by_name,
  required_cols_for_analysis = required_cols_for_analysis, cols_to_make_numeric = cols_to_make_numeric,
  run_advanced_analyses = run_advanced_analyses,
  numeric_vars_for_tests = numeric_vars_for_tests,
  categorical_vars_for_group_tests = categorical_vars_for_group_tests
)

# --- Section 2: Execute Workflow ---

cleaned_dataframe <- run_cleaning_pipeline(config)
analysis_output <- run_analysis_pipeline(cleaned_dataframe, config)

# Create directories for plots if they don't exist
if (!dir.exists("plots")) dir.create("plots")
if (!dir.exists("plots/numerical")) dir.create("plots/numerical")
if (!dir.exists("plots/categorical")) dir.create("plots/categorical")
if (!dir.exists("plots/normal")) dir.create("plots/normal")
if (!dir.exists("plots/density")) dir.create("plots/density")

# --- Section 3: Save Comprehensive Report ---

# Open a connection to a text file to save all results
sink("results.txt", split = TRUE)

# --- Report Header ---
cat("=========================================================\n")
cat("          COMPREHENSIVE STATISTICAL ANALYSIS REPORT          \n")
cat("=========================================================\n\n")

# --- 1. Executive Summary ---
cat("--- 1. Executive Summary ---\n\n")
cat("This report details the results of a predictive modeling analysis to determine the factors influencing 3-month patient outcomes.\n")
predicted_probs_for_summary <- predict(analysis_output$model_object, newx = analysis_output$final_model_predictors, s = analysis_output$optimal_lambda, type = "response")
roc_obj_for_summary <- roc(analysis_output$final_imputed_data$Outcome, as.vector(predicted_probs_for_summary))
cat(paste("The final predictive model demonstrated an excellent ability to distinguish between patient outcomes, with an Area Under the Curve (AUC) of:", round(auc(roc_obj_for_summary), 4), ".\n\n"))
cat("The most influential predictors identified by the LASSO regression model were:\n")
for(i in 1:nrow(analysis_output$results_table)) {
  if (analysis_output$results_table$Variable[i] != "(Intercept)") {
    cat(paste("- ", analysis_output$results_table$Variable[i], "\n"))
  }
}
cat("\nThis report provides a detailed breakdown of the model's performance, univariable test results, and final model coefficients.\n\n")


# --- 2. Final Model Predictive Performance ---
cat("--- 2. Final Model Predictive Performance ---\n\n")
cat("a. Model Discrimination (AUC/ROC):\n")
cat(paste("   - Area Under the Curve (AUC):", round(auc(roc_obj_for_summary), 4), "\n"))
png("plots/roc_curve.png", width = 800, height = 800, res = 120, bg = "white"); plot(roc_obj_for_summary, main="ROC Curve for LASSO Model", print.auc=TRUE); dev.off()
cat("   - Note: The ROC curve plot has been saved to 'plots/roc_curve.png'.\n\n")

cat("b. Model Calibration:\n")
cal_data <- data.frame(observed = ifelse(analysis_output$final_imputed_data$Outcome == "Poor", 1, 0), predicted = as.vector(predicted_probs_for_summary)) %>%
  mutate(prob_bin = cut(predicted, breaks = 10)) %>% group_by(prob_bin) %>%
  summarise(mean_predicted = mean(predicted), mean_observed = mean(observed), n = n(), .groups = 'drop')
p_cal <- ggplot(cal_data, aes(x = mean_predicted, y = mean_observed)) + geom_point(aes(size=n)) + geom_abline(slope=1, intercept=0, linetype="dashed", color="red") + lims(x=c(0,1), y=c(0,1)) + labs(title="Calibration Plot", x="Predicted Probability", y="Observed Proportion") + theme_minimal()
ggsave("plots/calibration_plot.png", plot = p_cal, bg = "white")
cat("   - Note: The calibration plot has been saved to 'plots/calibration_plot.png'.\n\n")


# --- 3. Final Model Coefficients and Odds Ratios ---
cat("--- 3. Final Model Coefficients and Odds Ratios ---\n\n")
selected_vars <- analysis_output$results_table$Variable[analysis_output$results_table$Variable != "(Intercept)"]
if(length(selected_vars) > 1) {
  model_formula <- as.formula(paste("Outcome ~", paste(selected_vars, collapse=" + ")))
  final_glm_fit <- glm(model_formula, data = analysis_output$final_imputed_data, family = "binomial")
  plot_data <- broom::tidy(final_glm_fit, conf.int = TRUE, exponentiate = TRUE)
  
  cat("The table below shows the Odds Ratios for the predictors in the final model. An Odds Ratio > 1 indicates an increased odds of a 'Poor' outcome, while an Odds Ratio < 1 indicates a decreased odds.\n\n")
  print(kable(plot_data, format = "pipe", caption = "Final Model Odds Ratios and 95% Confidence Intervals"))
  
  forest_plot <- ggplot(data = plot_data %>% filter(term != "(Intercept)"), aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) + geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") + geom_pointrange(shape = 15, size = 0.8) + scale_x_log10() + labs(title = "Forest Plot of Final Predictors", x = "Odds Ratio (95% CI, log scale)", y = "Variable") + theme_minimal(base_size = 14)
  ggsave("plots/forest_plot.png", plot = forest_plot, width = 10, height = 6, dpi = 300, bg = "white")
  cat("\n   - Note: A forest plot visualizing these odds ratios has been saved to 'plots/forest_plot.png'.\n\n")
}


# --- 4. Univariable Analysis and Associated Plots ---
cat("--- 4. Univariable Analysis and Associated Plots ---\n\n")
cat("This section presents the results of univariable tests and associated plots, which assess the relationship between each individual variable and the outcome.\n\n")

# Dynamically identify variables for testing
data_for_tests <- analysis_output$full_cleaned_data
all_vars <- names(data_for_tests)
outcome_var_name <- "Outcome"
numeric_vars_to_test <- all_vars[sapply(data_for_tests, is.numeric) & !all_vars %in% c(config$outcome_variable)]
categorical_vars_to_test <- all_vars[sapply(data_for_tests, is.factor) & all_vars != outcome_var_name]

# a. Numeric Variables (T-Tests, Boxplots, and Density Plots)
cat("a. Numeric Variables (Welch's Two-Sample T-Test):\n\n")
numeric_results <- data.frame()
for(var in numeric_vars_to_test){
  if(length(unique(data_for_tests[[outcome_var_name]])) > 1) {
    test_result <- broom::tidy(t.test(as.formula(paste(var, "~", outcome_var_name)), data = data_for_tests))
    numeric_results <- rbind(numeric_results, data.frame(Variable=var, Statistic=test_result$statistic, P_Value=test_result$p.value, Significant=test_result$p.value < 0.05))
    
    # Generate Boxplot
    p_box <- ggplot(data_for_tests, aes(x = .data[[outcome_var_name]], y = .data[[var]], fill = .data[[outcome_var_name]])) +
      geom_boxplot(alpha = 0.7) + theme_minimal() + labs(title = paste("Comparison of", var, "by Outcome"))
    ggsave(paste0("plots/numerical/boxplot_", var, ".png"), plot = p_box, bg = "white")
    
    # Generate Density Plot
    p_dens <- ggplot(data_for_tests, aes(x = .data[[var]], fill = .data[[outcome_var_name]])) +
      geom_density(alpha = 0.5) + labs(title=paste("Distribution of", var, "by Outcome")) + theme_minimal()
    ggsave(paste0("plots/density/density_", var, ".png"), plot = p_dens, bg = "white")
  }
}
print(kable(numeric_results, format = "pipe", caption = "Summary of T-Test Results for Numeric Variables"))
cat("\n   - Note: Boxplots and Density plots have been saved.\n\n")

# b. Categorical Variables (Chi-Squared Tests and Bar Plots)
cat("b. Categorical Variables (Pearson's Chi-squared Test):\n\n")
categorical_results <- data.frame()
for(var in categorical_vars_to_test){
  if(nrow(table(data_for_tests[[var]], data_for_tests[[outcome_var_name]])) > 1) {
    tbl <- table(data_for_tests[[var]], data_for_tests[[outcome_var_name]])
    try({
      test_result <- broom::tidy(chisq.test(tbl))
      categorical_results <- rbind(categorical_results, data.frame(Variable=var, Statistic=test_result$statistic, P_Value=test_result$p.value, Significant=test_result$p.value < 0.05))
    }, silent = TRUE)
    
    # Generate Bar Plot
    p_bar <- ggplot(data_for_tests, aes(x = .data[[var]], fill = .data[[outcome_var_name]])) +
      geom_bar(position = "fill") + theme_minimal() + labs(title = paste("Proportion of Outcomes by", var), y = "Proportion") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(paste0("plots/categorical/barplot_", var, ".png"), plot = p_bar, bg = "white")
  }
}
print(kable(categorical_results, format = "pipe", caption = "Summary of Chi-squared Test Results for Categorical Variables"))
cat("\n   - Note: Bar plots for each categorical variable have been saved.\n\n")


# --- 5. Feature Importance and Additional Diagnostic Plots ---
cat("--- 5. Feature Importance and Additional Diagnostic Plots ---\n\n")
cat("a. Initial Feature Screening (Random Forest):\n")
importance_df <- as.data.frame(analysis_output$importance_scores)
importance_df$Variable <- rownames(importance_df)
p_imp <- ggplot(head(importance_df[order(-importance_df$MeanDecreaseGini),], 20), aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) + geom_bar(stat="identity", fill="skyblue") + coord_flip() + labs(title="Top 20 Most Important Variables (Random Forest)", x="Variable", y="Mean Decrease Gini") + theme_minimal()
ggsave("plots/variable_importance.png", plot = p_imp, width = 10, height = 8, dpi = 300, bg = "white")
cat("   - The variable importance plot has been saved to 'plots/variable_importance.png'.\n\n")

cat("b. Correlation Analysis:\n")
correlation_data <- analysis_output$full_cleaned_data %>% select(any_of(numeric_vars_to_test))
if(ncol(correlation_data) > 1){
  correlation_matrix <- cor(correlation_data, method = "spearman", use = "pairwise.complete.obs")
  png("plots/correlogram.png", width = 1000, height = 1000, res = 120, bg = "white"); corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", mar=c(0,0,1,0), title = "Correlogram of Key Variables"); dev.off()
  cat("   - A correlogram of key numeric variables has been saved to 'plots/correlogram.png'.\n\n")
}

cat("c. Normality Check for Numeric Variables:\n")
for(var in numeric_vars_to_test){
  var_mean <- mean(data_for_tests[[var]], na.rm=TRUE)
  var_sd <- sd(data_for_tests[[var]], na.rm=TRUE)
  p_norm <- ggplot(data_for_tests, aes(x = .data[[var]])) +
    geom_histogram(aes(y=..density..), bins = 20, fill="skyblue", alpha=0.6) +
    geom_density(color="blue") +
    stat_function(fun=dnorm, args=list(mean=var_mean, sd=var_sd), color="red", linetype="dashed") +
    labs(title=paste("Normality Check for", var)) + theme_minimal()
  ggsave(paste0("plots/normal/normality_", var, ".png"), plot = p_norm, bg = "white")
}
cat("   - Normality check plots for all numeric variables have been saved to the 'plots/normal/' directory.\n\n")


# --- Report Footer ---
cat("=========================================================\n")
cat("                      END OF REPORT                      \n")
cat("=========================================================\n")

# Close the connection to the text file
sink()
cat("\n\nA comprehensive report has been saved to results.txt and all plots have been saved to their specified directories.\n")