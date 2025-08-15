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

# --- Section 3: Save Report ---

# Open a connection to a text file to save all results
sink("results.txt", split = TRUE)

cat("=========================================================\n")
cat("          STATISTICAL ANALYSIS RESULTS REPORT          \n")
cat("=========================================================\n")

cat("\n\n--- FINAL MODEL RESULTS (TOP PREDICTORS FROM LASSO) ---\n\n")
print(kable(analysis_output$results_table, format = "pipe", caption = "Final LASSO Model Predictors"))

if (config$run_advanced_analyses) {
  cat("\n\n\n--- STARTING ADVANCED ANALYSES ---\n")
  
  # Create directories for plots if they don't exist
  if (!dir.exists("plots")) dir.create("plots")
  if (!dir.exists("plots/numerical")) dir.create("plots/numerical")
  if (!dir.exists("plots/categorical")) dir.create("plots/categorical")
  
  cat("\n--- 1. Model Performance (AUC/ROC) ---\n\n")
  predicted_probs <- predict(analysis_output$model_object, newx = analysis_output$final_model_predictors, s = analysis_output$optimal_lambda, type = "response")
  roc_obj <- roc(analysis_output$final_imputed_data$Outcome, as.vector(predicted_probs))
  cat(paste("Area Under the Curve (AUC):", round(auc(roc_obj), 4), "\n"))
  png("plots/roc_curve.png", width = 800, height = 800, res = 120, bg = "white"); plot(roc_obj, main="ROC Curve for LASSO Model", print.auc=TRUE); dev.off()
  
  cat("\n\n\n--- 2. GROUP COMPARISON TESTS (UNIVARIABLE) ---\n")
  for(var in config$numeric_vars_for_tests){
    if(var %in% names(analysis_output$full_cleaned_data)){
      cat("\n T-test for variable:", var, "\n\n"); print(kable(broom::tidy(t.test(as.formula(paste(var, "~ Outcome")), data = analysis_output$full_cleaned_data)), format = "pipe"))
      p_box <- ggplot(analysis_output$full_cleaned_data, aes(x = Outcome, y = .data[[var]], fill = Outcome)) + geom_boxplot(alpha = 0.7) + theme_minimal() + labs(title = paste("Comparison of", var, "by Outcome"))
      ggsave(paste0("plots/numerical/boxplot_", var, ".png"), plot = p_box, bg = "white")
    }
  }
  for(var in config$categorical_vars_for_group_tests){
    if(var %in% names(analysis_output$full_cleaned_data)){
      cat("\n Chi-squared test for variable:", var, "\n\n"); tbl <- table(analysis_output$full_cleaned_data[[var]], analysis_output$full_cleaned_data$Outcome); print(kable(broom::tidy(chisq.test(tbl)), format = "pipe"))
      p_bar <- ggplot(analysis_output$full_cleaned_data, aes(x = .data[[var]], fill = Outcome)) + geom_bar(position = "fill") + theme_minimal() + labs(title = paste("Proportion of Outcomes by", var), y = "Proportion")
      ggsave(paste0("plots/categorical/barplot_", var, ".png"), plot = p_bar, bg = "white")
    }
  }
  
  cat("\n\n\n--- 3. ODDS RATIOS WITH 95% CONFIDENCE INTERVALS ---\n\n")
  selected_vars <- analysis_output$results_table$Variable[analysis_output$results_table$Variable != "(Intercept)"]
  if(length(selected_vars) > 1) {
    model_formula <- as.formula(paste("Outcome ~", paste(selected_vars, collapse=" + ")))
    final_glm_fit <- glm(model_formula, data = analysis_output$final_imputed_data, family = "binomial")
    plot_data <- broom::tidy(final_glm_fit, conf.int = TRUE, exponentiate = TRUE) %>% filter(term != "(Intercept)")
    print(kable(plot_data, format = "pipe", caption = "Odds Ratios and 95% Confidence Intervals"))
    forest_plot <- ggplot(data = plot_data, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) + geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") + geom_pointrange(shape = 15, size = 0.8) + scale_x_log10() + labs(title = "Forest Plot of Final Predictors", x = "Odds Ratio (95% CI, log scale)", y = "Variable") + theme_minimal(base_size = 14)
    ggsave("plots/forest_plot.png", plot = forest_plot, width = 10, height = 6, dpi = 300, bg = "white")
    cat("\nForest plot saved to plots/forest_plot.png\n")
  }
  
  cat("\n\n\n--- 4. SPEARMAN CORRELATION OF KEY NUMERIC VARIABLES ---\n\n")
  correlation_data <- analysis_output$full_cleaned_data %>% select(any_of(config$numeric_vars_for_tests))
  if(ncol(correlation_data) > 1){
    correlation_matrix <- cor(correlation_data, method = "spearman", use = "pairwise.complete.obs")
    png("plots/correlogram.png", width = 1000, height = 1000, res = 120, bg = "white"); corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", mar=c(0,0,1,0), title = "Correlogram of Key Variables"); dev.off()
    cat("\nCorrelogram saved to plots/correlogram.png\n")
  }
  
  # --- Section 5. ADDITIONAL DIAGNOSTIC AND MODEL PLOTS ---
  cat("\n\n\n--- 5. ADDITIONAL DIAGNOSTIC AND MODEL PLOTS ---\n")
  
  # a. Variable Importance Plot
  cat("\nGenerating Variable Importance Plot...\n")
  importance_df <- as.data.frame(analysis_output$importance_scores)
  importance_df$Variable <- rownames(importance_df)
  p_imp <- ggplot(head(importance_df[order(-importance_df$MeanDecreaseGini),], 20), aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) + geom_bar(stat="identity", fill="skyblue") + coord_flip() + labs(title="Top 20 Most Important Variables (Random Forest)", x="Variable", y="Mean Decrease Gini") + theme_minimal()
  ggsave("plots/variable_importance.png", plot = p_imp, width = 10, height = 8, dpi = 300, bg = "white")
  cat("Variable importance plot saved to plots/variable_importance.png\n")
  
  # b. Calibration Plot
  cat("\nGenerating Calibration Plot...\n")
  cal_data <- data.frame(observed = ifelse(analysis_output$final_imputed_data$Outcome == "Poor", 1, 0), predicted = as.vector(predicted_probs)) %>%
    mutate(prob_bin = cut(predicted, breaks = 10)) %>% group_by(prob_bin) %>%
    summarise(mean_predicted = mean(predicted), mean_observed = mean(observed), n = n(), .groups = 'drop')
  p_cal <- ggplot(cal_data, aes(x = mean_predicted, y = mean_observed)) + geom_point(aes(size=n)) + geom_abline(slope=1, intercept=0, linetype="dashed", color="red") + lims(x=c(0,1), y=c(0,1)) + labs(title="Calibration Plot", x="Predicted Probability", y="Observed Proportion") + theme_minimal()
  ggsave("plots/calibration_plot.png", plot = p_cal, bg = "white")
  cat("Calibration plot saved to plots/calibration_plot.png\n")
  
  # c. Density Plots for Group Comparison
  cat("\nGenerating Density Plots...\n")
  for(var in config$numeric_vars_for_tests){
    if(var %in% names(analysis_output$full_cleaned_data)){
      p_dens <- ggplot(analysis_output$full_cleaned_data, aes(x = .data[[var]], fill = Outcome)) + geom_density(alpha = 0.5) + labs(title=paste("Distribution of", var, "by Outcome")) + theme_minimal()
      ggsave(paste0("plots/density_", var, ".png"), plot = p_dens, bg = "white")
    }
  }
  
  # d. Normality Check for Top 50 Numeric Variables
  cat("\nGenerating Normality Check Plots...\n")
  if (!dir.exists("plots/normal")) dir.create("plots/normal")
  top_50_vars <- analysis_output$top_50_vars
  numeric_top_50 <- intersect(top_50_vars, names(analysis_output$full_cleaned_data)[sapply(analysis_output$full_cleaned_data, is.numeric)])
  for(var in numeric_top_50){
    var_mean <- mean(analysis_output$full_cleaned_data[[var]], na.rm=TRUE)
    var_sd <- sd(analysis_output$full_cleaned_data[[var]], na.rm=TRUE)
    p_norm <- ggplot(analysis_output$full_cleaned_data, aes(x = .data[[var]])) + geom_histogram(aes(y=..density..), bins = 20, fill="skyblue", alpha=0.6) + geom_density(color="blue") + stat_function(fun=dnorm, args=list(mean=var_mean, sd=var_sd), color="red", linetype="dashed") + labs(title=paste("Normality Check for", var)) + theme_minimal()
    ggsave(paste0("plots/normal/normality_", var, ".png"), plot = p_norm, bg = "white")
  }
  
  # e. Chi-squared Tests for Top 50 Categorical Variables
  cat("\n\nGenerating Chi-squared Tests and Plots for Top 50 Categorical Variables...\n")
  top_50_vars <- analysis_output$top_50_vars
  categorical_top_50 <- intersect(top_50_vars, names(analysis_output$full_cleaned_data)[sapply(analysis_output$full_cleaned_data, is.factor)])
  for(var in categorical_top_50) {
    if (var %in% names(analysis_output$full_cleaned_data)) {
      cat("\n--- Chi-squared test for Top 50 variable:", var, "---\n\n")
      tbl <- table(analysis_output$full_cleaned_data[[var]], analysis_output$full_cleaned_data$Outcome)
      try({
        chisq_res <- chisq.test(tbl)
        print(kable(broom::tidy(chisq_res), format = "pipe"))
      }, silent = TRUE)
      
      p_bar_top50 <- ggplot(analysis_output$full_cleaned_data, aes(x = .data[[var]], fill = Outcome)) + 
        geom_bar(position = "fill") + 
        theme_minimal() + 
        labs(title = paste("Proportion of Outcomes by", var), y = "Proportion") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggsave(paste0("plots/categorical/chi_sq_top50_", var, ".png"), plot = p_bar_top50, bg = "white", width = 8, height = 6)
    }
  }
}

cat("\n\n=========================================================\n")
cat("                      END OF REPORT                      \n")
cat("=========================================================\n")

# Close the connection to the text file
sink()
cat("\n\nAll results have been saved to results.txt and plots saved to specified directories.\n")