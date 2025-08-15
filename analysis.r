# This script defines the modeling pipeline.
# It is called by master_script.R.

run_analysis_pipeline <- function(cleaned_data, config) {
  
  # --- Modeling (Random Forest + LASSO) ---
  cat("--- Starting Modeling ---\n")
  df_filtered_2 <- cleaned_data
  df_filtered_2$Outcome <- as.factor(ifelse(df_filtered_2[[config$outcome_variable]] <= 2, "Good", "Poor"))
  df_filtered_2 <- df_filtered_2 %>% 
    mutate(across(where(is.character), as.factor))
  factor_levels <- sapply(df_filtered_2, function(x) if(is.factor(x)) nlevels(x) else 0)
  high_cardinality_cols <- names(factor_levels[factor_levels > 53])
  cols_to_exclude_rf <- c(config$outcome_variable, config$patient_id_column, high_cardinality_cols)
  rf_data <- df_filtered_2[, !names(df_filtered_2) %in% cols_to_exclude_rf]
  set.seed(123)
  rf_model <- randomForest(
    Outcome ~ ., 
    data = rf_data, 
    na.action = na.roughfix
  )
  importance_scores <- importance(rf_model)
  top_variables <- rownames(importance_scores)[order(importance_scores[, 1], decreasing = TRUE)][1:50]
  final_df <- df_filtered_2[, c("Outcome", top_variables)]
  final_df_imputed <- na.roughfix(final_df) 
  y <- final_df_imputed$Outcome
  x <- model.matrix(Outcome ~ . - 1, data = final_df_imputed)
  set.seed(123)
  cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 1)
  
  # --- Final Results ---
  optimal_lambda <- cv_fit$lambda.1se
  final_coeffs <- coef(cv_fit, s = optimal_lambda)
  odds_ratios <- as.matrix(exp(final_coeffs))
  final_results <- data.frame(
    Variable = rownames(odds_ratios),
    OddsRatio = odds_ratios[,1]
  ) %>%
    filter(OddsRatio != 1.0) %>%
    arrange(desc(OddsRatio))
  
  # Return a list of objects for advanced analysis
  return(
    list(
      results_table = final_results,
      model_object = cv_fit,
      final_imputed_data = final_df_imputed,
      full_cleaned_data = df_filtered_2,
      final_model_predictors = x,
      optimal_lambda = optimal_lambda,
      importance_scores = importance_scores,
      top_50_vars = top_variables
    )
  )
}