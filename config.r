# --------------------------------------------------------------------------
# --- Section 1: Configuration ---
# --------------------------------------------------------------------------

run_advanced_analyses <- TRUE
input_file <- "raw.csv"
output_file_cleaned <- "raw_cleaned.csv"
patient_id_column <- "SubjectID"
outcome_variable <- "mrs3mo"
keywords_to_remove <- c("note", "thromboutcome", "walkstatusdis")
cols_to_remove_by_name <- c(
  "SubjectIDtype", "ArrivalDate", "ArrivalTime", "LastKnownWellDate", 
  "lastKnownWellTime", "ivthrombDate", "ivthrombTime", "stay", "death", "cod",
  "GIbleedcomp", "heartcomp", "afib2", "uti", "pna", "peg", "trach",
  "LoCmonthandage", "eyeopeningandgrip", "bestgaze", "vffacialparesis",
  "motorleftarm", "motorrightarm", "motorleftleg", "motorrightleg",
  "limbataxia", "sensory", "bestlanguage", "dysarthria", "extinctioninattention",
  "DischargeDate", "DischargeTime", "DischargeStatus", "vf_discharge" 
)
required_cols_for_analysis <- c(
  "gcsadm", "mrsarrival", "InitialNIHSS", "MRSbase_manual", 
  "PreThrombectomyNIHSS", "PreThrombectomyGCS", "GCSdis", "DischargeMRS",
  outcome_variable
)
cols_to_make_numeric <- c(
  "InitialNIHSS", "PostThrombectomyNIHSS", "PreThrombectomyNIHSS", 
  "GCSdis", "mrsarrival", "Hgb", "hcrt", "MRSbase_manual",
  "PostThrombectomyGCS", "maxcreat", "gluc", "esr", "gcsadm",
  "creat", "SBPadm", "phosph", "SNAP", "plat", "calc", "ptt",
  "WBC", "maxNa", "chlr", "minNa", "magn", "maxtrop", "DischargeMRS", "oshinr", "Age"
)

# --- Variables for Advanced Analyses ---
# UPDATED: Use the specific list of variables for group and correlation tests
numeric_vars_for_tests <- c(
  "gcsadm", "mrsarrival", "InitialNIHSS", "MRSbase_manual", 
  "PreThrombectomyNIHSS", "PreThrombectomyGCS", "GCSdis", "DischargeMRS"
)
# Note: mrs3mo is the outcome, so it's not included here as a predictor to test.
# Add any categorical variables for Chi-squared tests below
categorical_vars_for_group_tests <- c("sex", "htn", "dvtpe")