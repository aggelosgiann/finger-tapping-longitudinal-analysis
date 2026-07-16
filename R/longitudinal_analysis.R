# ==========================================
# Prepare Libraries and Working Directory
# ==========================================

# Set working directory
setwd("C:\\Users\\aggel\\Documents\\Data")

# Install libraries
install.packages("dplyr")
install.packages("tidyr")
install.packages("broom")
install.packages("lmerTest")
install.packages("lme4")
install.packages("sjPlot")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("ggeffects")

# Load libraries
library(dplyr)
library(tidyr)
library(broom)
library(lmerTest)
library(lme4)
library(sjPlot)
library(ggplot2)
library(patchwork)
library(ggeffects)

# ==========================================
# 1. Unified Data Input & Reshaping
# ==========================================

# Load the new consolidated dataset
df_raw <- read.csv("datawithkeys.csv", stringsAsFactors = FALSE)

# Pivot the wide dataset to a long format suitable for LMM
# names_pattern "(.*)(\\d)$" grabs the base name and suffix separately
df_long <- df_raw %>%
  pivot_longer(
    cols = matches("[123]$"),
    names_to = c(".value", "Time_Suffix"),
    names_pattern = "(.*)(\\d)$"
  ) %>%
  mutate(
    Time = as.numeric(Time_Suffix) - 1, # Converts 1, 2, 3 -> 0, 1, 2
    Time_Num = Time,
    Time_Factor = as.factor(Time)
  )

# ==========================================
# 2. Data Cleaning & Variable Mapping
# ==========================================

df_clean <- df_long %>%
  rename(
    Patient_ID = name,
    Age = age,
    MoCA = moca,
    MIS = mis,
    EIS = eis,
    MMSE = mmse
  ) %>%
  mutate(
    # Set Baseline Status (Assuming 'mci' designates baseline: 0=NC, 1=MCI)
    Baseline_Status = factor(mci, levels = c(0, 1), labels = c("0", "1")),
    Outcome_Binary = ifelse(mci != 0, 1, 0),
    
    # Map Gender 
    Gender = ifelse(sex == 0, "Α", "Θ"),
    
    # Map Kinematic Variables based on f0=Middle, f1=Index
    Mean_ISI_Middle = f0_release2releaseintervals_mean,
    Mean_ISI_Index  = f1_release2releaseintervals_mean,
    
    # Calculate Standard Deviation from Coefficient of Variation (SD = CV * Mean)
    SD_ISI_Middle = f0_release2releaseintervals_cv * Mean_ISI_Middle,
    SD_ISI_Index  = f1_release2releaseintervals_cv * Mean_ISI_Index,
    
    # Map Duration
    Mean_DUR_Middle = f0_pressin2pressoutintervals_mean,
    Mean_DUR_Index  = f1_pressin2pressoutintervals_mean,
    
    # Map Total Taps 
    Taps_Middle = f0_press_sum,
    Taps_Index  = f1_press_sum
  )%>%
  # Convert raw hardware counts to milliseconds
  mutate(
    Mean_ISI_Middle = Mean_ISI_Middle * 0.76,
    Mean_ISI_Index  = Mean_ISI_Index * 0.76,
    SD_ISI_Middle   = SD_ISI_Middle * 0.76,
    SD_ISI_Index    = SD_ISI_Index * 0.76
  )

# ==========================================
# 3. Prepare Data for Longitudinal Analysis
# ==========================================

df_final_analysis <- df_clean %>%
  group_by(Gender) %>% 
  mutate(
    # Centralize Age
    Age_centered = Age - mean(Age, na.rm = TRUE),
    # Centralize Education
    Edu_centered = Education - mean(Education, na.rm = TRUE),
    
    # Centralize ISI_Index & scale over 100ms
    Mean_ISI_Index_centered = Mean_ISI_Index - mean(Mean_ISI_Index, na.rm = TRUE),
    ISI_Index_100ms_centered = Mean_ISI_Index_centered / 100,
    
    # Centralize ISI_Middle & scale over 100ms
    Mean_ISI_Middle_centered = Mean_ISI_Middle - mean(Mean_ISI_Middle, na.rm = TRUE),
    ISI_Middle_100ms_centered = Mean_ISI_Middle_centered / 100,
    
    # Centralize SD_ISI of middle finger & scale over 100ms
    SD_ISI_Middle_centered = SD_ISI_Middle - mean(SD_ISI_Middle, na.rm = TRUE),
    SD_ISI_Middle_100ms_centered = SD_ISI_Middle_centered / 100,
    
    # Centralize SD_ISI of index finger & scale over 100ms
    SD_ISI_Index_centered = SD_ISI_Index - mean(SD_ISI_Index, na.rm = TRUE),
    SD_ISI_Index_100ms_centered = SD_ISI_Index_centered / 100
  ) %>%
  ungroup() %>% 
  # Ensure Patient_ID is a factor for the LMM random effects
  mutate(Patient_ID = as.factor(Patient_ID))

# ==========================================
# 4. Predict Year 3 Scores from Baseline
# ==========================================

# Step 1: Extract Baseline (Time == 0) predictors
df_baseline <- df_final_analysis %>%
  filter(Time == 0) %>%
  select(
    Patient_ID, 
    Gender, 
    MoCA_base = MoCA, 
    MIS_base = MIS, 
    EIS_base = EIS,
    ISI_Middle_100ms_centered, ISI_Index_100ms_centered, 
    SD_ISI_Middle_100ms_centered, SD_ISI_Index_100ms_centered,
    Taps_Middle, Taps_Index
  )

# Step 2: Extract Year 3 (Time == 2) outcomes
df_year3 <- df_final_analysis %>%
  filter(Time == 2) %>%
  select(
    Patient_ID, 
    MoCA_Y3 = MoCA, 
    MIS_Y3 = MIS, 
    EIS_Y3 = EIS
  )

# Step 3: Merge baseline predictors with Year 3 outcomes
df_model_data <- inner_join(df_baseline, df_year3, by = "Patient_ID")

# Step 4: Define the variables to loop through
cognitive_tests <- c("MoCA", "MIS", "EIS")
kinematic_vars <- c("ISI_Middle_100ms_centered", "ISI_Index_100ms_centered", 
                    "SD_ISI_Middle_100ms_centered", "SD_ISI_Index_100ms_centered",
                    "Taps_Middle", "Taps_Index")

# Initialize an empty list to store results
results_list <- list()

# Step 5: Loop through tests and variables to build models
for (test in cognitive_tests) {
  
  # Define Dependent Variable (Year 3) and Baseline IV
  dv <- paste0(test, "_Y3")
  base_iv <- paste0(test, "_base")
  
  for (kin in kinematic_vars) {
    
    # Construct the formula: e.g., MoCA_Y3 ~ MoCA_base + Gender + Mean_ISI_Middle
    form_string <- paste(dv, "~", base_iv, "+ Gender +", kin)
    form <- as.formula(form_string)
    
    # Fit the linear model
    fit <- lm(form, data = df_model_data)
    
    # Extract model performance metrics using broom::glance()
    fit_glance <- glance(fit)
    
    # Extract p-value for the specific kinematic predictor using broom::tidy()
    fit_tidy <- tidy(fit)
    
    # Safely pull the p-value for the kinematic variable (in case of NAs or collinearity)
    p_val_kinematic <- fit_tidy %>% 
      filter(term == kin) %>% 
      pull(p.value)
    
    if (length(p_val_kinematic) == 0) p_val_kinematic <- NA
    
    # Store the results in a dataframe
    res <- data.frame(
      Cognitive_Test = test,
      Kinematic_Predictor = kin,
      Adj_R_Squared = fit_glance$adj.r.squared,
      AIC = fit_glance$AIC,
      P_Value_Kinematic = p_val_kinematic,
      stringsAsFactors = FALSE
    )
    
    # Append to the list
    results_list[[length(results_list) + 1]] <- res
  }
}

# Combine the list into a single dataframe
final_model_comparisons <- bind_rows(results_list)

# Filter for statistically significant kinematic predictors (p < 0.05)
significant_models <- final_model_comparisons %>%
  filter(P_Value_Kinematic < 0.05) %>%
  arrange(P_Value_Kinematic) # Sorting from lowest p-value to highest

# View the significant results
print(significant_models)

# ==========================================
# 5. Presentation of Significant Models
# ==========================================
# Re-fit the 3 Models with Scaled Variables
model_1 <- lm(MoCA_Y3 ~ MoCA_base + Gender + SD_ISI_Index_100ms_centered, data = df_model_data)
model_2 <- lm(MoCA_Y3 ~ MoCA_base + Gender + ISI_Index_100ms_centered, data = df_model_data)
model_3 <- lm(MoCA_Y3 ~ MoCA_base + Gender + SD_ISI_Middle_100ms_centered, data = df_model_data)

# Generate the Fixed Tables
# MODEL 1: SD_ISI_Index
tab_model(
  model_1,
  title = "Table 1: Predicting Year 3 Cognitive State (MoCA) using Baseline Index Finger Tapping Variability",
  dv.labels = "Year 3 MoCA Score",
  pred.labels = c("(Intercept)", "Baseline MoCA Score", "Gender", "100ms increase from average SD of ISI of Index Finger"),
  show.se = TRUE,
  show.stat = TRUE,
  file = "Model_1_SD_Index.html"
)

# MODEL 2: Mean_ISI_Index
tab_model(
  model_2,
  title = "Table 2: Predicting Year 3 Cognitive State (MoCA) using Baseline Index Finger Mean ISI",
  dv.labels = "Year 3 MoCA Score",
  pred.labels = c("(Intercept)", "Baseline MoCA Score", "Gender", "100ms increase from Mean ISI of Index Finger"),
  show.se = TRUE,
  show.stat = TRUE,
  file = "Model_2_Mean_Index.html"
)

# MODEL 3: SD_ISI_Middle
tab_model(
  model_3,
  title = "Table 3: Predicting Year 3 Cognitive State (MoCA) using Baseline Middle Finger Tapping Variability",
  dv.labels = "Year 3 MoCA Score",
  pred.labels = c("(Intercept)", "Baseline MoCA Score", "Gender", "100ms increase from average SD of ISI of Middle Finger"),
  show.se = TRUE,
  show.stat = TRUE,
  file = "Model_3_SD_Middle.html"
)

# ==========================================
# 6. Longitudinal LMM for MoCA Trajectory
# ==========================================

# Fit the Linear Mixed Model
# The interaction term (Time_Num * ISI_Index_100ms_centered) tests whether 
# the trajectory of MoCA over time depends on the Index Finger ISI.
lmm_moca_index <- lmer(
  MoCA ~ Gender + Age_centered + Edu_centered + Time_Num * SD_ISI_Middle_100ms_centered + (1 | Patient_ID), 
  data = df_final_analysis,
  control = lmerControl(optimizer = "bobyqa")
)

# Generate the analytical presentation summary table
tab_model(
  lmm_moca_index,
  title = "Table 4: Longitudinal Trajectory of Cognitive State (MoCA) Predicted by Index Finger Mean ISI",
  dv.labels = "Longitudinal MoCA Score",
  pred.labels = c(
    "(Intercept)", 
    "Gender (Women)",               
    "Age (Centered)", 
    "Education (Centered)", 
    "Time (Years)", 
    "Mean ISI of Index Finger (100ms centered)", 
    "Time × Mean ISI (Interaction)"
  ),
  show.se = TRUE,
  show.stat = TRUE,
  file = "Model_4_LMM_MoCA_Index_Trajectory.html"
)

# ==========================================
# 7. Stratified Longitudinal Trajectory Plot
# ==========================================

# Fit a new stratified model 
# The 3-way interaction lets the slope of Time vary by both ISI and Baseline Status
lmm_moca_stratified <- lmer(
  MoCA ~ Gender + Age_centered + Edu_centered + 
    Time_Num * ISI_Index_100ms_centered * Baseline_Status + 
    (1 | Patient_ID), 
  data = df_final_analysis,
  control = lmerControl(optimizer = "bobyqa")
)

# Calculate the Standard Deviation for the centered ISI index
# We round it to 3 decimals so ggpredict parses it cleanly
sd_isi <- round(sd(df_final_analysis$ISI_Index_100ms_centered, na.rm = TRUE), 3)

# Generate marginal predictions using ggpredict
# This tests the MoCA trajectory at Time = 0, 1, 2
# at -1 SD (Faster), Mean (0), and +1 SD (Slower) of the ISI_Index
# stratified by Baseline Status (0 = NC, 1 = MCI)
predictions <- ggpredict(
  lmm_moca_stratified, 
  terms = c(
    "Time_Num", 
    paste0("ISI_Index_100ms_centered [-", sd_isi, ", 0, ", sd_isi, "]"), 
    "Baseline_Status"
  )
)

# Clean up the labels for a professional plot
# Rename the facet (Baseline_Status) labels
levels(predictions$facet) <- c("Normal Cognition (NC)", "Mild Cognitive Impairment (MCI)")

# Rename the grouping (ISI Index standard deviations) labels
predictions$group <- factor(
  predictions$group,
  labels = c(
    paste0("-1 SD (Faster Tapping)"), 
    "Mean (Average)", 
    paste0("+1 SD (Slower Tapping)")
  )
)

# Build a polished, publication-ready ggplot
trajectory_plot <- ggplot(predictions, aes(x = x, y = predicted, color = group, fill = group)) +
  # Add confidence interval ribbons (transparent)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linetype = 0) +
  # Add trajectory lines
  geom_line(size = 1.2) +
  # Split the plot into two panels based on Baseline Status
  facet_wrap(~ facet) +
  # Customize the X-axis for longitudinal timepoints
  scale_x_continuous(breaks = c(0, 1, 2), labels = c("Baseline", "Year 1", "Year 2")) +
  # Use a colorblind-friendly palette
  scale_color_manual(
    name = "Index Finger Mean ISI",
    values = c("#0072B2", "#E69F00", "#D55E00") 
  ) +
  scale_fill_manual(
    name = "Index Finger Mean ISI",
    values = c("#0072B2", "#E69F00", "#D55E00")
  ) +
  # Add descriptive labels
  labs(
    title = "Longitudinal Trajectory of MoCA Scores over 2 Years",
    subtitle = "Stratified by Baseline Cognitive Status and Index Finger Tapping Speed (Mean ISI ± 1 SD)",
    x = "Time",
    y = "Predicted MoCA Score"
  ) +
  # Apply a clean theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines") # Add breathing room between the two panels
  )

# Display the plot in the RStudio viewer
print(trajectory_plot)

# Save the plot in high resolution (Optional)
ggsave("MoCA_Trajectory_Stratified.png", plot = trajectory_plot, width = 10, height = 6, dpi = 300)

# ==========================================
# 8. Diagnostic Plots (Combined 3x2 Grid)
# ==========================================

# Define a reusable function to build a single row
create_diagnostic_row <- function(model, row_title) {
  
  # Extract model metrics using broom
  model_data <- augment(model)
  
  # ---------------------------------------------------------
  # Panel 1: Residuals vs Fitted
  # ---------------------------------------------------------
  p_resid <- ggplot(model_data, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6, color = "#0072B2", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#D55E00", linewidth = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1) +
    labs(x = "Fitted Values", y = "Residuals") +
    theme_minimal(base_size = 14) +
    theme(
      aspect.ratio = 1, 
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      plot.margin = margin(5, 10, 5, 5)
    )
  
  # ---------------------------------------------------------
  # Panel 2: Normal Q-Q
  # ---------------------------------------------------------
  max_val <- max(abs(model_data$.std.resid), na.rm = TRUE)
  axis_limit <- ceiling(max_val)
  
  p_qq <- ggplot(model_data, aes(sample = .std.resid)) +
    stat_qq(alpha = 0.6, color = "#0072B2", size = 2) +
    stat_qq_line(linetype = "dashed", color = "#D55E00", linewidth = 1) +
    scale_x_continuous(limits = c(-axis_limit, axis_limit), breaks = -axis_limit:axis_limit) +
    scale_y_continuous(limits = c(-axis_limit, axis_limit), breaks = -axis_limit:axis_limit) +
    labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
    theme_minimal(base_size = 14) +
    theme(
      aspect.ratio = 1, 
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      plot.margin = margin(5, 5, 5, 10)
    )
  
  # ---------------------------------------------------------
  # Combine, Annotate, and Wrap
  # ---------------------------------------------------------
  # Combine the two plots and add the row-specific title
  row_pw <- (p_resid + p_qq) +
    plot_annotation(
      title = row_title,
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.margin = margin(10, 5, 10, 5) 
      )
    )
  
  # wrap_elements() "freezes" the row into a single protected object.
  # This prevents the layout crash AND stops the global title from overwriting it.
  return(wrap_elements(row_pw))
}

# Generate the 3 individual wrapped rows
row_1 <- create_diagnostic_row(model_1, "Model 1: SD of ISI, Index Finger")
row_2 <- create_diagnostic_row(model_2, "Model 2: Mean ISI, Index Finger")
row_3 <- create_diagnostic_row(model_3, "Model 3: SD of ISI, Middle Finger")

# Combine all rows vertically
final_combined_rows <- row_1 / row_2 / row_3

# Apply the formal title and the requested original caption
final_3x2_grid <- final_combined_rows +
  plot_annotation(
    title = "Diagnostic Evaluation of Ordinary Least Squares Regression Models",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      # hjust = 0 keeps it left-aligned, but the str_wrap removes the jagged look
      plot.caption = element_text(size = 12, hjust = 0, color = "black", margin = margin(t = 20), lineheight = 1.2),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

# Save the combined figure for the manuscript
ggsave("Diagnostics.png", plot = final_3x2_grid, width = 10, height = 15, dpi = 300)