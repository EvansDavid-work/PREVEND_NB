### Baseline Characteristics and Descriptive Stats###

install.packages("gtsummary")
library(gtsummary)

#Baseline Characteristics Table
table1.3 <- Full_NP %>%
  select(AgeAtBaseline, SEX, RACE, education, BMI,
         WaistHip_NC2,
         Mean24hAlbumin_NC2, eGFR_NC2, Creat_NC2, MeanUCreat_NC2, CYSC_NC2, 
         Macroalbuminuria_NC2,CKD_NC2, CVD_Status, HYP_NC2, ANTIHYP_2B, 
         ANTILIP_2B, V26_2B, V32_2B, V31_2B,
         DIAB_NC2, GLUC_2, ANTIDIAB_2B, Microalbuminuria_NC2, DSBP_2B, DDBP_2B,
         ELP_3_HP_ng_ml, C3M_HP_ng_ml, PRO_C16_HP_ng_ml, 
         PRO_C4_HP_ng_ml, VICM_HP_ng_ml, PRO_C3_roHP_ng_ml) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_n() %>%
  modify_table_body(
    ~.x %>%
      mutate(label = case_when(
        label == "AgeAtBaseline" ~ "Age",
        label == "SEX" ~ "Sex",
        label == "RACE" ~ "Race",
        label == "education" ~ "Level of Education",
        label == "BMI" ~ "BMI",
        label == "WaistHip_NC2" ~ "Waist-Hip Ratio",
        label == "V31_2B" ~ "Consumption of Alcoholic Beverages",
        label == "V32_2B" ~ "Frequency of Exercise",
        label == "V26_2B" ~ "Smoking",
        label == "eGFR_NC2" ~ "eGFR",
        label == "Mean24hAlbumin_NC2" ~ "24h Urine Albumin",
        label == "Creat_NC2" ~ "Plasma Creatinine",
        label == "MeanUCreat_NC2" ~ "Urinary Creatinine",
        label == "CYSC_NC2" ~ "Cystatin C",
        label == "sodium_NC2" ~ "Plasma Sodium",
        label == "potassium_NC2" ~ "Plasma Potassium",
        label == "CKD_NC2" ~ "CKD Positive",
        label == "CVD_Status" ~ "Cardiovascular Event(s) Prior to Start Date",
        label == "HYP_NC2" ~ "Hypertension",
        label == "DIAB_NC2" ~ "Diabetes",
        label == "GLUC_2" ~ "Glucose",
        label == "ANTIDIAB_2B" ~ "Antidiabetic Medication",
        label == "ANTIHYP_2B" ~ "Antihypertensive Medication",
        label == "DSBP_2B" ~ "Systolic Blood Pressure (mmHg)",
        label == "DDBP_2B" ~ "Diastolic Blood Pressure (mmHg)",
        label == "ANTILIP_2B" ~ "Antilipidemic medication",
        label == "C3M_HP_ng_ml" ~ "C3M",
        label == "ELP_3_HP_ng_ml" ~ "ELP-3",
        label == "PRO_C16_HP_ng_ml" ~ "PRO-C16",
        label == "PRO_C4_HP_ng_ml" ~ "PRO-C4",
        label == "PRO_C3_roHP_ng_ml" ~ "PRO-C3",
        label == "VICM_HP_ng_ml" ~ "VICM",
        TRUE ~ label
      ))
  ) %>%
  modify_caption("**Table 1. Patient Baseline and Follow-up Characteristics**") %>%
  bold_labels()
print(table1.3)

# Summaries

biomarkers <- c("ELP_3_HP_ng_ml", "C3M_HP_ng_ml", "PRO_C16_HP_ng_ml", 
                "PRO_C4_HP_ng_ml", "VICM_HP_ng_ml", "PRO_C3_roHP_ng_ml")

# Create Function to calculate summary statistics
calculate_summary <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarize(
      across(
        all_of(biomarkers),
        list(
          Median = ~median(.x, na.rm = TRUE),
          Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
          Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
        ),
        .names = "{col}_{fn}"
      ),
      .groups = "drop"
    )
}
summary_ckd <- calculate_summary(Full_NP, "CKD_NC2")
print(summary_ckd)
summary_albuminuria <- calculate_summary(Full_NP, "Microalbuminuria_NC2")
print(summary_albuminuria)
summary_eGFR <- calculate_summary(Full_NP, "Lower_eGFR_NC2")
print(summary_eGFR)

#########
#T-tests#
#########
biomarkers <- c("ELP_3_HP_ng_ml", "C3M_HP_ng_ml", "PRO_C16_HP_ng_ml", 
                "PRO_C4_HP_ng_ml", "VICM_HP_ng_ml", "PRO_C3_roHP_ng_ml")

#T-tests CKD longitudinal
p_values <- c(
  t.test(ELP_3_HP_ng_ml ~ CKD_Status, data = Full_NP)$p.value,
  t.test(C3M_HP_ng_ml ~ CKD_Status, data = Full_NP)$p.value,
  t.test(PRO_C16_HP_ng_ml ~ CKD_Status, data = Full_NP)$p.value,
  t.test(PRO_C4_HP_ng_ml ~ CKD_Status, data = Full_NP)$p.value,
  t.test(VICM_HP_ng_ml ~ CKD_Status, data = Full_NP)$p.value,
  t.test(PRO_C3_roHP_ng_ml ~ CKD_Status, data = Full_NP)$p.value
)
names(p_values) <- biomarkers

#FDR
adjusted_p <- p.adjust(p_values, method = "fdr")
data.frame(
  Biomarker = names(adjusted_p),
  Raw_P = p_values,
  FDR_P = adjusted_p
)

#T-tests CKD cross-sectional
p_values <- c(
  t.test(ELP_3_HP_ng_ml ~ CKD_NC2, data = Full_NP)$p.value,
  t.test(C3M_HP_ng_ml ~ CKD_NC2, data = Full_NP)$p.value,
  t.test(PRO_C16_HP_ng_ml ~ CKD_NC2, data = Full_NP)$p.value,
  t.test(PRO_C4_HP_ng_ml ~ CKD_NC2, data = Full_NP)$p.value,
  t.test(VICM_HP_ng_ml ~ CKD_NC2, data = Full_NP)$p.value,
  t.test(PRO_C3_roHP_ng_ml ~ CKD_NC2, data = Full_NP)$p.value
)

names(p_values) <- biomarkers
#FDR
adjusted_p <- p.adjust(p_values, method = "fdr")
data.frame(
  Biomarker = names(adjusted_p),
  Raw_P = p_values,
  FDR_P = adjusted_p
)

#T-tests eGFR (</>= 60)
p_values <- c(
  t.test(ELP_3_HP_ng_ml ~ Lower_eGFR_NC2, data = Full_NP)$p.value,
  t.test(C3M_HP_ng_ml ~ Lower_eGFR_NC2, data = Full_NP)$p.value,
  t.test(PRO_C16_HP_ng_ml ~ Lower_eGFR_NC2, data = Full_NP)$p.value,
  t.test(PRO_C4_HP_ng_ml ~ Lower_eGFR_NC2, data = Full_NP)$p.value,
  t.test(VICM_HP_ng_ml ~ Lower_eGFR_NC2, data = Full_NP)$p.value,
  t.test(PRO_C3_roHP_ng_ml ~ Lower_eGFR_NC2, data = Full_NP)$p.value
)

names(p_values) <- biomarkers
#FDR
adjusted_p <- p.adjust(p_values, method = "fdr")
data.frame(
  Biomarker = names(adjusted_p),
  Raw_P = p_values,
  FDR_P = adjusted_p
)

#T-tests Microalbuminuria
p_values <- c(
  t.test(ELP_3_HP_ng_ml ~ Microalbuminuria_NC2, data = Full_NP)$p.value,
  t.test(C3M_HP_ng_ml ~ Microalbuminuria_NC2, data = Full_NP)$p.value,
  t.test(PRO_C16_HP_ng_ml ~ Microalbuminuria_NC2, data = Full_NP)$p.value,
  t.test(PRO_C4_HP_ng_ml ~ Microalbuminuria_NC2, data = Full_NP)$p.value,
  t.test(VICM_HP_ng_ml ~ Microalbuminuria_NC2, data = Full_NP)$p.value,
  t.test(PRO_C3_roHP_ng_ml ~ Microalbuminuria_NC2, data = Full_NP)$p.value
)

names(p_values) <- biomarkers
#FDR
adjusted_p <- p.adjust(p_values, method = "fdr")
data.frame(
  Biomarker = names(adjusted_p),
  Raw_P = p_values,
  FDR_P = adjusted_p
)