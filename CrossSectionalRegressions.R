#####################
# Linear Regressions#
####################
## Baseline Regressions
# eGFR as Dependent 

# Linear Regressions at Baseline - Simple Models
summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm((lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit)))

summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm((lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit)))

summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

#FDR Correction:
p_values <- c(
  summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["C3M_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["ELP_3_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["PRO_C16_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["PRO_C4_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["PRO_C3_roHP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["VICM_HP_ng_ml_scaled", "Pr(>|t|)"]
)

fdr_p_values <- p.adjust(p_values, method = "fdr")

data.frame(
  Biomarker = c("C3M", "ELP_3", "PRO_C16", "PRO_C4", "PRO_C3", "VICM"),
  Original_P_Value = p_values,
  FDR_P_Value = fdr_p_values
)

# Linear Regressions at Baseline - Simple Models + Age and Sex
summary(lm(eGFR_NC2 ~ SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit)) #Base model

summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))

#FDR correction
p_values <- c(
  summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))$coefficients["C3M_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))$coefficients["ELP_3_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))$coefficients["PRO_C16_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))$coefficients["PRO_C4_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))$coefficients["PRO_C3_roHP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline, data = Full_NP, na.action = na.omit))$coefficients["VICM_HP_ng_ml_scaled", "Pr(>|t|)"]
)
fdr_p_values <- p.adjust(p_values, method = "fdr")
results <- data.frame(
  Biomarker = c("C3M", "ELP", "PRO_C16", "PRO_C4", "PRO_C3", "VICM"),
  Original_P_Value = p_values,
  FDR_P_Value = fdr_p_values
)
print(results)

# Linear Regressions at Baseline - Sophisticated Models
summary(lm(eGFR_NC2 ~ Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
             DIAB_NC2, data = Full_NP)) # Base model

summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
             DIAB_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
                DIAB_NC2, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
             DIAB_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
                DIAB_NC2, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
             DIAB_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
                DIAB_NC2, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
             DIAB_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
                DIAB_NC2, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
             DIAB_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
                DIAB_NC2, data = Full_NP, na.action = na.omit))

summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
             DIAB_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + 
                DIAB_NC2, data = Full_NP, na.action = na.omit))

#FDR correction
# Extract p-values for biomarker coefficients from models
p_values <- c(
  summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2, data = Full_NP, na.action = na.omit))$coefficients["C3M_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2, data = Full_NP, na.action = na.omit))$coefficients["ELP_3_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2, data = Full_NP, na.action = na.omit))$coefficients["PRO_C16_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2, data = Full_NP, na.action = na.omit))$coefficients["PRO_C4_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2, data = Full_NP, na.action = na.omit))$coefficients["PRO_C3_roHP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml_scaled + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2, data = Full_NP, na.action = na.omit))$coefficients["VICM_HP_ng_ml_scaled", "Pr(>|t|)"]
)
fdr_p_values <- p.adjust(p_values, method = "fdr")
results <- data.frame(
  Biomarker = c("C3M_HP", "ELP_3_HP", "PRO_C16_HP", "PRO_C4_HP", "PRO_C3_roHP", "VICM_HP"),
  Original_P_Value = p_values,
  FDR_P_Value = fdr_p_values
)
print(results)

# Albuminuria as Dependant 
summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))

# FDR Correction
p_values <- c(
  summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["C3M_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["ELP_3_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["PRO_C16_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["PRO_C4_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["PRO_C3_roHP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled, data = Full_NP, na.action = na.omit))$coefficients["VICM_HP_ng_ml_scaled", "Pr(>|t|)"]
)
fdr_p_values <- p.adjust(p_values, method = "fdr")
results <- data.frame(
  Biomarker = c("C3M", "ELP_3", "PRO_C16", "PRO_C4", "PRO_C3", "VICM"),
  Original_P_Value = p_values,
  FDR_P_Value = fdr_p_values
)
print(results)

# Simple Regressions + Age and Sex
summary(lm(Mean24hAlbumin_NC2 ~ SEX + AgeAtBaseline, 
           data = Full_NP)) #Base model
summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
           data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
              data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
           data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
              data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
           data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
              data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
           data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
              data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline, 
           data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline, 
              data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
           data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
              data = Full_NP, na.action = na.omit))

# FDR Correction
p_values <- c(
  summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
             data = Full_NP, na.action = na.omit))$coefficients["C3M_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
             data = Full_NP, na.action = na.omit))$coefficients["ELP_3_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
             data = Full_NP, na.action = na.omit))$coefficients["PRO_C16_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
             data = Full_NP, na.action = na.omit))$coefficients["PRO_C4_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline, 
             data = Full_NP, na.action = na.omit))$coefficients["PRO_C3_roHP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline, 
             data = Full_NP, na.action = na.omit))$coefficients["VICM_HP_ng_ml_scaled", "Pr(>|t|)"]
)
fdr_p_values <- p.adjust(p_values, method = "fdr")
results <- data.frame(
  Biomarker = c("C3M", "ELP_3", "PRO_C16", "PRO_C4", "PRO_C3", "VICM"),
  Original_P_Value = p_values,
  FDR_P_Value = fdr_p_values
)
print(results)

# Sophisticated Models
#Albumin as Dependent
summary(lm(Mean24hAlbumin_NC2 ~ eGFR_NC2 + SEX + AgeAtBaseline + HYP_NC2 + 
             DIAB_NC2, 
           data = Full_NP, na.action = na.omit)) #Base model
summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
             HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
                HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
             HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
                HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
             HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
                HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
             HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
                HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline + 
             HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline + 
                HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))

summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
             HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))
confint.lm(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline + 
                HYP_NC2 + DIAB_NC2 + eGFR_NC2, data = Full_NP, na.action = na.omit))

# FDR Correction
p_values <- c(
  summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml_scaled + SEX + AgeAtBaseline + HYP_NC2 + DIAB_NC2 + eGFR_NC2, 
             data = Full_NP, na.action = na.omit))$coefficients["C3M_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml_scaled + SEX + AgeAtBaseline + HYP_NC2 + DIAB_NC2 + eGFR_NC2, 
             data = Full_NP, na.action = na.omit))$coefficients["ELP_3_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml_scaled + SEX + AgeAtBaseline + HYP_NC2 + DIAB_NC2 + eGFR_NC2, 
             data = Full_NP, na.action = na.omit))$coefficients["PRO_C16_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml_scaled + SEX + AgeAtBaseline + HYP_NC2 + DIAB_NC2 + eGFR_NC2, 
             data = Full_NP, na.action = na.omit))$coefficients["PRO_C4_HP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml_scaled + SEX + AgeAtBaseline + HYP_NC2 + DIAB_NC2 + eGFR_NC2, 
             data = Full_NP, na.action = na.omit))$coefficients["PRO_C3_roHP_ng_ml_scaled", "Pr(>|t|)"],
  summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml_scaled + SEX + AgeAtBaseline + HYP_NC2 + DIAB_NC2 + eGFR_NC2, 
             data = Full_NP, na.action = na.omit))$coefficients["VICM_HP_ng_ml_scaled", "Pr(>|t|)"]
)
fdr_p_values <- p.adjust(p_values, method = "fdr")
results <- data.frame(
  Biomarker = c("C3M", "ELP", "PRO_C16", "PRO_C4", "PRO_C3", "VICM"),
  Original_P_Value = p_values,
  FDR_P_Value = fdr_p_values
)
print(results)