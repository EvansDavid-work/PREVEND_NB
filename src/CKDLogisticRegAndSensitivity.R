## Simple logistic reg
scaledbiomarkers <- c("C3M_HP_ng_ml_scaled", "ELP_3_HP_ng_ml_scaled", "PRO_C16_HP_ng_ml_scaled",
                      "PRO_C4_HP_ng_ml_scaled", "PRO_C3_roHP_ng_ml_scaled", "VICM_HP_ng_ml_scaled")

# Filter out prevalent CKD cases
Full_NP_filtered <- Full_NP %>%
  filter(TimeToCKD > 0)

# CKD Status
print("=== CKD_Status ===")
for (bm in scaledbiomarkers) {
  model <- glm(
    as.formula(paste("CKD_Status ~", bm, " + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2")),
    data = Full_NP_filtered, family = binomial
  )
  print(paste("===", bm, "==="))
  print(summary(model))
  print("---")
}

# SEnsitivity Analyses: CKD
biomarkers50 <- c("C3M_HP_ng_ml_50_z", "ELP_3_HP_ng_ml_50_z", "PRO_C16_HP_ng_ml_50_z",
                  "PRO_C4_HP_ng_ml_50_z", "PRO_C3_roHP_ng_ml_50_z", "VICM_HP_ng_ml_50_z")


# 50% LLOQ biomarkers
for (bm in biomarkers50) {
  zname <- paste0(bm, "_z")
  df_sensitivity[[zname]] <- as.vector(scale(df_sensitivity[[bm]]))
}
print("=== CKD_Status ===")
for (bm in biomarkers50) {
  model <- glm(
    as.formula(paste("CKD_Status ~", bm, " + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2")),
    data = df_sensitivity, family = binomial
  )
  print(paste("===", bm, "==="))
  print(summary(model))
  print("---")
}

# Sensitivity analysis microalbumin

print("=== microalbuminuria_Status ===")
for (bm in biomarkers50) {
  f <- as.formula(paste("Microalbuminuria_Status ~", bm,
                        "+ AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2"))
  model <- glm(f, data = df_sensitivity, family = binomial)
  
  cat("\n===", bm, "===\n")
  
  # Odds ratios
  OR <- exp(coef(model))
  
  # 95% CI (profile likelihood; use confint.default for Wald)
  CI <- exp(confint.default(model))  # or confint(model) if you prefer
  
  # Combine and print only the biomarker row
  res <- cbind(OR, CI)
  biom_row <- res[bm, , drop = FALSE]
  colnames(biom_row) <- c("OR", "CI_lower", "CI_upper")
  print(biom_row)
}
# CKD Status unadjusted
print("=== CKD_Status ===")
for (bm in scaledbiomarkers) {
  model <- lm(
    as.formula(paste("CKD_Status ~", bm)),
    data = Full_NP_filtered
  )
  print(paste("===", bm, "==="))
  print(summary(model))
  print("---")
}

# Microalbuminuria status
print("=== microalbuminuria_Status ===")
for (bm in scaledbiomarkers) {
  model <- glm(
    as.formula(paste("Microalbuminuria_Status ~", bm, " + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2")),
    data = Full_NP_filtered, family = binomial
  )
  print(paste("===", bm, "==="))
  print(summary(model))
  print("---")
}


pvals00 <- c(
  VICM  = 0.586,
  PROC3 = 0.459,
  PROC4 = 0.729,
  PROC16 = 0.0299,
  ELP3  = 0.126,
  C3M   = 0.290
)
p.adjust(pvals00, method = "BH")

pvals01 <- c(
  PROC3 = 0.459,
  PROC16 = 0.0299,
  ELP3  = 0.126
)
p.adjust(pvals01, method = "fdr")

# Microalbuminuria status unadjusted
print("=== microalbuminuria_Status ===")
for (bm in scaledbiomarkers) {
  model <- lm(
    as.formula(paste("Microalbuminuria_Status ~", bm)),
    data = Full_NP_filtered
  )
  print(paste("===", bm, "==="))
  print(summary(model))
  print("---")
}
# Lower eGFR status
print("=== Lower_eGFR_Status ===")
for (bm in scaledbiomarkers) {
  model <- glm(
    as.formula(paste("LoweGFR_Status ~", bm, " + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2")),
    data = Full_NP_filtered, family = binomial
  )
  print(paste("===", bm, "==="))
  print(summary(model))
  print("---")
}

model <- glm(
  LoweGFR_Status ~ PRO_C16_HP_ng_ml_scaled + AgeAtBaseline +
    SEX + HYP_NC2 + DIAB_NC2,
  data = Full_NP_filtered,
  family = binomial
)

# Odds ratio and 95% CI for PRO-C16 (per 1 SD)
or_ci <- exp(cbind(
  OR  = coef(model)["PRO_C16_HP_ng_ml_scaled"],
  confint(model, "PRO_C16_HP_ng_ml_scaled")
))

# Macroalbuminuria status
print("=== Macroalbuminuria_Status ===")
for (bm in scaledbiomarkers) {
  model <- lm(
    as.formula(paste("Macroalbuminuria_Status ~", bm, " + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2")),
    data = Full_NP_filtered
  )
  print(paste("===", bm, "==="))
  print(summary(model))
  print("---")
}