#### Sensitivity Analysis: Attrition Bias ####

library(dplyr)
Full_long2c <- Full_long2c %>%
  group_by(PK) %>%
  mutate(
    has_all_visits = all(1:4 %in% visit_rank), 
    Dropout = ifelse(has_all_visits, 0, 1)      
  ) %>%
  ungroup()

library(lme4)

# Model 4: C3M
model4_dropout <- lmer(
  eGFR ~ time_years*C3M_HP_ng_ml + time_years*Dropout + C3M_HP_ng_ml*Dropout + 
    AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
    (1 + time_years | PK), 
  data = Full_long2c,
  control = lmerControl(optimizer = "bobyqa")
)

# Model 5: ELP-3
model5_dropout <- lmer(
  eGFR ~ time_years*ELP_3_HP_ng_ml + time_years*Dropout + ELP_3_HP_ng_ml*Dropout + 
    AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
    (1 + time_years | PK), 
  data = Full_long2c,
  control = lmerControl(optimizer = "bobyqa")
)

# Model 6: PRO-C16
model6_dropout <- lmer(
  eGFR ~ time_years*PRO_C16_HP_ng_ml + time_years*Dropout + PRO_C16_HP_ng_ml*Dropout + 
    AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
    (1 + time_years | PK), 
  data = Full_long2c,
  control = lmerControl(optimizer = "bobyqa")
)

# Model 7: PRO-C4
model7_dropout <- lmer(
  eGFR ~ time_years*PRO_C4_HP_ng_ml + time_years*Dropout + PRO_C4_HP_ng_ml*Dropout + 
    AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
    (1 + time_years | PK), 
  data = Full_long2c,
  control = lmerControl(optimizer = "bobyqa")
)

# Model 8: PRO-C3
model8_dropout <- lmer(
  eGFR ~ time_years*PRO_C3_roHP_ng_ml + time_years*Dropout + PRO_C3_roHP_ng_ml*Dropout + 
    AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
    (1 + time_years | PK), 
  data = Full_long2c,
  control = lmerControl(optimizer = "bobyqa")
)

# Model 9: VICM
model9_dropout <- lmer(
  eGFR ~ time_years*VICM_HP_ng_ml + time_years*Dropout + VICM_HP_ng_ml*Dropout + 
    AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
    (1 + time_years | PK), 
  data = Full_long2c,
  control = lmerControl(optimizer = "bobyqa")
)

library(broom.mixed)

results <- list(
  C3M = tidy(model4_dropout, conf.int = TRUE),
  ELP3 = tidy(model5_dropout, conf.int = TRUE),
  PRO_C16 = tidy(model6_dropout, conf.int = TRUE),
  PRO_C4 = tidy(model7_dropout, conf.int = TRUE),
  PRO_C3 = tidy(model8_dropout, conf.int = TRUE),
  VICM = tidy(model9_dropout, conf.int = TRUE)
)

key_results <- lapply(results, function(x) {
  x %>% 
    filter(grepl("time_years.*biomarker|Dropout", term)) %>%
    select(term, estimate, conf.low, conf.high, p.value)
})
do.call(rbind, key_results)



write.csv(Full_NP_clean, "~/Library/CloudStorage/OneDrive-UMCG/Research/PREVEND_NB/Full_NP.csv", row.names = FALSE)