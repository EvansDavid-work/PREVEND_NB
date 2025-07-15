####################
#LMER PREPROCESSING#
####################

library(tidyr)
library(dplyr)

# Create long format df
Full_long2 <- Full_NP %>%
  select(-matches("^X\\d+$")) %>%  #Error handling
  filter(rowSums(is.na(.)) != ncol(.)) %>%  
  pivot_longer(
    cols = c(eGFR_NC2:eGFR_NC5, Middate_NC2:Middate_NC5, 
             Mean24hAlbumin_NC2:Mean24hAlbumin_NC5, WaistHip_NC2:WaistHip_NC5),
    names_to = c(".value", "Visit"),
    names_sep = "_NC"
  ) %>%
  mutate(
    Visit = as.numeric(Visit),
    Date = as.Date(Middate)
  )

Full_long2 <- Full_long2 %>%
  left_join(
    Full_NP %>% 
      select(PK, AgeAtBaseline, SEX, ANTIHYP_2B, ANTIDIAB_2B,
             C3M_HP_ng_ml, ELP_3_HP_ng_ml, PRO_C16_HP_ng_ml, 
             PRO_C4_HP_ng_ml, PRO_C3_roHP_ng_ml, VICM_HP_ng_ml),
    by = "PK"
  )

Full_long2 <- Full_long2 %>%
  group_by(PK) %>%
  mutate(
    first_visit_date = Date[Visit == 2],  
    time_years = as.numeric(difftime(Date, first_visit_date, units = "days")) / 365.25
  ) %>%
  ungroup()

Full_long2 <- Full_long2 %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x"))

numeric_cols <- c("C3M_HP_ng_ml", "ELP_3_HP_ng_ml", 
                  "PRO_C16_HP_ng_ml", "PRO_C4_HP_ng_ml", "PRO_C3_roHP_ng_ml", 
                  "VICM_HP_ng_ml", "Mean24hAlbumin")
Full_long2 <- Full_long2 %>%
  mutate(across(all_of(numeric_cols), scale))

Full_long2c <- Full_long2 %>%
  select(eGFR, time_years, PK, AgeAtBaseline, SEX, ANTIHYP_2B, ANTIDIAB_2B,
         C3M_HP_ng_ml, ELP_3_HP_ng_ml, PRO_C16_HP_ng_ml, PRO_C4_HP_ng_ml, 
         PRO_C3_roHP_ng_ml, VICM_HP_ng_ml, Mean24hAlbumin, WaistHip) %>%
  na.omit()

Full_long2c <- Full_long2c %>%
  group_by(PK) %>%
  arrange(PK, time_years) %>%               
  mutate(
    visit_rank = rank(time_years),          
    Visit = paste0("NC", visit_rank + 1)  # +1 to ensure visits start at 1
  ) %>%
  ungroup()

log_albumin_long <- Full_NP %>%
  select(PK, starts_with("log24hAlbumin_")) %>%
  pivot_longer(
    cols = -PK,
    names_to = "Visit",                     
    values_to = "log24halbumin",              
    names_prefix = "log24hAlbumin_"    
  )

Full_long2c <- Full_long2c %>%
  left_join(log_albumin_long, by = c("PK", "Visit"))

baseline_data <- Full_NP %>%
  select(PK, HYP_NC2, DIAB_NC2)

Full_long2c <- Full_long2c %>%
  left_join(baseline_data, by = "PK")

# check 
print(head(Full_long2c))
str(Full_long2c)

#################
##MIXED EFFECTS##
#################

#Handle Convergence warnings:
control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
install.packages("lme4")
library(lme4)
library(lmerTest)

# Set up control parameters for convergence
control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

# Omit rows where participants are missing data per NC
Full_long2c_complete <- na.omit(Full_long2c[, c("eGFR", "time_years", "AgeAtBaseline", 
                                                "SEX", "HYP_NC2", "DIAB_NC2", 
                                                "C3M_HP_ng_ml", "ELP_3_HP_ng_ml",
                                                "PRO_C16_HP_ng_ml", "PRO_C4_HP_ng_ml",
                                                "PRO_C3_roHP_ng_ml", "VICM_HP_ng_ml",
                                                "Mean24hAlbumin", "WaistHip", "PK")])

# Fit models
model1 <- lmer(eGFR ~ time_years + (1|PK), data = Full_long2c_complete, control = control)
model2 <- lmer(eGFR ~ time_years + (1 + time_years|PK), data = Full_long2c_complete, control = control)
model3 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 + 
                 (1 + time_years|PK), data = Full_long2c_complete, control = control)
model4 <- lmer(eGFR ~ AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 + 
                 time_years * C3M_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long2c_complete, control = control)
model5 <- lmer(eGFR ~  AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                 time_years * ELP_3_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long2c_complete, control = control)
model6 <- lmer(eGFR ~ AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                 time_years *  PRO_C16_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long2c_complete, control = control)
model7 <- lmer(eGFR ~  AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                 time_years * PRO_C4_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long2c_complete, control = control)
model8 <- lmer(eGFR ~ AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                 time_years * PRO_C3_roHP_ng_ml + 
                 (1 + time_years|PK), data = Full_long2c_complete, control = control)
model9 <- lmer(eGFR ~ AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                 time_years * VICM_HP_ng_ml + 
                 (1 + time_years|PK), data = Full_long2c_complete, control = control)
model10 <- lmer(eGFR ~ time_years + Mean24hAlbumin + WaistHip + 
                  (1 + time_years|PK), data = Full_long2c_complete, control = control)

# Compare all models 
all_models_comparison <- anova(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
print(all_models_comparison)

# Individual comparisons
c3m_comparison <- anova(model3, model4)
elp3_comparison <- anova(model3, model5)
pro_c16_comparison <- anova(model3, model6)
pro_c4_comparison <- anova(model3, model7)
pro_c3_comparison <- anova(model3, model8)
vicm_comparison <- anova(model3, model9)

# Print individual comparisons
print("C3M_HP_ng_ml comparison:")
print(c3m_comparison)
print("ELP_3_HP_ng_ml comparison:")
print(elp3_comparison)
print("PRO_C16_HP_ng_ml comparison:")
print(pro_c16_comparison)
print("PRO_C4_HP_ng_ml comparison:")
print(pro_c4_comparison)
print("PRO_C3_roHP_ng_ml comparison:")
print(pro_c3_comparison)
print("VICM_HP_ng_ml comparison:")
print(vicm_comparison)

# Model 4: C3M_HP_ng_ml
model4_interaction <- lmer(eGFR ~ time_years * C3M_HP_ng_ml + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                             (1 + time_years | PK), data = Full_long2c, na.action = na.omit)

# Model 5: ELP_3_HP_ng_ml
model5_interaction <- lmer(eGFR ~ time_years * ELP_3_HP_ng_ml + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                             (1 + time_years | PK), data = Full_long2c, na.action = na.omit)

# Model 6: PRO_C16_HP_ng_ml
model6_interaction <- lmer(eGFR ~ time_years * PRO_C16_HP_ng_ml + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                             (1 + time_years | PK),  data = Full_long2c, na.action = na.omit)

# Model 7: PRO_C4_HP_ng_ml
model7_interaction <- lmer(eGFR ~ time_years * PRO_C4_HP_ng_ml + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                             (1 + time_years | PK),  data = Full_long2c, na.action = na.omit)

# Model 8: PRO_C3_roHP_ng_ml
model8_interaction <- lmer(eGFR ~ time_years * PRO_C3_roHP_ng_ml + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                             (1 + time_years | PK),  data = Full_long2c, na.action = na.omit)

# Model 9: VICM_HP_ng_ml
model9_interaction <- lmer(eGFR ~ time_years * VICM_HP_ng_ml + AgeAtBaseline + SEX + HYP_NC2 + DIAB_NC2 +
                             (1 + time_years | PK), data = Full_long2c, na.action = na.omit)
summary(model4_interaction)
summary(model5_interaction)
summary(model6_interaction)
summary(model7_interaction)
summary(model8_interaction)
summary(model9_interaction)

get_biomarker_cis <- function(model, biomarker_name) {
  cis <- confint(model, method = "Wald", parm = "beta_")
  
  biomarker_terms <- c(biomarker_name, paste0("time_years:", biomarker_name))
  cis_filtered <- cis[rownames(cis) %in% biomarker_terms, ]
  
  return(cis_filtered)
}
cis_model4 <- get_biomarker_cis(model4_interaction, "C3M_HP_ng_ml")
cis_model5 <- get_biomarker_cis(model5_interaction, "ELP_3_HP_ng_ml")
cis_model6 <- get_biomarker_cis(model6_interaction, "PRO_C16_HP_ng_ml")
cis_model7 <- get_biomarker_cis(model7_interaction, "PRO_C4_HP_ng_ml")
cis_model8 <- get_biomarker_cis(model8_interaction, "PRO_C3_roHP_ng_ml")
cis_model9 <- get_biomarker_cis(model9_interaction, "VICM_HP_ng_ml")

# Tabulate
results <- rbind(
  "C3M_HP" = cis_model4,
  "ELP_3_HP" = cis_model5,
  "PRO_C16_HP" = cis_model6,
  "PRO_C4_HP" = cis_model7,
  "PRO_C3_roHP" = cis_model8,
  "VICM_HP" = cis_model9
)
print(results)

# FDR Adjust
install.packages("broom.mixed")
library(lme4)
library(broom.mixed)

model_names <- c(
  "model4_interaction",  # C3M_HP_ng_ml
  "model5_interaction",  # ELP_3_HP_ng_ml
  "model6_interaction",  # PRO_C16_HP_ng_ml
  "model7_interaction",  # PRO_C4_HP_ng_ml
  "model8_interaction",  # PRO_C3_roHP_ng_ml
  "model9_interaction"   # VICM_HP_ng_ml
)

biomarkers <- c(
  "C3M_HP_ng_ml",
  "ELP_3_HP_ng_ml",
  "PRO_C16_HP_ng_ml",
  "PRO_C4_HP_ng_ml",
  "PRO_C3_roHP_ng_ml",
  "VICM_HP_ng_ml"
)

get_p_values <- function(model, bio) {
  summ <- summary(model)
  main_p <- summ$coefficients[bio, "Pr(>|t|)"]
  int_p  <- summ$coefficients[paste0("time_years:", bio), "Pr(>|t|)"]
  data.frame(
    biomarker = bio,
    term = c("main", "interaction"),
    p_value = c(main_p, int_p)
  )
}

results <- lapply(seq_along(model_names), function(i) {
  model <- get(model_names[i])  
  get_p_values(model, biomarkers[i])
}) %>% bind_rows()

results$p_fdr <- p.adjust(results$p_value, method = "fdr")

print(results)

