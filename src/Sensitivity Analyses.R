#### Sensitivity Analysis ####
## LMER Attrition ##

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

# Extract key results
key_results <- lapply(results, function(x) {
  x %>% 
    filter(grepl("time_years.*biomarker|Dropout", term)) %>%
    select(term, estimate, conf.low, conf.high, p.value)
})

# Combine
do.call(rbind, key_results)

###############################################
# For LLoQ sensitivity analysis
##############################################

# 50% LLoQ values
Full_long2c$C3M_HP_ng_ml_50        <- ifelse(Full_long2c$C3M_HP_ng_ml        == 136,  136  * 0.50, Full_long2c$C3M_HP_ng_ml)
Full_long2c$PRO_C3_roHP_ng_ml_50   <- ifelse(Full_long2c$PRO_C3_roHP_ng_ml   == 20,   20   * 0.50, Full_long2c$PRO_C3_roHP_ng_ml)
Full_long2c$PRO_C4_HP_ng_ml_50     <- ifelse(Full_long2c$PRO_C4_HP_ng_ml     == 457.6,457.6* 0.50, Full_long2c$PRO_C4_HP_ng_ml)
Full_long2c$PRO_C16_HP_ng_ml_50    <- ifelse(Full_long2c$PRO_C16_HP_ng_ml    == 515,  515  * 0.50, Full_long2c$PRO_C16_HP_ng_ml)
Full_long2c$ELP_3_HP_ng_ml_50      <- ifelse(Full_long2c$ELP_3_HP_ng_ml      == 1179, 1179 * 0.50, Full_long2c$ELP_3_HP_ng_ml)
Full_long2c$VICM_HP_ng_ml_50       <- ifelse(Full_long2c$VICM_HP_ng_ml       == 8.3,  8.3  * 0.50, Full_long2c$VICM_HP_ng_ml)



log_albumin_long <- Full_NP %>%
  select(PK, starts_with("log24hAlbumin_")) %>%
  pivot_longer(
    cols = -PK,
    names_to = "Visit",                     
    values_to = "log24halbumin",              
    names_prefix = "log24hAlbumin_"    
  )

# Merge with Full_long2c
Full_long2c <- Full_long2c %>%
  left_join(log_albumin_long, by = c("PK", "Visit"))

# Calculate overall log-albumin-rate and mean-log-albumin
summary_data <- Full_long2c %>%
  group_by(PK) %>% 
  summarize(
    baseline_albumin = log24halbumin[which.min(time_years)],  
    max_albumin = log24halbumin[which.max(time_years)],       
    max_time = max(time_years, na.rm = TRUE),                
    log_albumin_rate = (max_albumin - baseline_albumin) / max_time,  
    mean_log_albumin = mean(log24halbumin, na.rm = TRUE)    
  )
Full_long2c <- Full_long2c %>%
  left_join(summary_data, by = "PK")

# Delete NaN, NA, INF - these are from only 1 visit or other weirdness
cleaned_summary_data <- summary_data %>%
  filter(!is.na(mean_log_albumin) & !is.na(log_albumin_rate))
# Elbow method to determine optimal k's
wss <- function(k) {
  result <- tryCatch({
    kmeans(cleaned_summary_data[, c("mean_log_albumin", "log_albumin_rate")], centers = k, nstart = 25)$tot.withinss
  }, error = function(e) NA)  # Return NA in case of error
  return(result)
}

k_values <- 2:10
wss_values <- sapply(k_values, wss)

# Plot valid WSS values
valid_indices <- !is.na(wss_values)
plot(k_values[valid_indices], wss_values[valid_indices], type = "b", pch = 19,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Cluster Sum of Squares") #  k = 3 appears to be best!

set.seed(123) 

# k-means clustering with k = 3
kmeans_result <- kmeans(cleaned_summary_data[, c("mean_log_albumin", "log_albumin_rate")], centers = 3, nstart = 25)

# Add cluster labels
cleaned_summary_data$Cluster <- kmeans_result$cluster
print(kmeans_result)

# Visualise clusters
library(factoextra)

fviz_cluster(kmeans_result, data = cleaned_summary_data[, c("mean_log_albumin", "log_albumin_rate")],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal())
library(dplyr)

# Summarize characteristics by cluster
cluster_summary <- cleaned_summary_data %>%
  group_by(Cluster) %>%
  summarize(
    mean_albumin = mean(mean_log_albumin),
    mean_rate = mean(log_albumin_rate),
    count = n()
  )

print(cluster_summary)

# Add Biomarkers to summary data
cleaned_summary_data <- cleaned_summary_data %>%
  left_join(Full_long2c[, c("PK", "C3M_HP_ng_ml_50", "ELP_3_HP_ng_ml_50", 
                            "PRO_C16_HP_ng_ml_50", "PRO_C4_HP_ng_ml_50", 
                            "PRO_C3_roHP_ng_ml_50", "VICM_HP_ng_ml_50")], by = "PK")


biomarker_summary <- cleaned_summary_data %>%
  group_by(Cluster) %>%
  summarize(
    mean_C3M_50 = mean(C3M_HP_ng_ml_50, na.rm = TRUE),
    mean_ELP3_50 = mean(ELP_3_HP_ng_ml_50, na.rm = TRUE),
    mean_PROC16_50 = mean(PRO_C16_HP_ng_ml_50, na.rm = TRUE),
    mean_PROC4_50 = mean(PRO_C4_HP_ng_ml_50, na.rm = TRUE),
    mean_PROC3_50 = mean(PRO_C3_roHP_ng_ml_50, na.rm = TRUE),
    mean_VICM_50 = mean(VICM_HP_ng_ml_50, na.rm = TRUE)
  )

print(biomarker_summary)

# Multinomial logistic regression to predict group membership
library(nnet)

# Fit multinomial logistic regression model
logistic_model <- multinom(Cluster ~ C3M_HP_ng_ml_50 + ELP_3_HP_ng_ml_50 +
                             PRO_C16_HP_ng_ml_50 + PRO_C4_HP_ng_ml_50 +
                             PRO_C3_roHP_ng_ml_50 + VICM_HP_ng_ml_50, data = cleaned_summary_data)

# Summarize the model
summary(logistic_model)

# Predict cluster membership for new data
predicted_clusters <- predict(logistic_model, newdata = cleaned_summary_data)
cleaned_summary_data$PredictedCluster <- predicted_clusters

# Calculate p values for logistic regression
library(broom)
tidy_model <- tidy(logistic_model)
tidy_model <- tidy_model %>%
  mutate(z_value = estimate / std.error,
         p_value = 2 * pnorm(-abs(z_value))) # Two-tailed test

print(tidy_model)
tidy_model <- tidy_model %>%
  mutate(fdr_adjusted_p = p.adjust(p_value, method = "fdr"))

print(tidy_model)

### Adjust for risk factors###
#Add Risk factors
cleaned_summary_data <- cleaned_summary_data %>%
  left_join(Full_NP[, c("PK", "AgeAtBaseline", "SEX", 
                        "DIAB_NC2", "HYP_NC2", "eGFR_NC2")], by = "PK")

# Omit risk cells with NA
adjusted_model <- multinom(Cluster ~ C3M_HP_ng_ml_50 + ELP_3_HP_ng_ml_50 + PRO_C16_HP_ng_ml_50 +
                             PRO_C4_HP_ng_ml_50 + PRO_C3_roHP_ng_ml_50 + VICM_HP_ng_ml_50 +
                             AgeAtBaseline + SEX + DIAB_NC2 + HYP_NC2 + eGFR_NC2,
                           data = cleaned_summary_data, na.action = na.omit)
print(adjusted_model)
