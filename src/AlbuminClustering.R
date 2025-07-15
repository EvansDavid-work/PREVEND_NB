##################################
###### Albumin clustering ########
#################################
# Summary Stats
# Long format 
library(dplyr)

# Calculate overall log-albumin-rate and mean-log-albumin
summary_data <- Full_long2c %>%
  group_by(PK) %>% 
  summarize(
    baseline_albumin = log24halbumin[which.min(time_years)],  # Albumin at baseline 
    max_albumin = log24halbumin[which.max(time_years)],       # Albumin at latest NC
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
  left_join(Full_long2c[, c("PK", "C3M_HP_ng_ml", "ELP_3_HP_ng_ml", 
                            "PRO_C16_HP_ng_ml", "PRO_C4_HP_ng_ml", 
                            "PRO_C3_roHP_ng_ml", "VICM_HP_ng_ml")], by = "PK")

biomarker_summary <- cleaned_summary_data %>%
  group_by(Cluster) %>%
  summarize(
    mean_C3M = mean(C3M_HP_ng_ml, na.rm = TRUE),
    mean_ELP3 = mean(ELP_3_HP_ng_ml, na.rm = TRUE),
    mean_PROC16 = mean(PRO_C16_HP_ng_ml, na.rm = TRUE),
    mean_PROC4 = mean(PRO_C4_HP_ng_ml, na.rm = TRUE),
    mean_PROC3 = mean(PRO_C3_roHP_ng_ml, na.rm = TRUE),
    mean_VICM = mean(VICM_HP_ng_ml, na.rm = TRUE)
  )

print(biomarker_summary)

# Multinomial logistic regression to predict group membership
library(nnet)

# Fit multinomial logistic regression model
logistic_model <- multinom(Cluster ~ C3M_HP_ng_ml + ELP_3_HP_ng_ml +
                             PRO_C16_HP_ng_ml + PRO_C4_HP_ng_ml +
                             PRO_C3_roHP_ng_ml + VICM_HP_ng_ml, data = cleaned_summary_data)

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

# Print the updated table with FDR-adjusted p-values
print(tidy_model)

### Adjust for risk factors###
#Add Risk factors
cleaned_summary_data <- cleaned_summary_data %>%
  left_join(Full_NP[, c("PK", "AgeAtBaseline", "SEX", "WaistHip_NC2", 
                        "DIAB_NC2", "HYP_NC2", "eGFR_NC2")], by = "PK")

# Omit risk cells with NA
adjusted_model <- multinom(Cluster ~ C3M_HP_ng_ml + ELP_3_HP_ng_ml + PRO_C16_HP_ng_ml +
                             PRO_C4_HP_ng_ml + PRO_C3_roHP_ng_ml + VICM_HP_ng_ml +
                             AgeAtBaseline + SEX + WaistHip_NC2 + DIAB_NC2 + HYP_NC2 + eGFR_NC2,
                           data = cleaned_summary_data, na.action = na.omit)
print(adjusted_model)

library(dplyr)

# Adjust p-values within each cluster (y.level)
adjusted_tidy_model <- tidy(adjusted_model) %>%
  group_by(y.level) %>%
  mutate(fdr_adjusted_p = p.adjust(p.value, method = "fdr")) %>%
  ungroup()

# Print adjusted results
print(adjusted_tidy_model, n = 26)

trial_model <- multinom(Cluster ~ PRO_C16_HP_ng_ml , data = cleaned_summary_data)
summary(trial_model)
