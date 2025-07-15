#######################################
# COX Regression: CKD Subdefinitions #
######################################

#Albuminuria
library(survival)
library(dplyr)
library(forestplot)

cox_model1 <- coxph(Surv(TimeToMicroalbuminuria, Microalbuminuria_Status) ~ 
                      C3M_HP_ng_ml_scaled + ELP_3_HP_ng_ml_scaled + PRO_C16_HP_ng_ml_scaled + 
                      PRO_C4_HP_ng_ml_scaled + PRO_C3_roHP_ng_ml_scaled + VICM_HP_ng_ml_scaled + 
                      SEX + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + DIAB_NC2 + HYP_NC2, data = Full_NP)

cox_summary <- summary(cox_model1)

readable_names <- c(
  "C3M", "ELP-3", "PRO-C16", "PRO-C4", "PRO-C3", "VICM",
  "Sex (Female)", "eGFR (Baseline)", "Albuminuria (baseline)" ,"Age", "Diabetes Positive", "Hypertension Positive"
)

forest_data <- data.frame(
  Variable = readable_names,
  HR = cox_summary$coefficients[, "exp(coef)"],       
  Lower_CI = cox_summary$conf.int[, "lower .95"],     
  Upper_CI = cox_summary$conf.int[, "upper .95"],     
  Pvalue = cox_summary$coefficients[, "Pr(>|z|)"]     
)

print("Extracted forest_data:")
print(forest_data)

# Prepare table text 
tabletext <- cbind(
  c("Covariate", forest_data$Variable),
  c("Hazard Ratio (95% CI)", paste0(sprintf("%.2f", forest_data$HR), 
                                    " (", sprintf("%.2f", forest_data$Lower_CI), 
                                    " - ", sprintf("%.2f", forest_data$Upper_CI), ")")),
  c("P-value", ifelse(forest_data$Pvalue < 0.001, "<0.001", sprintf("%.2f", forest_data$Pvalue)))
)

x_axis_min <- min(forest_data$Lower_CI) * 0.9  
x_axis_max <- max(forest_data$Upper_CI) * 1.1   
x_ticks <- seq(0.5, x_axis_max, by = 0.5)
if (!1 %in% x_ticks) {
  x_ticks <- sort(c(x_ticks, 1))              
}

txt_gp <- fpTxtGp(
  label = gpar(fontface = c("bold", rep("plain", nrow(forest_data)))), 
  ticks = gpar(fontface = "plain"),                                    
  xlab = gpar(fontface = "bold")                                      
)

# Create the zoomed-out forest plot including all covariates
forestplot(
  labeltext = tabletext,
  mean = c(NA, forest_data$HR),       
  lower = c(NA, forest_data$Lower_CI),
  upper = c(NA, forest_data$Upper_CI),
  zero = 1,                             
  boxsize = 0.2,                         
  lineheight = unit(1.5, "cm"),
  colgap = unit(0.5, "cm"),
  xlog = FALSE,                         
  xticks = x_ticks,                      
  txt_gp = txt_gp,                       
  col = fpColors(box = "black", line = "black", zero = "red"),
  title = "Forest Plot of Increased Albuminuria Hazard Ratios"
)

#######Impaired eGFR
library(survival)
library(dplyr)
library(forestplot)

cox_model2 <- coxph(Surv(TimeToLoweGFR, LoweGFR_Status) ~ 
                      C3M_HP_ng_ml_scaled + ELP_3_HP_ng_ml_scaled + PRO_C16_HP_ng_ml_scaled + 
                      PRO_C4_HP_ng_ml_scaled + PRO_C3_roHP_ng_ml_scaled + VICM_HP_ng_ml_scaled + 
                      SEX + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + DIAB_NC2 + HYP_NC2, data = Full_NP)

cox_summary <- summary(cox_model2)
readable_names <- c(
  "C3M", "ELP-3", "PRO-C16", "PRO-C4", "PRO-C3", "VICM",
  "Sex (Female)", "eGFR (Baseline)", "Albuminuria (baseline)" ,"Age", "Diabetes Positive", "Hypertension Positive"
)
forest_data <- data.frame(
  Variable = readable_names,
  HR = cox_summary$coefficients[, "exp(coef)"],      
  Lower_CI = cox_summary$conf.int[, "lower .95"],    
  Upper_CI = cox_summary$conf.int[, "upper .95"],     
  Pvalue = cox_summary$coefficients[, "Pr(>|z|)"]     
)

tabletext <- cbind(
  c("Covariate", forest_data$Variable),
  c("Hazard Ratio (95% CI)", paste0(sprintf("%.2f", forest_data$HR), 
                                    " (", sprintf("%.2f", forest_data$Lower_CI), 
                                    " - ", sprintf("%.2f", forest_data$Upper_CI), ")")),
  c("P-value", ifelse(forest_data$Pvalue < 0.001, "<0.001", sprintf("%.3f", forest_data$Pvalue)))
)

x_axis_min <- min(forest_data$Lower_CI) * 0.9   
x_axis_max <- max(forest_data$Upper_CI) * 1.1  
x_ticks <- seq(0.5, x_axis_max, by = 0.5)
if (!1 %in% x_ticks) {
  x_ticks <- sort(c(x_ticks, 1))            
}


txt_gp <- fpTxtGp(
  label = gpar(fontface = c("bold", rep("plain", nrow(forest_data)))), 
  ticks = gpar(fontface = "plain"),                                   
  xlab = gpar(fontface = "bold")                                      
)

# Create the zoomed-out forest plot including all covariates
forestplot(
  labeltext = tabletext,
  mean = c(NA, forest_data$HR),      
  lower = c(NA, forest_data$Lower_CI),
  upper = c(NA, forest_data$Upper_CI),
  zero = 1,                              
  boxsize = 0.2,                         
  lineheight = unit(1.5, "cm"),
  colgap = unit(0.5, "cm"),
  xlog = FALSE,                          
  xticks = x_ticks,                      
  txt_gp = txt_gp,                       
  col = fpColors(box = "black", line = "black", zero = "red"),
  title = "Forest Plot of Impaired eGFR Hazard Ratios"
)

