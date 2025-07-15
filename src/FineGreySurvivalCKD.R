###########################
#Fine-Gray Competing Risks#
###########################
install.packages("cmprsk")
library(forestplot)  
library(grid) 
library(cmprsk)
#Subset for complete cases
complete_rows <- complete.cases(Full_NP[, c(
                                            "TimeToCKDComp", 
                                            "CKD_Status_DeathCompete",
                                            "AgeAtBaseline", "C3M_HP_ng_ml_scaled", 
                                            "ELP_3_HP_ng_ml_scaled", "PRO_C16_HP_ng_ml_scaled", 
                                            "PRO_C4_HP_ng_ml_scaled", "PRO_C3_roHP_ng_ml_scaled", 
                                            "VICM_HP_ng_ml_scaled", "SEX", "HYP_Status", 
                                            "Diab_Status", "WaistHip_NC2", "HYP_NC2",
                                            "DIAB_NC2",
                                            "Mean24hAlbumin_NC2", "eGFR_NC2")])
Full_NP_complete1 <- Full_NP[complete_rows, ]


###########################
##CKD #Similar to Cox###
######################
covariates <- model.matrix(~ C3M_HP_ng_ml_scaled + 
                             ELP_3_HP_ng_ml_scaled + PRO_C16_HP_ng_ml_scaled + 
                             PRO_C4_HP_ng_ml_scaled + PRO_C3_roHP_ng_ml_scaled + 
                             VICM_HP_ng_ml_scaled + AgeAtBaseline + 
                             SEX + Mean24hAlbumin_NC2 +
                             eGFR_NC2 + HYP_NC2 + DIAB_NC2, data = Full_NP_complete1)[,-1]
fg_model <- crr(
  ftime = Full_NP_complete1$TimeToCKDComp,  
  fstatus = Full_NP_complete1$CKD_Status_DeathCompete, 
  cov1 = covariates,            
  failcode = 1,                 
  cencode = 0,                  
  variance = TRUE,
  na.action = na.omit
)
summary(fg_model)

# Get subdistribution hazard ratios with 95% CIs
results <- data.frame(
  Variable = colnames(covariates),
  sHR = exp(fg_model$coef),
  CI_low = exp(fg_model$coef - 1.96*sqrt(diag(fg_model$var))),
  CI_high = exp(fg_model$coef + 1.96*sqrt(diag(fg_model$var))),
  p_value = 2*(1 - pnorm(abs(fg_model$coef/sqrt(diag(fg_model$var)))))
)
# FDR Adjust
results$FDR_p_value <- p.adjust(results$p_value, method = "fdr")


print(results)
readable_names <- c(
  "C3M",
  "ELP-3",
  "PRO-C16",
  "PRO-C4",
  "PRO-C3",
  "VICM",
  "Age (Baseline)",
  "Sex (Female)",
  "Albuminuria (baseline)",
  "eGFR (Baseline)",
  "Hypertension Positive (Baseline)",
  "Diabetes Positive (Baseline)"
)

# Align var names
results$Variable <- readable_names

#text for forest plot
tabletext <- cbind(
  c("Covariate", results$Variable),
  c("Subdistribution HR (95% CI)", paste0(
    sprintf("%.2f", results$sHR), 
    " (", sprintf("%.2f", results$CI_low), " - ", sprintf("%.2f", results$CI_high), ")"
  )),
  c("FDR-adjusted P", ifelse(results$FDR_p_value < 0.001, "<0.001", sprintf("%.2f", results$FDR_p_value)))
)

# axis limits, tick marks
x_axis_min <- min(results$CI_low) * 0.9
x_axis_max <- max(results$CI_high) * 1.1
x_ticks <- seq(0.5, x_axis_max, by = 0.5)
if (!1 %in% x_ticks) x_ticks <- sort(c(x_ticks, 1))

# Text formatting for headings and axis
txt_gp <- fpTxtGp(
  label = gpar(fontface = c("bold", rep("plain", nrow(results)))), 
  ticks = gpar(fontface = "plain"),
  xlab = gpar(fontface = "bold")
)

# Create the forest plot
forestplot(
  labeltext = tabletext,
  mean = c(NA, results$sHR),         
  lower = c(NA, results$CI_low),
  upper = c(NA, results$CI_high),
  zero = 1,                          
  boxsize = 0.2,
  lineheight = unit(1.5, "cm"),
  colgap = unit(0.5, "cm"),
  xlog = FALSE,
  xticks = x_ticks,
  txt_gp = txt_gp,
  col = fpColors(box = "black", line = "black", zero = "red"),
  title = "Forest Plot of Subdistribution Hazard Ratios (Fine-Gray Model)"
)