## PREPROCESSING ##

# Create Combined DF
install.packages('haven')
install.packages('dplyr')
library(haven)
library(dplyr)

nb <- read_sav("/Users/d.r.evans/Library/CloudStorage/OneDrive-UMCG/20250110000/nordic_bioscience_PREVEND.sav")
prevend <- read_sav("/Users/d.r.evans/Library/CloudStorage/OneDrive-UMCG/Research/0_PREVEND.sav")
Full_NP <- left_join(prevend, nb, by = "PK")

#Delete rows with no biomarker data
Full_NP <- Full_NP %>%
  filter(if_any(c(C3M_HP_ng_ml, ELP_3_HP_ng_ml, PRO_C16_HP_ng_ml, 
                  PRO_C4_HP_ng_ml, PRO_C3_roHP_ng_ml, VICM_HP_ng_ml), ~!is.na(.)))

# Create Baseline Age Variable
Full_NP$AgeAtBaseline <- as.numeric(Full_NP$DATEV1_2 - Full_NP$DOB) / 365.25	

# Create a 24 hour albumin excretion column for each NC (in mg)
Full_NP$Mean24hAlbumin_NC2 <- ((Full_NP$UAC1_2B * Full_NP$UVOL1_2B/1000) 
                               + (Full_NP$UAC2_2B * Full_NP$UVOL2_2B/1000))/2
Full_NP$Mean24hAlbumin_NC3 <- ((Full_NP$UAC1_3 * Full_NP$UVOL1_3/1000) 
                               + (Full_NP$UAC2_3 * Full_NP$UVOL2_3/1000))/2
Full_NP$Mean24hAlbumin_NC4 <- ((Full_NP$UAC1_4 * Full_NP$UVOL1_4/1000) 
                               + (Full_NP$UAC2_4 * Full_NP$UVOL2_4/1000))/2
Full_NP$Mean24hAlbumin_NC5 <- ((Full_NP$UAC1_5 * Full_NP$UVOL1_5/1000) 
                               + (Full_NP$UAC2_5 * Full_NP$UVOL2_5/1000))/2

# Macroalbuminuria Column
Full_NP$Macroalbuminuria_NC2 <- ifelse(Full_NP$Mean24hAlbumin_NC2 <300, 0, 1)

#Microalbuminuria Status
Full_NP$Microalbuminuria_Status = case_when(
  Full_NP$Mean24hAlbumin_NC2 >=30 | Full_NP$Mean24hAlbumin_NC3 >=30 | Full_NP$Mean24hAlbumin_NC4 >=30 | 
    Full_NP$Mean24hAlbumin_NC5 >=30 ~ 1,
  TRUE ~ 0)

#Microalbumuria groups at every NC
Full_NP$Microalbuminuria_NC2 <- ifelse(Full_NP$Mean24hAlbumin_NC2 >=30, 1, 0)
Full_NP$Microalbuminuria_NC3 <- ifelse(Full_NP$Mean24hAlbumin_NC3 >=30, 1, 0)
Full_NP$Microalbuminuria_NC4 <- ifelse(Full_NP$Mean24hAlbumin_NC4 >=30, 1, 0)
Full_NP$Microalbuminuria_NC5 <- ifelse(Full_NP$Mean24hAlbumin_NC5 >=30, 1, 0)

# Create Mean Urine Creatinine at Baseline
Full_NP$MeanUCreat_NC2 <- (Full_NP$UCREA1_2B + Full_NP$UCREA2_2B)/2 #?

# Create Waist-to-Hip Ratios for Each NC
Full_NP$WaistHip_NC2 <- Full_NP$WAIST_2B/Full_NP$HIP_2B
Full_NP$WaistHip_NC3 <- Full_NP$WAIST_3/Full_NP$HIP_3
Full_NP$WaistHip_NC4 <- Full_NP$WAIST_4/Full_NP$HIP_4
Full_NP$WaistHip_NC5 <- Full_NP$WAIST_5/Full_NP$HIP_5

# Create BMI (in Kg.m^-2)
Full_NP$BMI <- Full_NP$WEIGH_2B/((Full_NP$LENGT_2B/100)*(Full_NP$LENGT_2B/100))

# <60 eGFR Column at Baseline (LoweGFR Variable)
Full_NP$LoweGFR_NC2 <- ifelse(Full_NP$eGFR_NC2 < 60, 1, 0)

#Create CKD_Status for Each NC (composite: positive if eGFR < 60 or 24h albumin >= 30)
Full_NP$CKD_NC2 <- ifelse(Full_NP$eGFR_NC2 < 60 | 
                            Full_NP$Mean24hAlbumin_NC2 >=30, 1, 0)
Full_NP$CKD_NC3 <- ifelse(Full_NP$eGFR_NC3 < 60 | 
                            Full_NP$Mean24hAlbumin_NC3 >=30, 1, 0)
Full_NP$CKD_NC4 <- ifelse(Full_NP$eGFR_NC4 < 60 | 
                            Full_NP$Mean24hAlbumin_NC4 >=30, 1, 0)
Full_NP$CKD_NC5 <- ifelse(Full_NP$eGFR_NC5 < 60 | 
                            Full_NP$Mean24hAlbumin_NC5 >=30, 1, 0)

# Create Death_Status Column
Full_NP$Death_status <- ifelse(is.na(Full_NP$DEAD_2017), 0, 1)

# Find "Average" Visit Date
Full_NP$Middate_NC2 <- as.Date(Full_NP$DATEV1_2 + 
                                 (Full_NP$DATEV2_2 - Full_NP$DATEV1_2)/2)
Full_NP$Middate_NC3 <- as.Date(Full_NP$DATEV1_3 + 
                                 (Full_NP$DATEV2_3 - Full_NP$DATEV1_3)/2)
Full_NP$Middate_NC4 <- as.Date(Full_NP$DATEV1_4 + 
                                 (Full_NP$DATEV2_4 - Full_NP$DATEV1_4)/2)
Full_NP$Middate_NC5 <- as.Date(Full_NP$DATEV1_5 + 
                                 (Full_NP$DATEV2_5 - Full_NP$DATEV1_5)/2)

# Calculate Time to Death (from first visit)
Full_NP$TimeToDeath <- Full_NP$DEAD_2017 - Full_NP$DATEV1_2
# If not dead, give time between first and last visit as TimeToDeat
Full_NP$TimeToDeath <- ifelse(is.na(Full_NP$TimeToDeath), 
                              Full_NP$LCONTDT_2017 - Full_NP$DATEV1_2, 
                              Full_NP$TimeToDeath)

# Median Imputed Time to Events 
# Calculate Time to CKD (from first visit)
Full_NP <- Full_NP %>%
  mutate(
    TimeToCKD = case_when(
      CKD_NC2 == 1 ~ 0,
      CKD_NC3 == 1 ~ as.numeric((as.numeric(Middate_NC2) + as.numeric(Middate_NC3)) / 2 - as.numeric(DATEV1_2)),
      CKD_NC4 == 1 ~ as.numeric((as.numeric(Middate_NC3) + as.numeric(Middate_NC4)) / 2 - as.numeric(DATEV1_2)),
      CKD_NC5 == 1 ~ as.numeric((as.numeric(Middate_NC4) + as.numeric(Middate_NC5)) / 2 - as.numeric(DATEV1_2)),
      TRUE ~ as.numeric(ifelse(is.na(Middate_NC5), as.numeric(LCONTDT_2017) - as.numeric(DATEV1_2), as.numeric(Middate_NC5) - as.numeric(DATEV1_2)))
    ),
    CKD_Status = case_when(
      CKD_NC2 == 1 | CKD_NC3 == 1 | CKD_NC4 == 1 | CKD_NC5 == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Calculate Time to microalbuminuria (from first visit)
Full_NP <- Full_NP %>%
  mutate(
    TimeToMicroalbuminuria = case_when(
      Microalbuminuria_NC2 == 1 ~ 0,
      Microalbuminuria_NC3 == 1 ~ as.numeric((as.numeric(Middate_NC2) + as.numeric(Middate_NC3)) / 2 - as.numeric(DATEV1_2)),
      Microalbuminuria_NC4 == 1 ~ as.numeric((as.numeric(Middate_NC3) + as.numeric(Middate_NC4)) / 2 - as.numeric(DATEV1_2)),
      Microalbuminuria_NC5 == 1 ~ as.numeric((as.numeric(Middate_NC4) + as.numeric(Middate_NC5)) / 2 - as.numeric(DATEV1_2)),
      TRUE ~ as.numeric(ifelse(is.na(Middate_NC5), as.numeric(LCONTDT_2017) - as.numeric(DATEV1_2), as.numeric(Middate_NC5) - as.numeric(DATEV1_2)))
    ),
    Microalbuminuria_Status = case_when(
      Microalbuminuria_NC2 == 1 | Microalbuminuria_NC3 == 1 | Microalbuminuria_NC4 == 1 | Microalbuminuria_NC5 == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Calculate Time to Impaired eGFR (from first visit, median imputation)
Full_NP <- Full_NP %>%
  mutate(
    TimeToLoweGFR = case_when(
      LoweGFR_NC2 == 1 ~ 0,
      LoweGFR_NC3 == 1 ~ as.numeric((as.numeric(Middate_NC2) + as.numeric(Middate_NC3)) / 2 - as.numeric(DATEV1_2)),
      LoweGFR_NC4 == 1 ~ as.numeric((as.numeric(Middate_NC3) + as.numeric(Middate_NC4)) / 2 - as.numeric(DATEV1_2)),
      LoweGFR_NC5 == 1 ~ as.numeric((as.numeric(Middate_NC4) + as.numeric(Middate_NC5)) / 2 - as.numeric(DATEV1_2)),
      TRUE ~ as.numeric(ifelse(is.na(Middate_NC5), as.numeric(LCONTDT_2017) - as.numeric(DATEV1_2), as.numeric(Middate_NC5) - as.numeric(DATEV1_2)))
    ),
    LoweGFR_Status = case_when(
      LoweGFR_NC2 == 1 | LoweGFR_NC3 == 1 | LoweGFR_NC4 == 1 | LoweGFR_NC5 == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Diabetes Status at Each NC
# Diabetes at NC2
Full_NP <- Full_NP %>%
  mutate(
    EAT_2B = as.character(EAT_2B),
    DRINK_2B = as.character(DRINK_2B),
    ANTIDIAB_2B = as.character(ANTIDIAB_2B),  
    fasting_nc2 = (GLUC_2 >= 7.0 & EAT_2B == "max 1 cracker" & DRINK_2B == "max 1 cup of tea"),
    non_fasting_nc2 = (GLUC_2 >= 11.0 & (EAT_2B != "max 1 cracker" | DRINK_2B != "max 1 cup of tea")),
    diab_med_use_nc2 = (ANTIDIAB_2B == "Yes"),
    DIAB_NC2 = case_when(
      is.na(GLUC_2) | is.na(EAT_2B) | is.na(DRINK_2B) | is.na(ANTIDIAB_2B) ~ NA_real_,
      fasting_nc2 | non_fasting_nc2 | diab_med_use_nc2 ~ 1,
      TRUE ~ 0
    )
  )


# Diabetes at NC3
Full_NP <- Full_NP %>%
  mutate(
    ANTIDIAB_3 = as.character(ANTIDIAB_3),
    FAST_3 = as.character(FAST_3),
    fasting_nc3 = (!is.na(GLUC_3) & GLUC_3 >= 7.0 & FAST_3 == "Yes"),
    non_fasting_nc3 = (!is.na(GLUC_3) & GLUC_3 >= 11.0 & FAST_3 != "Yes"),
    diab_med_use_nc3 = (ANTIDIAB_3 == "Yes"),
    DIAB_NC3 = case_when(
      is.na(GLUC_3) | is.na(FAST_3) | is.na(ANTIDIAB_3) ~ NA_real_,
      fasting_nc3 | non_fasting_nc3 | diab_med_use_nc3 ~ 1,
      TRUE ~ 0
    )
  )

# Diabetes at NC4
Full_NP <- Full_NP %>%
  mutate(
    ANTIDIAB_4 = as.character(ANTIDIAB_4),
    FAST_4 = as.character(FAST_4),
    fasting_nc4 = (!is.na(GLUC_4) & GLUC_4 >= 7.0 & FAST_4 == "Yes"),
    non_fasting_nc4 = (!is.na(GLUC_4) & GLUC_4 >= 11.0 & FAST_4 != "Yes"),
    diab_med_use_nc4 = (ANTIDIAB_4 == "Yes"),
    DIAB_NC4 = case_when(
      is.na(GLUC_4) | is.na(FAST_4) | is.na(ANTIDIAB_4) ~ NA_real_,
      fasting_nc4 | non_fasting_nc4 | diab_med_use_nc4 ~ 1,
      TRUE ~ 0
    )
  )

# Diabetes at NC5
Full_NP <- Full_NP %>%
  mutate(
    ANTIDIAB_5 = as.character(ANTIDIAB_5),
    FAST_5 = as.character(FAST_5),
    fasting_nc5 = (!is.na(GLUC_5) & GLUC_5 >= 7.0 & FAST_5 == "Yes"),
    non_fasting_nc5 = (!is.na(GLUC_5) & GLUC_5 >= 11.0 & FAST_5 != "Yes"),
    diab_med_use_nc5 = (ANTIDIAB_5 == "Yes"),
    DIAB_NC5 = case_when(
      is.na(GLUC_5) | is.na(FAST_5) | is.na(ANTIDIAB_5) ~ NA_real_,
      fasting_nc5 | non_fasting_nc5 | diab_med_use_nc5 ~ 1,
      TRUE ~ 0
    )
  )

#Overall Diabetic Status (if daib positive at any NC)
Full_NP$Diab_Status = case_when(
  Full_NP$DIAB_NC2 == 1 | Full_NP$DIAB_NC3 == 1 | Full_NP$DIAB_NC4 == 1 | 
    Full_NP$DIAB_NC5 == 1 ~ 1,
  TRUE ~ 0
)

# Hypertension Status at Each NC
# Calculate hypertension status at each NC
calculate_hypertension <- function(sbp, dbp, antihyp) {
  case_when(
    is.na(sbp) | is.na(dbp) | is.na(antihyp) ~ NA_real_,  
    sbp >= 140 | dbp >= 90 ~ 1,                         
    antihyp == "Yes" ~ 1,                               
    TRUE ~ 0                                             
  )
}
Full_NP <- Full_NP %>%
  mutate(
    ANTIHYP_2B = as.character(ANTIHYP_2B),
    ANTIHYP_3  = as.character(ANTIHYP_3),
    ANTIHYP_4  = as.character(ANTIHYP_4),
    ANTIHYP_5  = as.character(ANTIHYP_5)
  )
Full_NP <- Full_NP %>%
  mutate(
    HYP_NC2 = calculate_hypertension(DSBP_2B, DDBP_2B, ANTIHYP_2B),
    HYP_NC3 = calculate_hypertension(DSBP_3, DDBP_3, ANTIHYP_3),
    HYP_NC4 = calculate_hypertension(SBP_4, DBP_4, ANTIHYP_4),
    HYP_NC5 = calculate_hypertension(SBP_5, DBP_5, ANTIHYP_5)
  )

# Overall Hypertension Status
Full_NP$HYP_Status = case_when(
  Full_NP$HYP_NC2 == 1 | Full_NP$HYP_NC3 == 1 | Full_NP$HYP_NC4 == 1 | 
    Full_NP$HYP_NC5 == 1 ~ 1,
  TRUE ~ 0
)
# CVD Status
Full_NP$CVD_Status <- ifelse(is.na(Full_NP$CVWLDE),0, 1)

# Low eGFR Status
Full_NP$LoweGFR_Status = case_when(
  Full_NP$eGFR_NC2 < 60 | Full_NP$eGFR_NC3 < 60 | Full_NP$eGFR_NC4 < 60 | 
    Full_NP$eGFR_NC5 < 60 ~ 1,
  TRUE ~ 0)

# Low eGFR groups at every NC
Full_NP$LoweGFR_NC2 <- ifelse(Full_NP$eGFR_NC2 <60, 1, 0)
Full_NP$LoweGFR_NC3 <- ifelse(Full_NP$eGFR_NC3 <60, 1, 0)
Full_NP$LoweGFR_NC4 <- ifelse(Full_NP$eGFR_NC4 <60, 1, 0)
Full_NP$LoweGFR_NC5 <- ifelse(Full_NP$eGFR_NC5 <60, 1, 0)

# Survival Analysis Composite Outcomes (with Mortality as Competing Risk)
# 0 = event free, 1 = EoI, 2 = death
# CKD
Full_NP$CKD_Status_DeathCompete <- with(Full_NP, case_when(
  CKD_NC2 == 1 & (is.na(DEAD_2017) | Middate_NC2 < DEAD_2017) ~ 1,
  CKD_NC3 == 1 & (is.na(DEAD_2017) | Middate_NC3 < DEAD_2017) ~ 1,
  CKD_NC4 == 1 & (is.na(DEAD_2017) | Middate_NC4 < DEAD_2017) ~ 1,
  CKD_NC5 == 1 & (is.na(DEAD_2017) | Middate_NC5 < DEAD_2017) ~ 1,
  !is.na(DEAD_2017) & (CKD_Status == 0) ~ 2, TRUE ~ 0
))
# Composite Time to Events
# CKD
Full_NP$TimeToCKDComp <- with(Full_NP, case_when(
  CKD_Status_DeathCompete == 0 ~ TimeToCKD,
  CKD_Status_DeathCompete == 1 ~ TimeToCKD,
  CKD_Status_DeathCompete == 2 ~ TimeToDeath
))

# Calculate Time From Baseline (years)
Full_NP$TimeNC3_FromBaseline <- as.numeric(Full_NP$Middate_NC3 - Full_NP$Middate_NC2)/365.25
Full_NP$TimeNC4_FromBaseline <- as.numeric(Full_NP$Middate_NC4 - Full_NP$Middate_NC2)/365.25
Full_NP$TimeNC5_FromBaseline <- as.numeric(Full_NP$Middate_NC5 - Full_NP$Middate_NC2)/365.25

# Scale biomarkers for when necessary
numeric_cols <- c("C3M_HP_ng_ml", "ELP_3_HP_ng_ml", 
                  "PRO_C16_HP_ng_ml", "PRO_C4_HP_ng_ml", "PRO_C3_roHP_ng_ml", 
                  "VICM_HP_ng_ml")
for (col in numeric_cols) {
  new_col_name <- paste0(col, "_scaled")
  Full_NP[[new_col_name]] <- scale(Full_NP[[col]])
}

# Create log Albumin for GBTM
Full_NP$log24hAlbumin_NC2 <- log(Full_NP$Mean24hAlbumin_NC2)
Full_NP$log24hAlbumin_NC3 <- log(Full_NP$Mean24hAlbumin_NC3)
Full_NP$log24hAlbumin_NC4 <- log(Full_NP$Mean24hAlbumin_NC4)
Full_NP$log24hAlbumin_NC5 <- log(Full_NP$Mean24hAlbumin_NC5)

#For Competing Risk Regressions, Create Composite Outcome Status
# 0 = event free, 1 = event of interest, 2 = death
#CKD
Full_NP$CKD_Status_DeathCompete <- with(Full_NP, case_when(
  CKD_NC2 == 1 & (is.na(DEAD_2017) | Middate_NC2 < DEAD_2017) ~ 1,
  CKD_NC3 == 1 & (is.na(DEAD_2017) | Middate_NC3 < DEAD_2017) ~ 1,
  CKD_NC4 == 1 & (is.na(DEAD_2017) | Middate_NC4 < DEAD_2017) ~ 1,
  CKD_NC5 == 1 & (is.na(DEAD_2017) | Middate_NC5 < DEAD_2017) ~ 1,
  !is.na(DEAD_2017) & (CKD_Status == 0) ~ 2, TRUE ~ 0
))
#Diab
Full_NP$Diab_Status_DeathCompete <- with(Full_NP, case_when(
  DIAB_NC2 == 1 & (is.na(DEAD_2017) | Middate_NC2 < DEAD_2017) ~ 1,
  DIAB_NC3 == 1 & (is.na(DEAD_2017) | Middate_NC3 < DEAD_2017) ~ 1,
  DIAB_NC4 == 1 & (is.na(DEAD_2017) | Middate_NC4 < DEAD_2017) ~ 1,
  DIAB_NC5 == 1 & (is.na(DEAD_2017) | Middate_NC5 < DEAD_2017) ~ 1,
  !is.na(DEAD_2017) & (Diab_Status == 0) ~ 2, TRUE ~ 0
))
#HYP
Full_NP$HYP_Status_DeathCompete <- with(Full_NP, case_when(
  HYP_NC2 == 1 & (is.na(DEAD_2017) | Middate_NC2 < DEAD_2017) ~ 1,
  HYP_NC3 == 1 & (is.na(DEAD_2017) | Middate_NC3 < DEAD_2017) ~ 1,
  HYP_NC4 == 1 & (is.na(DEAD_2017) | Middate_NC4 < DEAD_2017) ~ 1,
  HYP_NC5 == 1 & (is.na(DEAD_2017) | Middate_NC5 < DEAD_2017) ~ 1,
  !is.na(DEAD_2017) & (HYP_Status == 0) ~ 2, TRUE ~ 0
))
#CVD
Full_NP$CVD_Status_DeathCompete <- with(Full_NP, case_when(
  CVD_Status == 1 & (is.na(DEAD_2017) | (!is.na(CVD_DatePostNC2) & 
                                            CVD_DatePostNC2 <= DEAD_2017)) ~ 1,
  CVD_Status == 0 & !is.na(DEAD_2017) ~ 2,
  TRUE ~ 0
))

#Composite Time to Events
#CKD
Full_NP$TimeToCKDComp <- with(Full_NP, case_when(
  CKD_Status_DeathCompete == 0 ~ TimeToCKD,
  CKD_Status_DeathCompete == 1 ~ TimeToCKD,
  CKD_Status_DeathCompete == 2 ~ TimeToDeath
))

