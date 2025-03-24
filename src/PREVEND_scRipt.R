## PREPROCESSING ##
#               #
# Create a 24 hour albumin excretion column for each NC
Full_NP$Mean24hAlbumin_NC2 <- ((Full_NP$UAC1_2B * Full_NP$UVOL1_2B/1000) 
                               + (Full_NP$UAC2_2B * Full_NP$UVOL2_2B/1000))/2
Full_NP$Mean24hAlbumin_NC3 <- ((Full_NP$UAC1_3 * Full_NP$UVOL1_3/1000) 
                               + (Full_NP$UAC2_3 * Full_NP$UVOL2_3/1000))/2
Full_NP$Mean24hAlbumin_NC4 <- ((Full_NP$UAC1_4 * Full_NP$UVOL1_4/1000) 
                               + (Full_NP$UAC2_4 * Full_NP$UVOL2_4/1000))/2
Full_NP$Mean24hAlbumin_NC5 <- ((Full_NP$UAC1_5 * Full_NP$UVOL1_5/1000) 
                               + (Full_NP$UAC2_5 * Full_NP$UVOL2_5/1000))/2

# Create Waist-to-Hip Ratios for Each NC
Full_NP$WaistHip_NC2 <- Full_NP$WAIST_2/Full_NP$HIP_2
Full_NP$WaistHip_NC3 <- Full_NP$WAIST_3/Full_NP$HIP_3
Full_NP$WaistHip_NC4 <- Full_NP$WAIST_4/Full_NP$HIP_4
Full_NP$WaistHip_NC5 <- Full_NP$WAIST_5/Full_NP$HIP_5

#Create CKD_Status for Each NC (positive if eGFR < 60 or 24h albumin >= 30)
Full_NP$CKD_NC2 <- ifelse(Full_NP$eGFR_NC2 < 60 | 
                            Full_NP$Mean24hAlbumin_NC2 >=30, 1, 0)
Full_NP$CKD_NC3 <- ifelse(Full_NP$eGFR_NC3 < 60 | 
                            Full_NP$Mean24hAlbumin_NC3 >=30, 1, 0)
Full_NP$CKD_NC4 <- ifelse(Full_NP$eGFR_NC4 < 60 | 
                            Full_NP$Mean24hAlbumin_NC4 >=30, 1, 0)
Full_NP$CKD_NC5 <- ifelse(Full_NP$eGFR_NC5 < 60 | 
                            Full_NP$Mean24hAlbumin_NC5 >=30, 1, 0)

#Create Death_Status Column
Full_NP$Death_status <- ifelse(is.na(Full_NP$DEAD_2017), 0, 1)

#Convert Visit Dates to Standard Format
Full_NP$Date_NC2.1 <- as.Date(Full_NP$DATEV1_2/86400, origin = "1582-10-14")
Full_NP$Date_NC2.2 <- as.Date(Full_NP$DATEV2_2/86400, origin = "1582-10-14")
Full_NP$Date_NC3.1 <- as.Date(Full_NP$DATEV1_3/86400, origin = "1582-10-14")
Full_NP$Date_NC3.2 <- as.Date(Full_NP$DATEV2_3/86400, origin = "1582-10-14")
Full_NP$Date_NC4.1 <- as.Date(Full_NP$DATEV1_4/86400, origin = "1582-10-14")
Full_NP$Date_NC4.2 <- as.Date(Full_NP$DATEV2_4/86400, origin = "1582-10-14")
Full_NP$Date_NC5.1 <- as.Date(Full_NP$DATEV1_5/86400, origin = "1582-10-14")
Full_NP$Date_NC5.2 <- as.Date(Full_NP$DATEV2_5/86400, origin = "1582-10-14")

#Convert Death Date to Standard Format
Full_NP$Death_date <- as.Date(Full_NP$DEAD_2017/86400, origin = "1582-10-14",
                              na.rm = TRUE)
#Convert Last Contact Date to Standard Format:
Full_NP$LastContactDate <- as.Date(Full_NP$LCONTDT_2017/86400, origin = "1582-10-14")

#Find "Average" Visit Date
Full_NP$Middate_NC2 <- as.Date(Full_NP$Date_NC2.1 + 
                         (Full_NP$Date_NC2.2 - Full_NP$Date_NC2.1)/2)
Full_NP$Middate_NC3 <- as.Date(Full_NP$Date_NC3.1 + 
                         (Full_NP$Date_NC3.2 - Full_NP$Date_NC3.1)/2)
Full_NP$Middate_NC4 <- as.Date(Full_NP$Date_NC4.1 + 
                         (Full_NP$Date_NC4.2 - Full_NP$Date_NC4.1)/2)
Full_NP$Middate_NC5 <- as.Date(Full_NP$Date_NC5.1 + 
                         (Full_NP$Date_NC5.2 - Full_NP$Date_NC5.1)/2)

#Calculate Time to Death (from first visit)
Full_NP$TimeToDeath <- Full_NP$Death_date - Full_NP$Date_NC2.1
#If not dead, give time between first and last visit as "time to death"
Full_NP$TimeToDeath <- ifelse(is.na(Full_NP$TimeToDeath), 
                            Full_NP$LastContactDate - Full_NP$Date_NC2.1, 
                            Full_NP$TimeToDeath)

#Calculate Time to CKD (from first visit) and Create CKD_Status column
library(dplyr)
Full_NP <- Full_NP %>%
  mutate(
    TimeToCKD = case_when(
      CKD_NC2 == 1 ~ 0,
      CKD_NC3 == 1 ~ as.numeric(Middate_NC3 - Date_NC2.1),
      CKD_NC4 == 1 ~ as.numeric(Middate_NC4 - Date_NC2.1),
      CKD_NC5 == 1 ~ as.numeric(Middate_NC5 - Date_NC2.1),
      TRUE ~ as.numeric(LastContactDate - Date_NC2.1)  # Censored at NC5 if no CKD
    ),
    CKD_Status = case_when(
      CKD_NC2 == 1 | CKD_NC3 == 1 | CKD_NC4 == 1 | CKD_NC5 == 1 ~ 1,
      TRUE ~ 0
    )
  )

#Convert Cardiovascular Event Dates to Standard Format
Full_NP$CVD_Date <- as.Date(Full_NP$CVWLDDT/86400, origin = "1582-10-14",
                              na.rm = TRUE)
#Create CVD Date after Baseline (Patients who hitherto had no CVD events)
Full_NP <- Full_NP %>%
  mutate(
    CVD_DatePostNC2 = as.Date(ifelse(CVD_Date > Date_NC2.1, CVD_Date, NA), 
                              origin = "1970-01-01")
  )
#Create CVD Status Column
Full_NP$CVD_Status <- ifelse(is.na(Full_NP$CVWLDE),0, 1)


#Calculate Time to Cardiac Event
Full_NP$TimeToCVD <- ifelse(!is.na(Full_NP$CVD_DatePostNC2), 
                            (Full_NP$CVD_DatePostNC2 - Full_NP$Date_NC2.1), 
                            (Full_NP$LastContactDate - Full_NP$Date_NC2.1))

#Diabetes Status at Each NC
library(dplyr)

# Diabetes at NC2
Full_NP <- Full_NP %>%
  mutate(
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
    fasting_nc3 = (GLUC_3 >= 7.0 & FAST_3 == "Yes"),
    non_fasting_nc3 = (GLUC_3 >= 11.0 & FAST_3 != "Yes"),
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
    fasting_nc4 = (GLUC_4 >= 7.0 & FAST_4 == "Yes"),
    non_fasting_nc4 = (GLUC_4 >= 11.0 & FAST_4 != "Yes"),
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
    fasting_nc5 = (GLUC_5 >= 7.0 & FAST_5 == "Yes"),
    non_fasting_nc5 = (GLUC_5 >= 11.0 & FAST_5 != "Yes"),
    diab_med_use_nc5 = (ANTIDIAB_5 == "Yes"),
    DIAB_NC5 = case_when(
      is.na(GLUC_5) | is.na(FAST_5) | is.na(ANTIDIAB_5) ~ NA_real_,
      fasting_nc5 | non_fasting_nc5 | diab_med_use_nc5 ~ 1,
      TRUE ~ 0
    )
  )

########################################################################
    
# Linear Regressions at Baseline - Simple Models
summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml, data = Full_NP))
summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml, data = Full_NP))
summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml, data = Full_NP))
#Albumin as Dependent 
summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml, data = Full_NP))

# Linear Regressions at Baseline - Simple Models + Age and Sex
summary(lm(eGFR_NC2 ~ SEX + AgeAtBaseline, data = Full_NP)) #Base model
summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml + SEX, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
#Albumin as Dependent 
summary(lm(Mean24hAlbumin_NC2 ~ SEX + AgeAtBaseline, 
           data = Full_NP)) #Base model
summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))

# Linear Regressions at Baseline - Sophisticated Models
summary(lm(eGFR_NC2 ~ Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(eGFR_NC2 ~ C3M_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC2 ~ ELP_3_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C16_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C4_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC2 ~ PRO_C3_roHP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC2 ~ VICM_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
#NC3
summary(lm(eGFR_NC3 ~ Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(eGFR_NC3 ~ C3M_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ ELP_3_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ PRO_C16_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ PRO_C4_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ PRO_C3_roHP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ VICM_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
#NC4
summary(lm(eGFR_NC4 ~ Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(eGFR_NC4 ~ C3M_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ ELP_3_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ PRO_C16_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ PRO_C4_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ PRO_C3_roHP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ VICM_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
#NC5
summary(lm(eGFR_NC5 ~ Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(eGFR_NC5 ~ C3M_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ ELP_3_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ PRO_C16_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ PRO_C4_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ PRO_C3_roHP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ VICM_HP_ng_ml + eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
##With Interaction##
#NC3
summary(lm(eGFR_NC3 ~ Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(eGFR_NC3 ~ C3M_HP_ng_ml* eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ ELP_3_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ PRO_C16_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ PRO_C4_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ PRO_C3_roHP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC3 ~ VICM_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
#NC4
summary(lm(eGFR_NC4 ~ Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(eGFR_NC4 ~ C3M_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ ELP_3_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ PRO_C16_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ PRO_C4_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ PRO_C3_roHP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC4 ~ VICM_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
#NC5
summary(lm(eGFR_NC5 ~ Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(eGFR_NC5 ~ C3M_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ ELP_3_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ PRO_C16_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ PRO_C4_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ PRO_C3_roHP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
summary(lm(eGFR_NC5 ~ VICM_HP_ng_ml * eGFR_NC2 + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + 
             ANTIDIAB_2B, data = Full_NP))
#Albumin as Dependent
summary(lm(Mean24hAlbumin_NC2 ~ eGFR_NC2 + SEX + AgeAtBaseline + ANTIHYP_2B + 
             ANTIDIAB_2B, 
           data = Full_NP)) #Base model
summary(lm(Mean24hAlbumin_NC2 ~ C3M_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ ELP_3_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC2 ~ VICM_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
#NC3
summary(lm(Mean24hAlbumin_NC3 ~ eGFR_NC2 + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP)) #Base model
summary(lm(Mean24hAlbumin_NC3 ~ C3M_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC3 ~ ELP_3_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC3 ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC3 ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC3 ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC3 ~ VICM_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
#NC4
summary(lm(Mean24hAlbumin_NC4 ~ eGFR_NC2 + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP)) #Base model
summary(lm(Mean24hAlbumin_NC4 ~ C3M_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC4 ~ ELP_3_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC4 ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC4 ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC4 ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC4 ~ VICM_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
#NC5
summary(lm(Mean24hAlbumin_NC5 ~ eGFR_NC2 + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP)) #Base model
summary(lm(Mean24hAlbumin_NC5 ~ C3M_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC5 ~ ELP_3_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC5 ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC5 ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC5 ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(Mean24hAlbumin_NC5 ~ VICM_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
#Scaling 24H Albumin Excretion
Full_NP$Mean24hAlbumin_NC2_scaled <- scale(Full_NP$Mean24hAlbumin_NC2)
#Rescaling to minimise range (-1 - 1)
Full_NP$Mean24hAlbumin_NC2_rescaled <- scales::rescale(Full_NP$Mean24hAlbumin_NC2_scaled, to = c(-1, 1))

#################
#Cox Regressions#
#################

library(survival) 
cox_model <- coxph(Surv(TimeToDeath, Death_status) ~  C3M_HP_ng_ml + 
ELP_3_HP_ng_ml + PRO_C16_HP_ng_ml +  PRO_C4_HP_ng_ml + PRO_C3_roHP_ng_ml + 
VICM_HP_ng_ml + Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + 
ANTIHYP2 + ANTIDIAB_2B, data=Full_NP) 
#View model summary 
summary(cox_model) 
#Check proportional hazards assumption 
test.ph <- cox.zph(cox_model) 
print(test.ph)
plot(test.ph) 
#Plot survival curves 
plot(survfit(cox_model), main="Survival Curves", xlab="Time", 
     ylab="Survival Probability") 
#CKD
library(survival) 
cox_model <- coxph(Surv(TimeToCKD, CKD_Status) ~  C3M_HP_ng_ml + 
                     ELP_3_HP_ng_ml + PRO_C16_HP_ng_ml +  PRO_C4_HP_ng_ml + PRO_C3_roHP_ng_ml + 
                     VICM_HP_ng_ml + SEX + Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + 
                     ANTIHYP2 + ANTIDIAB_2B, data=Full_NP) 
#View model summary 
summary(cox_model) 
#Check proportional hazards assumption 
test.ph <- cox.zph(cox_model) 
print(test.ph)
plot(test.ph) 
#Plot survival curves 
plot(survfit(cox_model), main="Survival Curves", xlab="Time", 
     ylab="Survival Probability") 

#CVD
#Filter out participants with pre-baseline CVD events
CVDsubsetdata <- Full_NP %>%
  filter(is.na(CVD_Date) | CVD_Date >= Date_NC2.1)

library(survival) 
cox_model <- coxph(Surv(TimeToCVD, CVD_Status) ~  C3M_HP_ng_ml + 
                     ELP_3_HP_ng_ml + PRO_C16_HP_ng_ml +  PRO_C4_HP_ng_ml + PRO_C3_roHP_ng_ml + 
                     VICM_HP_ng_ml + SEX + Mean24hAlbumin_NC2 + eGFR_NC2 + AgeAtBaseline + 
                     ANTIHYP2 + ANTIDIAB_2B, data=CVDsubsetdata)
#View model summary 
summary(cox_model) 
#Check proportional hazards assumption 
test.ph <- cox.zph(cox_model) 
print(test.ph)
plot(test.ph) 
#Plot survival curves 
plot(survfit(cox_model), main="Survival Curves", xlab="Time", 
     ylab="Survival Probability") 

#+#+#+#+#+#+#+#+#+#+#
#Plot eGFR Change 
# Define custom functions
first_non_na <- function(x) {
  non_na <- x[!is.na(x)]
  if(length(non_na) > 0) return(non_na[1]) else return(NA)
}

last_non_na <- function(x) {
  non_na <- x[!is.na(x)]
  if(length(non_na) > 0) return(tail(non_na, 1)) else return(NA)
}

# Extract earliest and latest eGFR values and dates
Full_NP$earliest_eGFR <- apply(Full_NP[, c("eGFR_NC2", "eGFR_NC3", "eGFR_NC4", "eGFR_NC5")], 1, first_non_na)
Full_NP$latest_eGFR <- apply(Full_NP[, c("eGFR_NC2", "eGFR_NC3", "eGFR_NC4", "eGFR_NC5")], 1, last_non_na)
Full_NP$earliest_eGFR_date <- apply(Full_NP[, c("Middate_NC2", "Middate_NC3", "Middate_NC4", "Middate_NC5")], 1, first_non_na)
Full_NP$latest_eGFR_date <- apply(Full_NP[, c("Middate_NC2", "Middate_NC3", "Middate_NC4", "Middate_NC5")], 1, last_non_na)

# Convert to appropriate types
Full_NP$earliest_eGFR <- as.numeric(Full_NP$earliest_eGFR)
Full_NP$latest_eGFR <- as.numeric(Full_NP$latest_eGFR)
Full_NP$earliest_eGFR_date <- as.Date(Full_NP$earliest_eGFR_date)
Full_NP$latest_eGFR_date <- as.Date(Full_NP$latest_eGFR_date)

# Calculate overall eGFR rate (change per year)
Full_NP$overall_eGFR_rate <- with(Full_NP, {
  time_diff <- as.numeric(difftime(latest_eGFR_date, earliest_eGFR_date, units = "days")) / 365.25
  (latest_eGFR - earliest_eGFR) / time_diff
})

# Repeat the process for Albumin
Full_NP$earliest_Albumin <- apply(Full_NP[, c("Mean24hAlbumin_NC2", "Mean24hAlbumin_NC3", "Mean24hAlbumin_NC4", "Mean24hAlbumin_NC5")], 1, first_non_na)
Full_NP$latest_Albumin <- apply(Full_NP[, c("Mean24hAlbumin_NC2", "Mean24hAlbumin_NC3", "Mean24hAlbumin_NC4", "Mean24hAlbumin_NC5")], 1, last_non_na)

Full_NP$earliest_Albumin <- as.numeric(Full_NP$earliest_Albumin)
Full_NP$latest_Albumin <- as.numeric(Full_NP$latest_Albumin)

Full_NP$overall_Albumin_rate <- with(Full_NP, {
  time_diff <- as.numeric(difftime(latest_eGFR_date, earliest_eGFR_date, units = "days")) / 365.25
  (latest_Albumin - earliest_Albumin) / time_diff
})

# Handle potential Inf or NaN values
Full_NP$overall_eGFR_rate[is.infinite(Full_NP$overall_eGFR_rate) | is.nan(Full_NP$overall_eGFR_rate)] <- NA
Full_NP$overall_Albumin_rate[is.infinite(Full_NP$overall_Albumin_rate) | is.nan(Full_NP$overall_Albumin_rate)] <- NA

#Rate Regressions

# Linear Regressions at Baseline - Simple Models
summary(lm(overall_eGFR_rate ~ C3M_HP_ng_ml, data = Full_NP))
summary(lm(overall_eGFR_rate ~ ELP_3_HP_ng_ml, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C16_HP_ng_ml, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C4_HP_ng_ml, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C3_roHP_ng_ml, data = Full_NP))
summary(lm(overall_eGFR_rate ~ VICM_HP_ng_ml, data = Full_NP))
#Albumin as Dependent 
summary(lm(overall_Albumin_rate ~ C3M_HP_ng_ml, data = Full_NP))
summary(lm(overall_Albumin_rate ~ ELP_3_HP_ng_ml, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C16_HP_ng_ml, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C4_HP_ng_ml, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C3_roHP_ng_ml, data = Full_NP))
summary(lm(overall_Albumin_rate ~ VICM_HP_ng_ml, data = Full_NP))

# Linear Regressions at Baseline - Simple Models + Age and Sex
summary(lm(overall_eGFR_rate ~ SEX + AgeAtBaseline, data = Full_NP)) #Base model
summary(lm(overall_eGFR_rate ~ C3M_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(overall_eGFR_rate ~ ELP_3_HP_ng_ml + SEX, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
summary(lm(overall_eGFR_rate ~ VICM_HP_ng_ml + SEX + AgeAtBaseline, data = Full_NP))
#Albumin as Dependent 
summary(lm(overall_Albumin_rate ~ SEX + AgeAtBaseline, 
           data = Full_NP)) #Base model
summary(lm(overall_Albumin_rate ~ C3M_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(overall_Albumin_rate ~ ELP_3_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))
summary(lm(overall_Albumin_rate ~ VICM_HP_ng_ml + SEX + AgeAtBaseline, 
           data = Full_NP))

# Linear Regressions at Baseline - Sophisticated Models
summary(lm(overall_eGFR_rate ~ Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + eGFR_NC2 +
             ANTIDIAB_2B, data = Full_NP)) # Base model
summary(lm(overall_eGFR_rate ~ C3M_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + eGFR_NC2 +
             ANTIDIAB_2B, data = Full_NP))
summary(lm(overall_eGFR_rate ~ ELP_3_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + eGFR_NC2 +
             ANTIDIAB_2B, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C16_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + eGFR_NC2 +
             ANTIDIAB_2B, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C4_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + eGFR_NC2 +
             ANTIDIAB_2B, data = Full_NP))
summary(lm(overall_eGFR_rate ~ PRO_C3_roHP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + eGFR_NC2 +
             ANTIDIAB_2B, data = Full_NP))
summary(lm(overall_eGFR_rate ~ VICM_HP_ng_ml + Mean24hAlbumin_NC2 + AgeAtBaseline + SEX + ANTIHYP_2B + eGFR_NC2 +
             ANTIDIAB_2B, data = Full_NP))

#Albumin as Dependent
summary(lm(overall_Albumin_rate ~ eGFR_NC2 + AgeAtBaseline + ANTIHYP_2B + 
             ANTIDIAB_2B, 
           data = Full_NP)) #Base model
summary(lm(overall_Albumin_rate ~ C3M_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ ELP_3_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C16_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C4_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C3_roHP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ VICM_HP_ng_ml + SEX + AgeAtBaseline + 
             ANTIHYP_2B + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))

#Only significant Risks
#Albumin as Dependent
summary(lm(overall_Albumin_rate ~ eGFR_NC2 + 
             ANTIDIAB_2B, 
           data = Full_NP)) #Base model
summary(lm(overall_Albumin_rate ~ C3M_HP_ng_ml + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ ELP_3_HP_ng_ml + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C16_HP_ng_ml + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C4_HP_ng_ml + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ PRO_C3_roHP_ng_ml + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))
summary(lm(overall_Albumin_rate ~ VICM_HP_ng_ml + ANTIDIAB_2B + eGFR_NC2, data = Full_NP))

###############
###############
#Convert data to long format for Mixed Effects
library(tidyr)
library(dplyr)

# Step 1: Create the initial long format dataframe
Full_long <- Full_NP %>%
  # Remove any default counting column
  select(-matches("^X\\d+$")) %>%
  
  # Remove rows where all values are NA
  filter(rowSums(is.na(.)) != ncol(.)) %>%
  
  # Convert relevant columns to long format
  pivot_longer(
    cols = c(eGFR_NC2:eGFR_NC5, Middate_NC2:Middate_NC5, 
             Mean24hAlbumin_NC2:Mean24hAlbumin_NC5, WaistHip_NC2:WaistHip_NC5),
    names_to = c(".value", "Visit"),
    names_sep = "_NC"
  ) %>%
  
  # Convert Visit to numeric and ensure Date is in proper format
  mutate(
    Visit = as.numeric(Visit),
    Date = as.Date(Middate)
  )

# Step 2: Add baseline biomarkers and static covariates, excluding LastContactDate
Full_long <- Full_long %>%
  left_join(
    Full_NP %>% 
      select(PK, AgeAtBaseline, SEX, ANTIHYP_2B, ANTIDIAB_2B,
             C3M_HP_ng_ml, ELP_3_HP_ng_ml, PRO_C16_HP_ng_ml, 
             PRO_C4_HP_ng_ml, PRO_C3_roHP_ng_ml, VICM_HP_ng_ml),
    by = "PK"
  )

# Display the first few rows and structure of the new dataframe
print(head(Full_long))
print(str(Full_long))

#Add Time variable for each participant in long table
# Add Time variable for each participant in long table, ensuring NC2 is baseline
library(dplyr)

Full_long <- Full_long %>%
  group_by(PK) %>%
  mutate(
    first_visit_date = Date[Visit == 2],  # Force NC2 as baseline
    time_years = as.numeric(difftime(Date, first_visit_date, units = "days")) / 365.25
  ) %>%
  ungroup()

#Bugfixing
# 1. Clean up column names
Full_long <- Full_long %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x"))

# 2. Standardize numeric predictors
numeric_cols <- c("time_years", "AgeAtBaseline", "C3M_HP_ng_ml", "ELP_3_HP_ng_ml", 
                  "PRO_C16_HP_ng_ml", "PRO_C4_HP_ng_ml", "PRO_C3_roHP_ng_ml", 
                  "VICM_HP_ng_ml", "Mean24hAlbumin", "WaistHip")

Full_long <- Full_long %>%
  mutate(across(all_of(numeric_cols), scale))

# 3. Handle missing data
Full_long_complete <- Full_long %>%
  select(eGFR, time_years, PK, AgeAtBaseline, SEX, ANTIHYP_2B, ANTIDIAB_2B,
         C3M_HP_ng_ml, ELP_3_HP_ng_ml, PRO_C16_HP_ng_ml, PRO_C4_HP_ng_ml, 
         PRO_C3_roHP_ng_ml, VICM_HP_ng_ml, Mean24hAlbumin, WaistHip) %>%
  na.omit()

#################
##MIXED EFFECTS##
#################


# Refit models
model1 <- lmer(eGFR ~ time_years + (1|PK), data = Full_long_complete)
model2 <- lmer(eGFR ~ time_years + (1 + time_years|PK), data = Full_long_complete)
model3 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B + 
                 (1 + time_years|PK), data = Full_long_complete)
model4 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B 
               + C3M_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long_complete)
model5 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B 
               + ELP_3_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long_complete)
model6 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B 
               +  PRO_C16_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long_complete)
model7 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B 
               + PRO_C4_HP_ng_ml +
                 (1 + time_years|PK), data = Full_long_complete)
model8 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B 
               + PRO_C3_roHP_ng_ml + 
                 (1 + time_years|PK), data = Full_long_complete)
model9 <- lmer(eGFR ~ time_years + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B 
               +  VICM_HP_ng_ml + 
                 (1 + time_years|PK), data = Full_long_complete)
model10 <- lmer(eGFR ~ time_years + Mean24hAlbumin + WaistHip + 
                 (1 + time_years|PK), data = Full_long_complete)

# Compare models
anova(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
# Compare model 4 (C3M_HP_ng_ml) to model 3
anova(model3, model4)

# Compare model 5 (ELP_3_HP_ng_ml) to model 3
anova(model3, model5)

# Compare model 6 (PRO_C16_HP_ng_ml) to model 3
anova(model3, model6)

# Compare model 7 (PRO_C4_HP_ng_ml) to model 3
anova(model3, model7)

# Compare model 8 (PRO_C3_roHP_ng_ml) to model 3
anova(model3, model8)

# Compare model 9 (VICM_HP_ng_ml) to model 3
anova(model3, model9)

#Interaction Models
# Model 4: C3M_HP_ng_ml
model4_interaction <- lmer(eGFR ~ time_years * C3M_HP_ng_ml + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B +
                             (1 + time_years | PK), data = Full_long_complete)

# Model 5: ELP_3_HP_ng_ml
model5_interaction <- lmer(eGFR ~ time_years * ELP_3_HP_ng_ml + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B +
                             (1 + time_years | PK), data = Full_long_complete)

# Model 6: PRO_C16_HP_ng_ml
model6_interaction <- lmer(eGFR ~ time_years * PRO_C16_HP_ng_ml + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B +
                             (1 + time_years | PK), data = Full_long_complete)

# Model 7: PRO_C4_HP_ng_ml
model7_interaction <- lmer(eGFR ~ time_years * PRO_C4_HP_ng_ml + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B +
                             (1 + time_years | PK), data = Full_long_complete)

# Model 8: PRO_C3_roHP_ng_ml
model8_interaction <- lmer(eGFR ~ time_years * PRO_C3_roHP_ng_ml + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B +
                             (1 + time_years | PK), data = Full_long_complete)

# Model 9: VICM_HP_ng_ml
model9_interaction <- lmer(eGFR ~ time_years * VICM_HP_ng_ml + AgeAtBaseline + SEX + ANTIHYP_2B + ANTIDIAB_2B +
                             (1 + time_years | PK), data = Full_long_complete)
#Interpretation:
summary(model4_interaction)
summary(model5_interaction)
summary(model6_interaction)
summary(model7_interaction)
summary(model8_interaction)
summary(model9_interaction)





