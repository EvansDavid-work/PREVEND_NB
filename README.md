A repository for all code related to the PREVEND Nordic Bioscience ECM Remodelling Biomarkers CKD Manuscript.

Script Sequence:
1. Preprocessing -> 2. Baseline and Descriptive Stats -> 3. Cross Sectional Regressions -> 4. LMER -> 5. Albumin Clustering -> 6. CKD Logistic Reg and Sensetivity -> 7. Sensetivity analyses.

1. Preprocessing
-----------------
Combines PREVEND and Nordic ECM datasets and calculates clinical endpoints used in this study.
- Filters to participants with complete biomarker data
- Albuminuria (mg/24h): (UAC×UVOL/1000) averaged across 2 collections/visit (per NC)
- Variable naming follows PREVEND conventions (NC2=baseline)

2. Baseline and Descriptive Stats
---------------------------------
Baseline demographics 

3. Cross Sectional Regressions
------------------------------
Baseline linear regressions of biomarkers (z-scored) vs. eGFR_NC2 and albuminuria (Mean24hAlbumin_NC2 and log24hAlbumin_NC2). 
univariate, age + sex adjusted, albumin + age + sex + HYP_NC2 + DIAB_NC2 adjusted models.

4. LMER
-------
Linear mixed effects models (lmer) for eGFR trajectories (NC2→NC5). Best-fitting adjusted models selected via anova() comparisons → eGFR × time interactions predicting decline rates.

5. Albumin Clustering
---------------------
Clusters PREVEND log-albumin trajectories (NC2-5) into 3 risk groups using k-means (k=3 via elbow plot). Multinomial logistic tests baseline biomarkers as predictors (univariate models and adjusted for clinical risk factors).

Steps:
- summary_data: log_albumin_rate + mean_log_albumin per PK
- kmeans(k=3, nstart=25) → low/moderate/high-risk  
- multinom(Cluster ~ biomarkers + age/sex/DIAB/HYP/eGFR) + FDR

6. Logistic Regressions and sensitivity
---------------------------------------
Binomial logistic regressions predict incident CKD, microalbuminuria, low-eGFR, and macroalbuminuria from z-scored biomarkers + age/sex/HYP/DIAB.

Endpoints:
- CKD_Status (incident only, microalbuminuria OR low eGFR)
- Microalbuminuria_Status (albumin≥30)  
- LoweGFR_Status (eGFR<60)
- Macroalbuminuria_Status (albumin≥300)

Adjusted: biomarker + age + sex + HYP_NC2 + DIAB_NC2
Unadjusted: biomarker only (linear models)
LLoQ sensitivity: 50% LLoQ biomarkers 

7. Sensitivity Analyses (Supplementary)
---------------------------------------
- LMER Attrition Bias
Tests dropout modification of biomarker×time interactions. Dropout=1 if missing visits. 6 lmer models: time*biomarker + time*Dropout + biomarker*Dropout. No significant interactions → complete cases valid.

- Albumin Clustering LLoQ Sensitivity  
Biomarkers at 50% LLoQ. Repeats k=3 clustering + multinomial logistic. Associations robust.


