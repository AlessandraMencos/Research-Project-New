#Alessandra Mencos
library(tidyr)
library(readxl)
library(dplyr)
OriginalData <- read_excel("~/BBIM01/Research Project/Research-Project/g1_s1_dataset_v251001.xlsx")
View(OriginalData)
##question 1: is SLEDAI score related to biomarker expression/

OriginalData$sledai_score <- cut(OriginalData$sledai_score, 
                                 breaks = c(0, 1, 5, 10,30), include.lowest = TRUE, 
                                 labels = c('no/low activity', 'mild activity', 
                                            'moderate activity', 
                                            'high/very high activity'))
summary(OriginalData$sledai_score)

OriginalData <- OriginalData %>% mutate(ethnicity = factor(ethnicity))
OriginalData <- OriginalData %>% mutate(menopausal_status = factor(menopausal_status))

biomarkers <- c('vwf_iu_dl', 'sdc1_ng_ml', 'tm_ng_ml', 'ox_ldl_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')

shapiro_df <- data.frame(
  biomarker = biomarkers,
  p_value = sapply(biomarkers, function(col) 
    shapiro.test(OriginalData[[col]])$p.value), 
  w_statistic = sapply(biomarkers, function(col) 
    unname(shapiro.test(OriginalData[[col]])$statistic)))

par(mfrow = c(1,2))
#vWF
sledai_vwf <- lm(vwf_iu_dl~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status,
                 data = OriginalData)
plot(OriginalData$sledai_score, sledai_vwf[['fitted.values']], main = 'vWF', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$vwf_iu_dl, sledai_vwf$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b =1, a = 0)

#sdc1
sledai_sdc1 <- lm(sdc1_ng_ml~sledai_score + age_years + 
                    ifn_type1_iu_ml,  data = OriginalData)
plot(OriginalData$sledai_score, sledai_sdc1[['fitted.values']], main = 'sDC1', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$sdc1_ng_ml, sledai_sdc1$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#tm
sledai_tm <- lm(tm_ng_ml~sledai_score + time_since_diagnosis_years + 
                  age_years + bmi_kg_m2 + ifn_type1_iu_ml +
                  menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_tm[['fitted.values']], main = 'TM', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$tm_ng_ml, sledai_tm$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#oxLDL
sledai_oxLDL <- lm(ox_ldl_ng_ml~sledai_score + age_years + 
                     bmi_kg_m2 + ifn_type1_iu_ml, 
                   data = OriginalData)
plot(OriginalData$sledai_score, sledai_oxLDL[['fitted.values']], main = 'ox LDL',
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$ox_ldl_ng_ml, sledai_oxLDL$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#sVCAM1
sledai_VCAM1 <- lm(svcam1_ng_ml~sledai_score + age_years + bmi_kg_m2, 
                   data = OriginalData)
plot(OriginalData$sledai_score, sledai_VCAM1[['fitted.values']], main = 'sVCAM1',
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$svcam1_ng_ml, sledai_VCAM1$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#LDH
sledai_LDH <- lm(ldh_u_l~sledai_score + age_at_diagnosis_years + age_years +
                 + bmi_kg_m2 + ifn_type1_iu_ml + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_LDH[['fitted.values']], main = 'LDH', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$ldh_u_l, sledai_LDH$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

results_biomarkers_sledai <- list(summary(sledai_vwf), summary(sledai_sdc1), 
                                  summary(sledai_tm), summary(sledai_VCAM1), 
                                  summary(sledai_oxLDL), summary(sledai_LDH))
names(results_biomarkers_sledai) <- biomarkers
rm(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1, 
   sledai_oxLDL, sledai_LDH)

#based on the results, it seems none of the biomarkers are good predictors 
  #for mild activity, but all of them (sans oxLDL and TM) are good predictors 
  #for low, moderate and high activities. 

##I don't think that's the right way to adjust confounders, but the rest of the 
  ##confounders can just be added back to the original model. 

#QUESTION 2: is there a relationship between disease activity and OPG levels? 

sledai_opg <- lm(opg_pg_ml~sledai_score + age_at_diagnosis_years +
                   time_since_diagnosis_years + bmi_kg_m2 +
                   ifn_type1_iu_ml + ethnicity + menopausal_status, 
                 data = OriginalData)

summary(sledai_opg)
plot(OriginalData$sledai_score, sledai_opg[['fitted.values']], main = 'OPG', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$opg_pg_ml, sledai_opg$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

##thid question: is OPG related to the biomarkers? 
#opg and vwf
vwf_opg <- lm(opg_pg_ml~vwf_iu_dl + age_at_diagnosis_years + time_since_diagnosis_years
              + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, vwf_opg[['fitted.values']], main = 'vWF', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$opg_pg_ml, vwf_opg$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#opg and sdcq1
sdc1_opg <- lm(opg_pg_ml~sdc1_ng_ml + age_at_diagnosis_years + time_since_diagnosis_years
               + ifn_type1_iu_ml + ethnicity + 
                 menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sdc1_opg[['fitted.values']], main = 'sDC1', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$opg_pg_ml, sdc1_opg$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#opg and tm
tm_opg <- lm(opg_pg_ml~tm_ng_ml + age_at_diagnosis_years + time_since_diagnosis_years
             + bmi_kg_m2 + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, tm_opg[['fitted.values']], main = 'TM', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$opg_pg_ml, tm_opg$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#opg and ox LDL
oxldl_opg <- lm(opg_pg_ml~ox_ldl_ng_ml + age_at_diagnosis_years + time_since_diagnosis_years 
                + bmi_kg_m2 + ifn_type1_iu_ml + menopausal_status, 
               data = OriginalData)
plot(OriginalData$sledai_score, oxldl_opg[['fitted.values']], main = 'ox LDL', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$opg_pg_ml, oxldl_opg$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#opg and svcam1
svcam1_opg <- lm(opg_pg_ml~svcam1_ng_ml + age_at_diagnosis_years + time_since_diagnosis_years +
                   menopausal_status, 
                data = OriginalData)
plot(OriginalData$sledai_score, svcam1_opg[['fitted.values']], main = 'sVCAM1', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$opg_pg_ml,svcam1_opg$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

#opg and ldh
ldh_opg <- lm(opg_pg_ml~ldh_u_l + age_at_diagnosis_years + time_since_diagnosis_years +
                menopausal_status, 
                data = OriginalData)
plot(OriginalData$sledai_score, ldh_opg[['fitted.values']], main = 'LDH', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
plot(OriginalData$opg_pg_ml, ldh_opg$fitted.values, 
     xlab = 'true values', ylab = ' ') +
  abline (b = 1, a = 0)

results_opg_biomarkers <- list(summary(vwf_opg), summary(sdc1_opg), 
                               summary(tm_opg), summary(oxldl_opg), 
                               summary(svcam1_opg), summary(ldh_opg))
names(results_opg_biomarkers) <- biomarkers
rm(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg)