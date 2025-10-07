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
  p_value = sapply(biomarkers, function(col) shapiro.test(OriginalData[[col]])$p.value), 
  w_statistic = sapply(biomarkers, function(col) unname(shapiro.test(OriginalData[[col]])$statistic)))

#vWF
sledai_vwf <- lm(vwf_iu_dl~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + bmi_kg_m2,
                  data = OriginalData)
summary(sledai_vwf)
par(mfrow = c(2,2))
plot(sledai_vwf, main = 'vWF')
par(mfrow = c(1,2))
plot(OriginalData$sledai_score, sledai_vwf[['fitted.values']], main = 'vWF')
plot(OriginalData$vwf_iu_dl, sledai_vwf$fitted.values, 
     xlab = 'true values', ylab = 'fitted values') +
  abline (b =1, a = 0)

#sdc1
sledai_sdc1 <- lm(sdc1_ng_ml~sledai_score + age_at_diagnosis_years + 
                    ifn_type1_iu_ml,  data = OriginalData)
summary(sledai_sdc1)
par(mfrow = c(2,2))
plot(sledai_sdc1, main = 'sDC1')
par(mfrow = c(1, 2))
plot(OriginalData$sledai_score, sledai_sdc1[['fitted.values']], main = 'sDC1')
plot(OriginalData$sdc1_ng_ml, sledai_sdc1$fitted.values, 
     xlab = 'true values', ylab = 'fitted values') +
  abline (b = 1, a = 0)

#tm
sledai_tm <- lm(tm_ng_ml~sledai_score + bmi_kg_m2 + ifn_type1_iu_ml, data = OriginalData)
summary(sledai_tm)
par(mfrow = c(2,2))
plot(sledai_tm, main = 'TM')
par(mfrow = c(1,2))
plot(OriginalData$sledai_score, sledai_tm[['fitted.values']], main = 'TM')
plot(OriginalData$tm_ng_ml, sledai_tm$fitted.values, 
     xlab = 'true values', ylab = 'fitted values') +
  abline (b = 1, a = 0)

#oxLDL
sledai_oxLDL <- lm(ox_ldl_ng_ml~sledai_score + 
                     time_since_diagnosis_years + bmi_kg_m2 + ifn_type1_iu_ml, 
                   data = OriginalData)
summary(sledai_oxLDL)
par(mfrow = c(2,2))
plot(sledai_oxLDL, main = 'ox LDL')
par(mfrow = c(1,2))
plot(OriginalData$sledai_score, sledai_oxLDL[['fitted.values']], main = 'ox LDL')
plot(OriginalData$ox_ldl_ng_ml, sledai_oxLDL$fitted.values, 
     xlab = 'true values', ylab = 'fitted values') +
  abline (b = 1, a = 0)

#sVCAM1
sledai_VCAM1 <- lm(svcam1_ng_ml~sledai_score, data = OriginalData)
summary(sledai_VCAM1)
par(mfrow = c(2,2))
plot(sledai_VCAM1, main = 'sVCAM')
par(mfrow = c(1,2))
plot(OriginalData$sledai_score, sledai_VCAM1[['fitted.values']], main = 'sVCAM1')
plot(OriginalData$svcam1_ng_ml, sledai_VCAM1$fitted.values, 
     xlab = 'true values', ylab = 'fitted values') +
  abline (b = 1, a = 0)

#LDH
sledai_LDH <- lm(ldh_u_l~sledai_score + age_at_diagnosis_years 
                 + bmi_kg_m2 + ifn_type1_iu_ml, data = OriginalData)
summary(sledai_LDH)
par(mfrow = c(2,2))
plot(sledai_LDH, main = 'LDH')
par(mfrow = c(1,2))
plot(OriginalData$sledai_score, sledai_LDH[['fitted.values']], main = 'LDH')
plot(OriginalData$ldh_u_l, sledai_LDH$fitted.values, 
     xlab = 'true values', ylab = 'fitted values') +
  abline (b = 1, a = 0)

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
##^^The above is how i had originally done the adjustment for the confoundng in 
  ##in the biomarkers, but we were told we may have been adjusting for too many things. 
  ##I guess what I want to know is if we just take those that have significance in the summary? 
summary(sledai_opg)
par(mfrow = c(2,2))
plot(sledai_opg, main = 'OPG')
par(mfrow = c(1,2))
plot(OriginalData$sledai_score, sledai_opg[['fitted.values']], main = 'OPG')
plot(OriginalData$opg_pg_ml, sledai_opg$fitted.values, 
     xlab = 'true values', ylab = 'fitted values') +
  abline (b = 1, a = 0)


