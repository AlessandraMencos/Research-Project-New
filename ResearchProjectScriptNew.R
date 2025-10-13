#Alessandra Mencos
library(tidyverse)
library(readxl)
library(writexl)
OriginalData <- read_excel("/Users/alessandramencos/BBIM01/Research Project/OriginalData_norm.xlsx")
View(OriginalData)

#make the necessary columns into categories
OriginalData$sledai_score <- cut(OriginalData$sledai_score, 
                                 breaks = c(-1, 0, 5, 10,30), include.lowest = TRUE, 
                                 labels = c('no activity', 'mild activity', 
                                            'moderate activity', 
                                            'high activity'))
OriginalData <- OriginalData %>% mutate(ethnicity = factor(ethnicity))
OriginalData <- OriginalData %>% mutate(menopausal_status = factor(menopausal_status))

par(bg = 'beige', font.main = 4, cex.main = 1.5, cex.lab = 1.2, font.lab = 2)
counts_sledai <- OriginalData %>% group_by(sledai_score) %>% reframe(count = n())
coul_sledai <- c('pink', 'indianred', 'firebrick', 'firebrick4')
bars <- barplot(counts_sledai$count, 
                names.arg = counts_sledai$sledai_score, axis.lty = 1, 
                ylim = c(0, 90), main = 'SLEDAI 2k score patient counts', col = coul_sledai, 
                xlab = 'SLEDAI category', ylab = 'Counts')
text(x = bars, y = counts_sledai$count, labels = counts_sledai$count, pos = 3)
counts_menopausal <- OriginalData %>% group_by(menopausal_status) %>% reframe(count = n())
coul_menopausal <- c('seagreen', 'palegreen4')
bars <- barplot(counts_menopausal$count, 
                names.arg = counts_menopausal$menopausal_status, axis.lty = 1, 
                ylim = c(0, 110), main = 'Menopausal Status counts', col = coul_menopausal, 
                xlab = 'Menopausal Status', ylab = 'Counts')
text(x = bars, y = counts_menopausal$count, labels = counts_menopausal$count, pos = 3)
counts_ethnicity <- OriginalData %>% group_by(ethnicity) %>% reframe(count = n())
coul_ethnicity <- c('lightskyblue', 'lightskyblue4')
bars <- barplot(counts_ethnicity$count, 
                names.arg = counts_ethnicity$ethnicity, axis.lty = 1, 
                ylim = c(0, 180), main = 'Ethnicity Counts', col = coul_ethnicity,
                xlab = 'Ethnicity', ylab = 'Counts')
text(x = bars, y = counts_ethnicity$count, labels = counts_ethnicity$count, pos = 3)
#now, percentages: 
percentage_sledai <- ((counts_sledai$count/181)*100) %>% data.frame()
bar_percent_sledai <- barplot(percentage_sledai$., ylim = c(0, 100), 
                              names.arg = counts_sledai$sledai_score, 
                              main = 'Percentage of patients in each group', axis.lty = 1, 
                              col = coul_sledai, xlab = 'SLEDAI category', ylab = 'Percent (%)')
text(x = bar_percent_sledai, y = percentage_sledai$., 
     labels = paste0(round(percentage_sledai$., 2), '%'), pos = 3)
percentage_menopause <- ((counts_menopausal$count/181)*100) %>% data.frame()
bar_percent_menopause <- barplot(percentage_menopause$., ylim = c(0, 100), 
                              names.arg = counts_menopausal$menopausal_status, 
                              main = 'Percentage of patients in each group', axis.lty = 1, 
                              col = coul_menopausal, xlab = 'Menopausal Status', ylab = 'Percent (%)')
text(x = bar_percent_menopause, y = percentage_menopause$., 
     labels = paste0(round(percentage_menopause$., 2), '%'), pos = 3)
percentage_ethnicity <- ((counts_ethnicity$count/181)*100) %>% data.frame()
bar_percent_ethnicity <- barplot(percentage_ethnicity$., ylim = c(0, 100), 
                              names.arg = counts_ethnicity$ethnicity,
                              main = 'Percentage of patients in each group', axis.lty = 1, 
                              col = coul_ethnicity, xlab = 'Ethnicity', ylab = 'Percent (%)')
text(x = bar_percent_ethnicity, y = percentage_ethnicity$., 
     labels = paste0(round(percentage_ethnicity$., 2), '%'), pos = 3)

to_export <- list(percentage_ethnicity, counts_ethnicity, percentage_menopause, 
                  counts_menopausal, percentage_sledai, counts_sledai)
names(to_export) <- c('% Ethnicity', 'Counts Ethnicity', '% Menopausal Status', 
                      'Counts Menopausal Status', '% SLEDAI category', 
                      'Counts SLEDAI category')
write_xlsx(to_export, path = "/Users/alessandramencos/BBIM01/Research Project/Researchprojectdata.xlsx")
rm(percentage_ethnicity, percentage_menopause, percentage_sledai, counts_ethnicity, 
   counts_menopausal, counts_sledai, bars, coul_ethnicity, coul_menopausal, coul_sledai, 
   bar_percent_ethnicity, bar_percent_menopause, bar_percent_sledai, to_export)

print("question 1: is SLEDAI score related to biomarker expression?")
biomarkers <- c('vwf_iu_dl', 'sdc1_ng_ml', 'tm_ng_ml', 
                'ox_ldl_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')
#vWF
sledai_vwf <- glm(vwf_iu_dl~sledai_score + age_at_diagnosis_years + 
                   time_since_diagnosis_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status,
                 data = OriginalData)
plot(OriginalData$sledai_score, sledai_vwf[['fitted.values']], main = 'vWF', 
     xlab = 'SLEDAI activity', ylab = 'fitted values vWF')

#sdc1
sledai_sdc1 <- glm(sdc1_ng_ml~sledai_score + age_at_diagnosis_years + 
                     time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                     ifn_type1_iu_ml + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_sdc1[['fitted.values']], main = 'sDC1', 
     xlab = 'SLEDAI activity', ylab = 'Fitted values sDC1')

#tm
sledai_tm <- glm(tm_ng_ml~sledai_score +  age_at_diagnosis_years + 
                   time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_tm[['fitted.values']], main = 'TM', 
     xlab = 'SLEDAI activity', ylab = 'fitted values TM')

#oxLDL
sledai_oxLDL <- glm(ox_ldl_ng_ml~sledai_score + age_at_diagnosis_years + 
                      time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                      ifn_type1_iu_ml + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_oxLDL[['fitted.values']], main = 'ox LDL',
     xlab = 'SLEDAI activity', ylab = 'fitted values ox LDL')

#sVCAM1
sledai_VCAM1 <- glm(svcam1_ng_ml~sledai_score + age_at_diagnosis_years + 
                      time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                      ifn_type1_iu_ml + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_VCAM1[['fitted.values']], main = 'sVCAM1',
     xlab = 'SLEDAI activity', ylab = 'fitted values s-VCAM1')

#LDH
sledai_LDH <- glm(ldh_u_l~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_LDH[['fitted.values']], main = 'LDH', 
     xlab = 'SLEDAI activity', ylab = 'fitted values LDH')
par(mfrow = c(2,2))
plot(sledai_vwf, which = c(2, 4), main = 'lm SLEDAI vWF')
plot(sledai_sdc1, which = c(2, 4), main = 'lm SLEDAI sDC1')
plot(sledai_tm, which = c(2, 4), main = 'lm SLEDAI TM')
plot(sledai_VCAM1, which = c(2, 4), main = 'lm SLEDAI s-VCAM1')
plot(sledai_oxLDL, which = c(2, 4), main = 'lm SLEDAI ox LDL')
plot(sledai_LDH, which = c(2, 4), main = 'lm SLEDAI LDH')

results_biomarkers_sledai <- list(summary(sledai_vwf), summary(sledai_sdc1), 
                                  summary(sledai_tm), summary(sledai_VCAM1), 
                                  summary(sledai_oxLDL), summary(sledai_LDH))
names(results_biomarkers_sledai) <- biomarkers
rm(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1, 
   sledai_oxLDL, sledai_LDH)

#QUESTION 2: is there a relationship between disease activity and OPG levels? 
par(mfrow = c(1,1))
sledai_opg <- glm(opg_pg_ml~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
summary(sledai_opg)
plot(OriginalData$sledai_score, sledai_opg[['fitted.values']], main = 'OPG', 
     xlab = 'SLEDAI activity', ylab = 'fitted values OPG')
par(mfrow = c(1,2))
plot(sledai_opg, which = c(2,4), main = 'lm SLEDAI OPG')

##thid question: is OPG related to the biomarkers? 
#opg and vwf
par(mfrow = c(1,1))
vwf_opg <- glm(opg_pg_ml~ vwf_iu_dl + age_at_diagnosis_years + 
                 time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                 ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(vwf_iu_dl, vwf_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'vWF')

#opg and sdc1
sdc1_opg <- glm(opg_pg_ml~ sdc1_ng_ml + age_at_diagnosis_years + 
                  time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                  ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(sdc1_ng_ml, sdc1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'sDC1')

#opg and tm
tm_opg <- glm(opg_pg_ml~ tm_ng_ml + age_at_diagnosis_years + 
                time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(tm_ng_ml, tm_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'TM')

#opg and ox LDL
oxldl_opg <- glm(opg_pg_ml~ ox_ldl_ng_ml + age_at_diagnosis_years + 
                   time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(ox_ldl_ng_ml, oxldl_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'ox LDL')

#opg and svcam1
svcam1_opg <- glm(opg_pg_ml~ svcam1_ng_ml + age_at_diagnosis_years + 
                    time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(svcam1_ng_ml, svcam1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 's-VCAM1')

#opg and ldh
ldh_opg <- glm(opg_pg_ml~ ldh_u_l + age_at_diagnosis_years + 
                 time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                 ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(ldh_u_l, ldh_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'LDH')
par(mfrow = c(2,2))
plot(vwf_opg, which = c(2, 4), main = 'lm OPG vWF')
plot(sdc1_opg, which = c(2, 4), main = 'lm OPG sDC1')
plot(tm_opg, which = c(2, 4), main = 'lm OPG TM')
plot(svcam1_opg, which = c(2, 4), main = 'lm OPG s-VCAM1')
plot(oxldl_opg, which = c(2, 4), main = 'lm OPG ox LDL')
plot(ldh_opg, which = c(2, 4), main = 'lm OPG LDH')

results_opg_biomarkers <- list(summary(vwf_opg), summary(sdc1_opg), 
                               summary(tm_opg), summary(oxldl_opg), 
                               summary(svcam1_opg), summary(ldh_opg))
names(results_opg_biomarkers) <- biomarkers
rm(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg)

##end 