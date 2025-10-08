#Alessandra Mencos
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
OriginalData <- read_excel("~/BBIM01/Research Project/Research-Project/g1_s1_dataset_v251001.xlsx")
View(OriginalData)
##question 1: is SLEDAI score related to biomarker expression/

counts_sledai <- OriginalData %>% group_by(sledai_score) %>% reframe(count = n())
bars <- barplot(counts_sledai$count, 
                names.arg = counts_sledai$sledai_score, axis.lty = 1, 
                ylim = c(0, 60), main = 'SLEDAI 2k score patient counts')
text(x = bars, y = counts_sledai$count, labels = counts_sledai$count, pos = 3, 
     cex = 0.7)
rm(bars)

OriginalData$sledai_score <- cut(OriginalData$sledai_score, 
                                 breaks = c(-1, 0, 5, 10,30), include.lowest = TRUE, 
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

#vWF
sledai_vwf <- lm(log2(vwf_iu_dl)~sledai_score + age_at_diagnosis_years + 
                   time_since_diagnosis_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status,
                 data = OriginalData)
plot(OriginalData$sledai_score, sledai_vwf[['fitted.values']], main = 'vWF', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(vwf_iu_dl, sledai_vwf$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'vWF')

#sdc1
sledai_sdc1 <- lm(log2(sdc1_ng_ml)~sledai_score + age_years + 
                    ifn_type1_iu_ml,  data = OriginalData)
plot(OriginalData$sledai_score, sledai_sdc1[['fitted.values']], main = 'sDC1', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(sdc1_ng_ml, sledai_sdc1$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'sDC1')

#tm
sledai_tm <- lm(log2(tm_ng_ml)~sledai_score + time_since_diagnosis_years + 
                  age_years + bmi_kg_m2 + ifn_type1_iu_ml +
                  menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_tm[['fitted.values']], main = 'TM', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(tm_ng_ml, sledai_tm$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'TM')

#oxLDL
sledai_oxLDL <- lm(ox_ldl_ng_ml~sledai_score + age_years + 
                     bmi_kg_m2 + ifn_type1_iu_ml, 
                   data = OriginalData)
plot(OriginalData$sledai_score, sledai_oxLDL[['fitted.values']], main = 'ox LDL',
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(ox_ldl_ng_ml, sledai_oxLDL$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'ox LDL')

#sVCAM1
sledai_VCAM1 <- lm(log2(svcam1_ng_ml)~sledai_score + age_years + bmi_kg_m2, 
                   data = OriginalData)
plot(OriginalData$sledai_score, sledai_VCAM1[['fitted.values']], main = 'sVCAM1',
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(svcam1_ng_ml, sledai_VCAM1$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 's-VCAM1')

#LDH
sledai_LDH <- lm(log2(ldh_u_l)~sledai_score + age_at_diagnosis_years + age_years +
                   + bmi_kg_m2 + ifn_type1_iu_ml + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_LDH[['fitted.values']], main = 'LDH', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(ldh_u_l, sledai_LDH$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'LDH')

results_biomarkers_sledai <- list(summary(sledai_vwf), summary(sledai_sdc1), 
                                  summary(sledai_tm), summary(sledai_VCAM1), 
                                  summary(sledai_oxLDL), summary(sledai_LDH))
names(results_biomarkers_sledai) <- biomarkers
rm(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1, 
   sledai_oxLDL, sledai_LDH)

#QUESTION 2: is there a relationship between disease activity and OPG levels? 

sledai_opg <- lm(log2(opg_pg_ml)~sledai_score + age_at_diagnosis_years +
                   time_since_diagnosis_years + bmi_kg_m2 +
                   ifn_type1_iu_ml + ethnicity + menopausal_status, 
                 data = OriginalData)
summary(sledai_opg)
plot(OriginalData$sledai_score, sledai_opg[['fitted.values']], main = 'OPG', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(opg_pg_ml, sledai_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'OPG')

##thid question: is OPG related to the biomarkers? 
#opg and vwf
vwf_opg <- lm(log2(opg_pg_ml)~log2(vwf_iu_dl) + age_at_diagnosis_years + time_since_diagnosis_years
              + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, vwf_opg[['fitted.values']], main = 'vWF', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(vwf_iu_dl, vwf_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'vWF')

#opg and sdcq1
sdc1_opg <- lm(log2(opg_pg_ml)~log2(sdc1_ng_ml) + age_at_diagnosis_years + time_since_diagnosis_years
               + ifn_type1_iu_ml + ethnicity + 
                 menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sdc1_opg[['fitted.values']], main = 'sDC1', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(sdc1_ng_ml, sdc1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'sDC1')

#opg and tm
tm_opg <- lm(log2(opg_pg_ml)~log2(tm_ng_ml) + age_at_diagnosis_years + time_since_diagnosis_years
             + bmi_kg_m2 + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, tm_opg[['fitted.values']], main = 'TM', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(tm_ng_ml, tm_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'TM')

#opg and ox LDL
oxldl_opg <- lm(log2(opg_pg_ml)~log2(ox_ldl_ng_ml) + age_at_diagnosis_years + time_since_diagnosis_years 
                + bmi_kg_m2 + ifn_type1_iu_ml + menopausal_status, 
                data = OriginalData)
plot(OriginalData$sledai_score, oxldl_opg[['fitted.values']], main = 'ox LDL', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(ox_ldl_ng_ml, oxldl_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'ox LDL')

#opg and svcam1
svcam1_opg <- lm(log2(opg_pg_ml)~log2(svcam1_ng_ml) + age_at_diagnosis_years + time_since_diagnosis_years +
                   menopausal_status, 
                 data = OriginalData)
plot(OriginalData$sledai_score, svcam1_opg[['fitted.values']], main = 'sVCAM1', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(svcam1_ng_ml, svcam1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 's-VCAM1')

#opg and ldh
ldh_opg <- lm(log2(opg_pg_ml)~log2(ldh_u_l) + age_at_diagnosis_years + time_since_diagnosis_years +
                menopausal_status, 
              data = OriginalData)
plot(OriginalData$sledai_score, ldh_opg[['fitted.values']], main = 'LDH', 
     xlab = 'SLEDAI activity', ylab = 'fitted values')
ggplot(data = OriginalData, aes(ldh_u_l, ldh_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values', title = 'LDH')

results_opg_biomarkers <- list(summary(vwf_opg), summary(sdc1_opg), 
                               summary(tm_opg), summary(oxldl_opg), 
                               summary(svcam1_opg), summary(ldh_opg))
names(results_opg_biomarkers) <- biomarkers
rm(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg)

##end 
