#Alessandra Mencos
library(tidyverse)
library(readxl)
library(writexl)
OriginalData <- read_excel("/Users/alessandramencos/BBIM01/Research Project/OriginalData_norm.xlsx")
View(OriginalData)
sledai_score_discrete <- data.frame(OriginalData$sledai_score)
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
#Descriptive stats of continuous variables
continuous_variables <- data.frame(OriginalData[sapply(OriginalData, is.numeric)])
IQR_continuous <- sapply(continuous_variables, IQR) %>% data.frame()
other_info_continuous <- sapply(continuous_variables, summary) %>% t() %>% data.frame()
IQR_DESCRIPTIVE_CONTINOUS <- c(IQR_continuous, other_info_continuous) %>% data.frame()
colnames(IQR_DESCRIPTIVE_CONTINOUS) <- c('IQR', 'Min', 'Q1', 'Median', 'Q3', 'Max')

ethnicity <- c(counts_ethnicity, percentage_ethnicity) %>% data.frame()
colnames(ethnicity) <- c('Ethnicity', 'Counts (out of 181)', 'Percentage (%)')
menopause <- c(counts_menopausal, percentage_menopause) %>% data.frame()
colnames(menopause) <- c('Menopausal Status', 'Counts (out of 181)', 'Percentage (%)')
sledai <- c(counts_sledai, percentage_sledai) %>% data.frame()
colnames(sledai) <- c('SLEDAI level', 'Counts (out of 181)', 'Percentage(%)')
rm(percentage_ethnicity, percentage_menopause, percentage_sledai, counts_ethnicity, 
   counts_menopausal, counts_sledai, bars, coul_ethnicity, coul_menopausal, coul_sledai, 
   bar_percent_ethnicity, bar_percent_menopause, bar_percent_sledai, IQR_continuous,
   other_info_continuous)

#question 1: is SLEDAI score related to biomarker expression?
biomarkers <- c('vwf_iu_dl', 'sdc1_ng_ml', 'tm_ng_ml', 
                'ox_ldl_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')
#vWF
sledai_vwf <- lm(vwf_iu_dl~sledai_score + age_at_diagnosis_years + 
                   time_since_diagnosis_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status,
                 data = OriginalData)
vwf_discrete_sledai <- glm(vwf_iu_dl~sledai_score_discrete$OriginalData.sledai_score + 
                             age_at_diagnosis_years + time_since_diagnosis_years + 
                             bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                           data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_vwf[['fitted.values']])) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'darkgreen', fill = 'seagreen') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(title = "vWF levels in relation to SLEDAI category", x = 'SLEDAI level', y = 'vWF levels')

#sdc1
sledai_sdc1 <- lm(sdc1_ng_ml~sledai_score + age_at_diagnosis_years + 
                     time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                     ifn_type1_iu_ml + menopausal_status, data = OriginalData)
sdc1_discrete_sledai <- glm(sdc1_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_sdc1[['fitted.values']])) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'blue', fill = 'skyblue') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(title = "sDC1 levels in relation to SLEDAI category", x = 'SLEDAI level', y = 'sDC1 levels')

#tm
sledai_tm <- lm(tm_ng_ml~sledai_score +  age_at_diagnosis_years + 
                   time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status, data = OriginalData)
tm_discrete_sledai <- glm(tm_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_tm[['fitted.values']])) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'lemonchiffon4', fill = 'lemonchiffon3') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(title = "TM levels in relation to SLEDAI category", x = 'SLEDAI level', y = 'TM levels')

#oxLDL
sledai_oxLDL <- lm(ox_ldl_ng_ml~sledai_score + age_at_diagnosis_years + 
                      time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                      ifn_type1_iu_ml + menopausal_status, data = OriginalData)
oxldl_discrete_sledai <- glm(ox_ldl_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_oxLDL[['fitted.values']])) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'mediumpurple4', fill = 'lavender') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(title = "ox LDL levels in relation to SLEDAI category", x = 'SLEDAI level', 
       y = 'ox LDL levels')

#sVCAM1
sledai_VCAM1 <- lm(svcam1_ng_ml~sledai_score + age_at_diagnosis_years + 
                      time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                      ifn_type1_iu_ml + menopausal_status, data = OriginalData)
svcam1_discrete_sledai <- glm(svcam1_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_VCAM1[['fitted.values']])) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'indianred4', fill = 'indianred') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(title = "s-VCAM1 levels in relation to SLEDAI category", x = 'SLEDAI level', 
       y = 's-VCAM1 levels')

#LDH
sledai_LDH <- lm(ldh_u_l~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ldh_discrete_sledai <- glm(ldh_u_l~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_LDH[['fitted.values']])) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'goldenrod4', fill = 'goldenrod') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(title = "LDH levels in relation to SLEDAI category", x = 'SLEDAI level', y = 'LDH levels')

par(mfrow = c(2,2))
plot(sledai_vwf, which = c(2, 4), main = 'lm vWF vs SLEDAI')
plot(vwf_discrete_sledai, which = c(2,4), main = 'glm vWF vs Discrete SLEDAI')
plot(sledai_sdc1, which = c(2, 4), main = 'lm sDC1 vs SLEDAI')
plot(sdc1_discrete_sledai, which = c(2,4), main = 'glm sDC1 vs Discrete SLEDAI')
plot(sledai_tm, which = c(2, 4), main = 'lm TM vs SLEDAI')
plot(tm_discrete_sledai, which = c(2,4), main = 'glm TM vs Discrete SLEDAI')
plot(sledai_VCAM1, which = c(2, 4), main = 'lm s-VCAM1 vs SLEDAI')
plot(svcam1_discrete_sledai, which = c(2,4), main = 'glm s-VCAM1 vs Discrete SLEDAI')
plot(sledai_oxLDL, which = c(2, 4), main = 'lm ox LDL vs SLEDAI')
plot(oxldl_discrete_sledai, which = c(2,4), main = 'glm ox LDL vs Discrete SLEDAI')
plot(sledai_LDH, which = c(2, 4), main = 'lm LDH vs SLEDAI')
plot(ldh_discrete_sledai, which = c(2,4), main = 'glm LDH vs Discrete SLEDAI')

res_biomarkers_sledai <- list(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1, 
                                  sledai_oxLDL, sledai_LDH)
names(res_biomarkers_sledai) <- paste(biomarkers, 'vs SLEDAI categories')
results_biomarkers_sledai_summaries <- lapply(res_biomarkers_sledai, function(m) {
  df <- as.data.frame(summary(m)$coefficients)
  df <- cbind(Term = rownames(df), df)
  rownames(df) <- NULL
  df
})
res_biomarkers_dscrtsledai <- list(vwf_discrete_sledai, sdc1_discrete_sledai, 
                                       tm_discrete_sledai, svcam1_discrete_sledai, 
                                       oxldl_discrete_sledai, ldh_discrete_sledai)
names(res_biomarkers_dscrtsledai) <- paste(biomarkers, 'vs discrete SLEDAI')
results_biomarkers_dscrtsledai_summaries <- lapply(res_biomarkers_dscrtsledai, function(m) {
  df <- as.data.frame(summary(m)$coefficients)
  df <- cbind(Term = rownames(df), df)
  rownames(df) <- NULL
  df
})
biomarker_sledai_summaries <- c(results_biomarkers_sledai_summaries, 
                                results_biomarkers_dscrtsledai_summaries)
biomarkers_sledai <- c(res_biomarkers_sledai, res_biomarkers_dscrtsledai)
conf_interval <- lapply(biomarkers_sledai, function(x) {
  ci <- confint(x)
  df <- as.data.frame(ci)
  df <- cbind(Term = rownames(df), df)
  rownames(df) <- NULL
  df
})

rm(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1,sledai_oxLDL, 
   sledai_LDH, vwf_discrete_sledai, sdc1_discrete_sledai, tm_discrete_sledai, 
   svcam1_discrete_sledai, oxldl_discrete_sledai, ldh_discrete_sledai, res_biomarkers_dscrtsledai, 
   res_biomarkers_sledai, results_biomarkers_dscrtsledai_summaries, 
   results_biomarkers_sledai_summaries, biomarkers_sledai)

#QUESTION 2: is there a relationship between disease activity and OPG levels? 
par(mfrow = c(1,1))
sledai_opg <- glm(opg_pg_ml~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
opg_discrete_sledai <- glm(opg_pg_ml~sledai_score_discrete$OriginalData.sledai_score + 
                             age_at_diagnosis_years + time_since_diagnosis_years + 
                             bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                           data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_opg[['fitted.values']])) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'royalblue', fill = 'powderblue') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  labs(title = "OPG levels in relation to SLEDAI category", x = 'SLEDAI level', y = 'OPG levels')
par(mfrow = c(1,2))
plot(sledai_opg, which = c(2,4), main = 'lm OPG vs SLEDAI')
results_opg_sledai <- list(sledai_opg, opg_discrete_sledai)
sledai_opg_summaries <- lapply(results_opg_sledai, function(m) {
  df <- as.data.frame(summary(m)$coefficients)
  df <- cbind(Term = rownames(df), df)
  rownames(df) <- NULL
  df
})
names(sledai_opg_summaries) <- c('OPG vs SLEDAI categories', 'OPG vs discrete SLEDAI')
confint_sledai_opg <- lapply(results_opg_sledai, function(x) {
  ci <- confint(x)
  df <- as.data.frame(ci)
  df <- cbind(Term = rownames(df), df)
  rownames(df) <- NULL
  df
})
names(confint_sledai_opg) <- c('OPG vs SLEDAI categories', 'OPG vs discrete SLEDAI')
rm(results_opg_sledai, sledai_opg, opg_discrete_sledai)


##thid question: is OPG related to the biomarkers? 
#opg and vwf
par(mfrow = c(1,1))
vwf_opg <- glm(opg_pg_ml~ vwf_iu_dl + age_at_diagnosis_years + 
                 time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                 ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(vwf_iu_dl, vwf_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth(method = glm) +
  labs(x = 'vWF levels', y = 'OPG levels', title = 'Plasma OPG levels in relation to vWF levels')

#opg and sdc1
sdc1_opg <- glm(opg_pg_ml~ sdc1_ng_ml + age_at_diagnosis_years + 
                  time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                  ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(sdc1_ng_ml, sdc1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth(method = glm) +
  labs(x = 'sDC1 levels', y = 'OPG levels', title = 'Plasma OPG levels in relation to sDC1 levels')

#opg and tm
tm_opg <- glm(opg_pg_ml~ tm_ng_ml + age_at_diagnosis_years + 
                time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(tm_ng_ml, tm_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth(method = glm) +
  labs(x = 'TM levels', y = 'OPG levels', title = 'Plasma OPG levels in relation to TM levels')
#opg and ox LDL
oxldl_opg <- glm(opg_pg_ml~ ox_ldl_ng_ml + age_at_diagnosis_years + 
                   time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(ox_ldl_ng_ml, oxldl_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth(method = glm) +
  labs(x = 'ox LDL levels', y = 'OPG levels', title = 'Plasma OPG levels in relation to ox LDL levels')

#opg and svcam1
svcam1_opg <- glm(opg_pg_ml~ svcam1_ng_ml + age_at_diagnosis_years + 
                    time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(svcam1_ng_ml, svcam1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth(method = glm) +
  labs(x = 's-VCAM1 levels', y = 'OPG levels', title = 'Plasma OPG levels in relation to s-VCAM1 levels')

#opg and ldh
ldh_opg <- glm(opg_pg_ml~ ldh_u_l + age_at_diagnosis_years + 
                 time_since_diagnosis_years + age_years + bmi_kg_m2 + 
                 ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(ldh_u_l, ldh_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth(method = glm) +
  labs(x = 'LDH', y = 'OPG levels', title = 'Plasma OPG in relation to vWF levels')

par(mfrow = c(2,2))
plot(vwf_opg, which = c(2, 4), main = 'lm OPG vWF')
plot(sdc1_opg, which = c(2, 4), main = 'lm OPG sDC1')
plot(tm_opg, which = c(2, 4), main = 'lm OPG TM')
plot(svcam1_opg, which = c(2, 4), main = 'lm OPG s-VCAM1')
plot(oxldl_opg, which = c(2, 4), main = 'lm OPG ox LDL')
plot(ldh_opg, which = c(2, 4), main = 'lm OPG LDH')

res_opg_biomarkers <- list(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg)
names(res_opg_biomarkers) <- biomarkers
results_opg_biomarkers_summary <- lapply(res_opg_biomarkers, function(m) {
  df <- as.data.frame(summary(m)$coefficients)
  df <- cbind(Term = rownames(df), df)
  rownames(df) <- NULL
  df
})
confint_opgbiomarkers <- lapply(res_opg_biomarkers, function(x) {
  ci <- confint(x)
  df <- as.data.frame(ci)
  df <- cbind(Term = rownames(df), df)
  rownames(df) <- NULL
  df
})
rm(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg, res_opg_biomarkers)

to_export <- list(ethnicity, menopause, sledai, IQR_DESCRIPTIVE_CONTINOUS)
names(to_export) <- c('Ethnicity', 'Menopause', 'SLEDAI', 'Continous Variables')
write_xlsx(to_export, path = "/Users/alessandramencos/BBIM01/Research Project/Researchprojectdata.xlsx")
write_xlsx(biomarker_sledai_summaries, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Biomarkers_vs_SLEDAI.xlsx")
write_xlsx(conf_interval, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Biomarkers_vs_SLEDAI_confint.xlsx")
write_xlsx(sledai_opg_summaries, 
           path = "/Users/alessandramencos/BBIM01/Research Project/OPG_vs_SLEDAI.xlsx")
write_xlsx(confint_sledai_opg, 
           path = "/Users/alessandramencos/BBIM01/Research Project/OPG_vs_SLEDAI_confint.xlsx")
write_xlsx(results_opg_biomarkers_summary, 
           path = "/Users/alessandramencos/BBIM01/Research Project/OPG_vs_Biomarkers.xlsx")
write_xlsx(confint_opgbiomarkers, 
           path = "/Users/alessandramencos/BBIM01/Research Project/OPG_vs_Biomarkers_confint.xlsx")
##end 