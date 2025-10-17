#Alessandra Mencos
library(tidyverse)
library(readxl)
library(writexl)
OriginalData <- read_excel("/Users/alessandramencos/BBIM01/Research Project/Research-Project-New/OriginalData_norm.xlsx")
View(OriginalData)
sledai_score_discrete <- data.frame(OriginalData$sledai_score)
#make the necessary columns into categories
OriginalData$sledai_score <- cut(OriginalData$sledai_score, 
                                 breaks = c(-1, 0, 5, 10,30), include.lowest = TRUE, 
                                 labels = c('No activity', 'Mild activity', 
                                            'Moderate activity', 
                                            'High activity'))
OriginalData <- OriginalData %>% mutate(ethnicity = factor(ethnicity))
OriginalData <- OriginalData %>% mutate(menopausal_status = factor(menopausal_status))

par(bg = 'white',font.main = 4, cex.main = 1.5, cex.lab = 1.2, font.lab = 2, mfrow = c(1,1),
    font.sub = 3, cex.sub = 0.8)
counts_sledai <- OriginalData %>% group_by(sledai_score) %>% reframe(count = n())
coul_sledai <- c('pink', 'indianred', 'firebrick', 'firebrick4')
bars <- barplot(counts_sledai$count, 
                names.arg = counts_sledai$sledai_score, axis.lty = 1, 
                ylim = c(0, 90), main = 'SLEDAI 2k score patient counts', col = coul_sledai, 
                xlab = 'SLEDAI category', ylab = 'Counts', sub = 'n = 181')
text(x = bars, y = counts_sledai$count, labels = counts_sledai$count, pos = 3)
counts_menopausal <- OriginalData %>% group_by(menopausal_status) %>% reframe(count = n())
coul_menopausal <- c('seagreen', 'palegreen4')
bars <- barplot(counts_menopausal$count, 
                names.arg = counts_menopausal$menopausal_status, axis.lty = 1, 
                ylim = c(0, 110), main = 'Menopausal Status counts', col = coul_menopausal, 
                xlab = 'Menopausal Status', ylab = 'Counts', sub = 'n = 181')
text(x = bars, y = counts_menopausal$count, labels = counts_menopausal$count, pos = 3)
counts_ethnicity <- OriginalData %>% group_by(ethnicity) %>% reframe(count = n())
coul_ethnicity <- c('lightskyblue', 'lightskyblue4')
bars <- barplot(counts_ethnicity$count, 
                names.arg = counts_ethnicity$ethnicity, axis.lty = 1, 
                ylim = c(0, 180), main = 'Ethnicity Counts', col = coul_ethnicity,
                xlab = 'Ethnicity', ylab = 'Counts', sub = 'n = 181')
text(x = bars, y = counts_ethnicity$count, labels = counts_ethnicity$count, pos = 3)
#now, percentages: 
percentage_sledai <- ((counts_sledai$count/181)*100) %>% data.frame()
bar_percent_sledai <- barplot(percentage_sledai$., ylim = c(0, 100), 
                              names.arg = counts_sledai$sledai_score, 
                              main = 'Percentage of patients in each group', axis.lty = 1, 
                              col = coul_sledai, xlab = 'SLEDAI category', 
                              ylab = 'Percent (%)', sub = 'n = 181')
text(x = bar_percent_sledai, y = percentage_sledai$., 
     labels = paste0(round(percentage_sledai$., 2), '%'), pos = 3)
percentage_menopause <- ((counts_menopausal$count/181)*100) %>% data.frame()
bar_percent_menopause <- barplot(percentage_menopause$., ylim = c(0, 100), 
                              names.arg = counts_menopausal$menopausal_status, 
                              main = 'Percentage of patients in each group', axis.lty = 1, 
                              col = coul_menopausal, xlab = 'Menopausal Status', 
                              ylab = 'Percent (%)', sub = 'n = 181')
text(x = bar_percent_menopause, y = percentage_menopause$., 
     labels = paste0(round(percentage_menopause$., 2), '%'), pos = 3)
percentage_ethnicity <- ((counts_ethnicity$count/181)*100) %>% data.frame()
bar_percent_ethnicity <- barplot(percentage_ethnicity$., ylim = c(0, 100), 
                              names.arg = counts_ethnicity$ethnicity,
                              main = 'Percentage of patients in each group', axis.lty = 1, 
                              col = coul_ethnicity, xlab = 'Ethnicity', 
                              ylab = 'Percent (%)', sub = 'n = 181')
text(x = bar_percent_ethnicity, y = percentage_ethnicity$., 
     labels = paste0(round(percentage_ethnicity$., 2), '%'), pos = 3)
#Descriptive stats of continuous variables
continuous_variables_names <- c('age_at_diagnosis_years', 'time_since_diagnosis_years', 
                                'age_years', 'bmi_kg_m2', 'ifn_type1_iu_ml', 'opg_pg_ml', 
                                'vwf_iu_dl', 'sdc1_ng_ml','tm_ng_ml', 'ox_ldl_ng_ml',
                                'svcam1_ng_ml', 'ldh_u_l')
continuous_variables <- data.frame(OriginalData[sapply(OriginalData, is.numeric)])
IQR_continuous <- sapply(continuous_variables, IQR) %>% data.frame()
other_info_continuous <- sapply(continuous_variables, summary) %>% t() %>% data.frame()
IQR_DESCRIPTIVE_CONTINOUS <- c(IQR_continuous, other_info_continuous) %>% data.frame()
IQR_DESCRIPTIVE_CONTINOUS <- cbind(term = (continuous_variables_names), IQR_DESCRIPTIVE_CONTINOUS)
colnames(IQR_DESCRIPTIVE_CONTINOUS) <- c('Variable', 'IQR', 'Min', 'Q1', 'Median','Mean', 'Q3', 'Max')

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
sledai_vwf <- glm(vwf_iu_dl~sledai_score + age_at_diagnosis_years + 
                   time_since_diagnosis_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status,
                 data = OriginalData)
vwf_discrete_sledai <- glm(vwf_iu_dl~sledai_score_discrete$OriginalData.sledai_score + 
                             age_at_diagnosis_years + time_since_diagnosis_years + 
                             bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                           data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_vwf[['fitted.values']])) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'darkgreen', fill = 'seagreen') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  geom_jitter(color = 'black', position=position_jitter(0.16), alpha = 0.9) +
  labs(title = "vWF levels in relation to SLEDAI category", x = 'SLEDAI category', 
       y = 'vWF levels [IU/dL]', subtitle = 'n = 181')

#sdc1
sledai_sdc1 <- glm(sdc1_ng_ml~sledai_score + age_at_diagnosis_years + 
                     time_since_diagnosis_years + bmi_kg_m2 + 
                     ifn_type1_iu_ml + menopausal_status, data = OriginalData)
sdc1_discrete_sledai <- glm(sdc1_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_sdc1[['fitted.values']])) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'blue', fill = 'skyblue') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  geom_jitter(color = 'black', position=position_jitter(0.16), alpha = 0.9) +
  labs(title = "SDC-1 levels in relation to SLEDAI category", x = 'SLEDAI category', 
       y = 'SDC-1 levels [ng/mL]', subtitle = 'n = 181')

#tm
sledai_tm <- glm(tm_ng_ml~sledai_score +  age_at_diagnosis_years + 
                   time_since_diagnosis_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status, data = OriginalData)
tm_discrete_sledai <- glm(tm_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_tm[['fitted.values']])) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'lemonchiffon4', fill = 'lemonchiffon3') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  geom_jitter(color = 'black', position=position_jitter(0.16), alpha = 0.9) +
  labs(title = "TM levels in relation to SLEDAI category", x = 'SLEDAI category', 
       y = 'TM levels [ng/mL]', subtitle = 'n = 181')

#oxLDL
sledai_oxLDL <- glm(ox_ldl_ng_ml~sledai_score + age_at_diagnosis_years + 
                      time_since_diagnosis_years + bmi_kg_m2 + 
                      ifn_type1_iu_ml + menopausal_status, data = OriginalData)
oxldl_discrete_sledai <- glm(ox_ldl_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_oxLDL[['fitted.values']])) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'mediumpurple4', fill = 'lavender') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  geom_jitter(color = 'black', position=position_jitter(0.16), alpha = 0.9) +
  labs(title = "ox-LDL levels in relation to SLEDAI category", x = 'SLEDAI category', 
       y = 'ox-LDL levels [ng/mL]', subtitle = 'n = 181')

#sVCAM1
sledai_VCAM1 <- glm(svcam1_ng_ml~sledai_score + age_at_diagnosis_years + 
                      time_since_diagnosis_years + bmi_kg_m2 + 
                      ifn_type1_iu_ml + menopausal_status, data = OriginalData)
svcam1_discrete_sledai <- glm(svcam1_ng_ml~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_VCAM1[['fitted.values']])) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'indianred4', fill = 'indianred') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  geom_jitter(color = 'black', position=position_jitter(0.16), alpha = 0.9) +
  labs(title = "sVCAM1 levels in relation to SLEDAI category", x = 'SLEDAI category', 
       y = 'sVCAM1 levels [ng/mL]', subtitle = 'n = 181')

#LDH
sledai_LDH <- glm(ldh_u_l~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ldh_discrete_sledai <- glm(ldh_u_l~sledai_score_discrete$OriginalData.sledai_score + 
                              age_at_diagnosis_years + time_since_diagnosis_years + 
                              bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                            data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_LDH[['fitted.values']])) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'goldenrod4', fill = 'goldenrod') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  geom_jitter(color = 'black', position=position_jitter(0.16), alpha = 0.9) +
  labs(title = "LDH levels in relation to SLEDAI category", x = 'SLEDAI category', 
       y = 'LDH levels [U/L]', subtitle = 'n = 181')

par(mfrow = c(2,2))
plot(sledai_vwf, which = c(2, 4), main = 'vWF vs SLEDAI')
plot(vwf_discrete_sledai, which = c(2,4), main = 'vWF vs Discrete SLEDAI')
plot(sledai_sdc1, which = c(2, 4), main = 'SDC-1 vs SLEDAI')
plot(sdc1_discrete_sledai, which = c(2,4), main = 'SDC-1 vs Discrete SLEDAI')
plot(sledai_tm, which = c(2, 4), main = 'TM vs SLEDAI')
plot(tm_discrete_sledai, which = c(2,4), main = 'TM vs Discrete SLEDAI')
plot(sledai_VCAM1, which = c(2, 4), main = 'sVCAM1 vs SLEDAI')
plot(svcam1_discrete_sledai, which = c(2,4), main = 'sVCAM1 vs Discrete SLEDAI')
plot(sledai_oxLDL, which = c(2, 4), main = 'ox-LDL vs SLEDAI')
plot(oxldl_discrete_sledai, which = c(2,4), main = 'ox-LDL vs Discrete SLEDAI')
plot(sledai_LDH, which = c(2, 4), main = 'LDH vs SLEDAI')
plot(ldh_discrete_sledai, which = c(2,4), main = 'LDH vs Discrete SLEDAI')

res_biomarkers_sledai <- list(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1, 
                                  sledai_oxLDL, sledai_LDH)
names(res_biomarkers_sledai) <- paste(biomarkers, 'vs SLEDAI categories')
results_biomarkers_sledai_summaries <- lapply(res_biomarkers_sledai, function(model) {
  df_coef <- as.data.frame(summary(model)$coefficients)
  df_coef <- cbind(Term = rownames(df_coef), df_coef)
})
res_biomarkers_dscrtsledai <- list(vwf_discrete_sledai, sdc1_discrete_sledai, 
                                       tm_discrete_sledai, svcam1_discrete_sledai, 
                                       oxldl_discrete_sledai, ldh_discrete_sledai)
names(res_biomarkers_dscrtsledai) <- paste(biomarkers, 'vs discrete SLEDAI')
results_biomarkers_dscrtsledai_summaries <- lapply(res_biomarkers_dscrtsledai, function(model) {
  df_coef <- as.data.frame(summary(model)$coefficients)
  df_coef <- cbind(Term = rownames(df_coef), df_coef)
})
biomarker_sledai_summaries <- c(results_biomarkers_sledai_summaries, 
                                results_biomarkers_dscrtsledai_summaries)
biomarkers_sledai <- c(res_biomarkers_sledai, res_biomarkers_dscrtsledai)
conf_interval <- lapply(biomarkers_sledai, function(model) {
  conf_int <- confint(model)
  df_confint <- as.data.frame(conf_int)
  df_confint <- cbind(Term = rownames(df_confint), df_confint)
})
#note that df_coef cbind and df_confint cbind are NECESSARY because I'm going to
  #export these object as .xlsx, which wouldn't export the names if they're not
  #in a separate column! 
rm(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1,sledai_oxLDL, 
   sledai_LDH, vwf_discrete_sledai, sdc1_discrete_sledai, tm_discrete_sledai, 
   svcam1_discrete_sledai, oxldl_discrete_sledai, ldh_discrete_sledai, res_biomarkers_dscrtsledai, 
   res_biomarkers_sledai, results_biomarkers_dscrtsledai_summaries, 
   results_biomarkers_sledai_summaries, biomarkers_sledai)

#QUESTION 2: is there a relationship between disease activity and OPG levels? 
par(mfrow = c(1,1))
sledai_opg <- glm(opg_pg_ml~sledai_score + age_at_diagnosis_years + 
                    time_since_diagnosis_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
opg_discrete_sledai <- glm(opg_pg_ml~sledai_score_discrete$OriginalData.sledai_score + 
                             age_at_diagnosis_years + time_since_diagnosis_years + 
                             bmi_kg_m2 +ifn_type1_iu_ml + menopausal_status, 
                           data = OriginalData)
ggplot(data= OriginalData, aes(sledai_score, sledai_opg[['fitted.values']])) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_violin(trim = FALSE, alpha = 0.6, color = 'chocolate4', fill = 'chocolate') +
  geom_boxplot(alpha = 0.8, width = 0.2) +
  geom_jitter(color = 'black', position=position_jitter(0.16), alpha = 0.9) +
  labs(title = "OPG levels in relation to SLEDAI category", x = 'SLEDAI category', 
       y = 'OPG levels [pg/mL]', subtitle = 'n = 181')
par(mfrow = c(1,2))
plot(sledai_opg, which = c(2,4), main = 'OPG vs SLEDAI')
results_opg_sledai <- list(sledai_opg, opg_discrete_sledai)
sledai_opg_summaries <- lapply(results_opg_sledai, function(model) {
  df_coef <- as.data.frame(summary(model)$coefficients)
  df_coef <- cbind(Term = rownames(df_coef), df_coef)
})
names(sledai_opg_summaries) <- c('OPG vs SLEDAI categories', 'OPG vs discrete SLEDAI')
confint_sledai_opg <- lapply(results_opg_sledai, function(x) {
  conf_int <- confint(x)
  df_confint <- as.data.frame(conf_int)
  df_confint <- cbind(Term = rownames(df_confint), df_confint)
})
names(confint_sledai_opg) <- c('OPG vs SLEDAI categories', 'OPG vs discrete SLEDAI')
rm(results_opg_sledai, sledai_opg, opg_discrete_sledai)


##thid question: is OPG related to the biomarkers? 
#opg and vwf
par(mfrow = c(1,1))
vwf_opg <- glm(opg_pg_ml~ vwf_iu_dl + age_at_diagnosis_years + 
                 time_since_diagnosis_years + bmi_kg_m2 + 
                 ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(vwf_iu_dl, vwf_opg$fitted.values)) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_point(color = 'black', shape = 20)+ 
  geom_smooth(method = glm, color = 'darkolivegreen', fill = 'darkolivegreen2', alpha = 0.5) +
  labs(x = 'vWF levels [IU/dL]', y = 'OPG levels [pg/mL]', 
       title = 'Plasma OPG levels in relation to vWF levels', subtitle = 'n = 181')

#opg and sdc1
sdc1_opg <- glm(opg_pg_ml~ sdc1_ng_ml + age_at_diagnosis_years + 
                  time_since_diagnosis_years + bmi_kg_m2 + 
                  ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(sdc1_ng_ml, sdc1_opg$fitted.values)) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_point(color = 'black', shape = 20)+ 
  geom_smooth(method = glm, color = 'navyblue', fill = 'cornflowerblue', alpha = 0.5) +
  labs(x = 'SDC-1 levels [ng/mL]', y = 'OPG levels [pg/mL]', 
       title = 'Plasma OPG levels in relation to SDC-1 levels', subtitle = 'n = 181')

#opg and tm
tm_opg <- glm(opg_pg_ml~ tm_ng_ml + age_at_diagnosis_years + 
                time_since_diagnosis_years + bmi_kg_m2 + 
                ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(tm_ng_ml, tm_opg$fitted.values)) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_point(color = 'black', shape = 20)+ 
  geom_smooth(method = glm, color = 'bisque3', fill = 'bisque', alpha = 0.5) +
  labs(x = 'TM levels [ng/mL]', y = 'OPG levels [pg/mL]', 
       title = 'Plasma OPG levels in relation to TM levels', subtitle = 'n = 181') 
#opg and ox-LDL
oxldl_opg <- glm(opg_pg_ml~ ox_ldl_ng_ml + age_at_diagnosis_years + 
                   time_since_diagnosis_years + bmi_kg_m2 + 
                   ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(ox_ldl_ng_ml, oxldl_opg$fitted.values)) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_point(color = 'black', shape = 20)+ 
  geom_smooth(method = glm, color = 'orchid4', fill = 'plum', alpha = 0.5) +
  labs(x = 'ox-LDL levels [ng/mL]', y = 'OPG levels [pg/mL]', 
       title = 'Plasma OPG levels in relation to ox-LDL levels', subtitle = 'n = 181')

#opg and svcam1
svcam1_opg <- glm(opg_pg_ml~ svcam1_ng_ml + age_at_diagnosis_years + 
                    time_since_diagnosis_years + bmi_kg_m2 + 
                    ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(svcam1_ng_ml, svcam1_opg$fitted.values)) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_point(color = 'black', shape = 20)+ 
  geom_smooth(method = glm, color = 'firebrick4', fill = 'indianred', alpha = 0.5) +
  labs(x = 'sVCAM1 levels [ng/mL]', y = 'OPG levels [pg/mL]', 
       title = 'Plasma OPG levels in relation to sVCAM1 levels', subtitle = 'n = 181')

#opg and ldh
ldh_opg <- glm(opg_pg_ml~ ldh_u_l + age_at_diagnosis_years + 
                 time_since_diagnosis_years  + bmi_kg_m2 + 
                 ifn_type1_iu_ml + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(ldh_u_l, ldh_opg$fitted.values)) +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'), 
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'), 
        panel.grid.minor = element_line(color = 'grey', linetype = 'dashed')) +
  geom_point(color = 'black', shape = 20)+ 
  geom_smooth(method = glm, color = 'gold4', fill = 'goldenrod1', alpha = 0.5) +
  labs(x = 'LDH levels [U/L]', y = 'OPG levels [pg/mL]', 
       title = 'Plasma OPG in relation to LDH levels', subtitle = 'n = 181')

par(mfrow = c(2,2))
plot(vwf_opg, which = c(2, 4), main = 'OPG vWF')
plot(sdc1_opg, which = c(2, 4), main = 'OPG SDC-1')
plot(tm_opg, which = c(2, 4), main = 'OPG TM')
plot(svcam1_opg, which = c(2, 4), main = 'OPG sVCAM1')
plot(oxldl_opg, which = c(2, 4), main = 'OPG ox-LDL')
plot(ldh_opg, which = c(2, 4), main = 'OPG LDH')

res_opg_biomarkers <- list(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg)
names(res_opg_biomarkers) <- biomarkers
results_opg_biomarkers_summary <- lapply(res_opg_biomarkers, function(model) {
  df_coef <- as.data.frame(summary(model)$coefficients)
  df_coef <- cbind(Term = rownames(df_coef), df_coef)
})
confint_opgbiomarkers <- lapply(res_opg_biomarkers, function(x) {
  conf_int <- confint(x)
  df_confint <- as.data.frame(conf_int)
  df_confint <- cbind(Term = rownames(df_confint), df_confint)
})
rm(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg, res_opg_biomarkers)

to_export <- list(ethnicity, menopause, sledai, IQR_DESCRIPTIVE_CONTINOUS)
names(to_export) <- c('Ethnicity', 'Menopause', 'SLEDAI', 'Continous variables adjusted')
write_xlsx(to_export, path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)/Researchprojectdata.xlsx")
write_xlsx(biomarker_sledai_summaries, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)/Biomarkers_vs_SLEDAI.xlsx")
write_xlsx(conf_interval, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)//Biomarkers_vs_SLEDAI_confint.xlsx")
write_xlsx(sledai_opg_summaries, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)/OPG_vs_SLEDAI.xlsx")
write_xlsx(confint_sledai_opg, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)/OPG_vs_SLEDAI_confint.xlsx")
write_xlsx(results_opg_biomarkers_summary, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)/OPG_vs_Biomarkers.xlsx")
write_xlsx(confint_opgbiomarkers, 
           path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)/OPG_vs_Biomarkers_confint.xlsx")
##end 