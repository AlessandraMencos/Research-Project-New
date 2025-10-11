#Alessandra Mencos
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
OriginalData <- read_excel("~/BBIM01/Research Project/Research-Project/g1_s1_dataset_v251007.xlsx")
View(OriginalData)
##question 1: is SLEDAI score related to biomarker expression/

##remove patients diagnosed before 18 years of age
OriginalData <- OriginalData[OriginalData$age_at_diagnosis_years >=18, ]

#make the necessary columns into categories
OriginalData$sledai_score <- cut(OriginalData$sledai_score, 
                                 breaks = c(-1, 0, 5, 10,30), include.lowest = TRUE, 
                                 labels = c('no/low activity', 'mild activity', 
                                            'moderate activity', 
                                            'high/very high activity'))
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
#rm(bars, coul_ethnicity, coul_menopausal, coul_sledai) 

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

biomarkers <- c('vwf_iu_dl', 'sdc1_ng_ml', 'tm_ng_ml', 
                'ox_ldl_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')

shapiro_df_biomarkers <- data.frame(
  biomarker = biomarkers,
  p_value = sapply(biomarkers, function(col) 
    shapiro.test(OriginalData[[col]])$p.value), 
  w_statistic = sapply(biomarkers, function(col) 
    unname(shapiro.test(OriginalData[[col]])$statistic)))
##none of the biomarkers have a normal distributions, check for left or right with histograms
histograms_biomarkers <- lapply(biomarkers, function(col) {
  print(hist(OriginalData[[col]], data = OriginalData, xlab = col, 
             main = paste('Distribution of ', col)))
  })
##all the histograms show that the distribution is ± right shifted, 
##so we take the log of all the biomarkers to normalize the data and check again

shapiro_df_biomarkers_log2 <- data.frame(
  biomarker = biomarkers,
  p_value = sapply(biomarkers, function(col) 
    shapiro.test(log2(OriginalData[[col]]))$p.value), 
  w_statistic = sapply(biomarkers, function(col) 
    unname(shapiro.test(OriginalData[[col]])$statistic)))
#seems to be normally distributed now
#check histograms
histograms_biomarkers_log2 <- lapply(biomarkers, function(col) {
  print(hist(log2(OriginalData[[col]]), data = OriginalData, xlab = paste('Log ', col), main = paste('Distribution of log', col)))
})
##seems about right 

#vWF
sledai_vwf <- glm(log2(vwf_iu_dl)~sledai_score + log2(age_at_diagnosis_years) + 
                   log2(time_since_diagnosis_years) + log2(bmi_kg_m2) + 
                   log2(ifn_type1_iu_ml) + menopausal_status,
                 data = OriginalData)
plot(OriginalData$sledai_score, sledai_vwf[['fitted.values']], main = 'vWF', 
     xlab = 'SLEDAI activity', ylab = 'fitted values vWF')

#sdc1
sledai_sdc1 <- glm(log2(sdc1_ng_ml)~sledai_score + log2(age_years) + 
                    log2(ifn_type1_iu_ml),  data = OriginalData)
plot(OriginalData$sledai_score, sledai_sdc1[['fitted.values']], main = 'sDC1', 
     xlab = 'SLEDAI activity', ylab = 'Fitted values sDC1')

#tm
sledai_tm <- glm(log2(tm_ng_ml)~sledai_score + log2(time_since_diagnosis_years) + 
                  log2(age_years) + log2(bmi_kg_m2) + log2(ifn_type1_iu_ml) +
                  menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_tm[['fitted.values']], main = 'TM', 
     xlab = 'SLEDAI activity', ylab = 'fitted values TM')

#oxLDL
sledai_oxLDL <- glm(log2(ox_ldl_ng_ml)~sledai_score + log2(time_since_diagnosis_years) + 
                     log2(age_years) + log2(bmi_kg_m2), 
                   data = OriginalData)
plot(OriginalData$sledai_score, sledai_oxLDL[['fitted.values']], main = 'ox LDL',
     xlab = 'SLEDAI activity', ylab = 'fitted values ox LDL')

#sVCAM1
sledai_VCAM1 <- glm(log2(svcam1_ng_ml)~sledai_score + log2(age_years) + log2(bmi_kg_m2) +
                    log2(ifn_type1_iu_ml) + menopausal_status,
                   data = OriginalData)
plot(OriginalData$sledai_score, sledai_VCAM1[['fitted.values']], main = 'sVCAM1',
     xlab = 'SLEDAI activity', ylab = 'fitted values s-VCAM1')

#LDH
sledai_LDH <- glm(log2(ldh_u_l)~sledai_score + log2(age_years) +
                   + log2(bmi_kg_m2) + menopausal_status, data = OriginalData)
plot(OriginalData$sledai_score, sledai_LDH[['fitted.values']], main = 'LDH', 
     xlab = 'SLEDAI activity', ylab = 'fitted values LDH')

results_biomarkers_sledai <- list(summary(sledai_vwf), summary(sledai_sdc1), 
                                  summary(sledai_tm), summary(sledai_VCAM1), 
                                  summary(sledai_oxLDL), summary(sledai_LDH))
names(results_biomarkers_sledai) <- biomarkers
rm(sledai_vwf, sledai_sdc1, sledai_tm, sledai_VCAM1, 
   sledai_oxLDL, sledai_LDH)

#QUESTION 2: is there a relationship between disease activity and OPG levels? 
##repear log transformations for these: 
sledai_opg <- glm(opg_pg_ml~sledai_score + log2(age_at_diagnosis_years) +
                   log2(age_years) + log2(bmi_kg_m2) +
                   log2(ifn_type1_iu_ml) + ethnicity + menopausal_status, 
                 data = OriginalData)
summary(sledai_opg)
plot(OriginalData$sledai_score, sledai_opg[['fitted.values']], main = 'OPG', 
     xlab = 'SLEDAI activity', ylab = 'fitted values OPG')

##thid question: is OPG related to the biomarkers? 
#opg and vwf
vwf_opg <- glm(opg_pg_ml~log2(vwf_iu_dl) + log2(age_at_diagnosis_years) + 
                 log2(time_since_diagnosis_years) + log2(age_years) + log2(ifn_type1_iu_ml) +
                 menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(vwf_iu_dl, vwf_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'vWF')

#opg and sdc1
sdc1_opg <- glm(opg_pg_ml~log2(sdc1_ng_ml) + log2(age_at_diagnosis_years) + 
                  log2(time_since_diagnosis_years) + log2(age_years) + 
                  log2(ifn_type1_iu_ml) + ethnicity + menopausal_status, 
                data = OriginalData)
ggplot(data = OriginalData, aes(sdc1_ng_ml, sdc1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'sDC1')

#opg and tm
tm_opg <- glm(opg_pg_ml~log2(tm_ng_ml) + age_at_diagnosis_years + 
                time_since_diagnosis_years
             + bmi_kg_m2 + menopausal_status, data = OriginalData)
ggplot(data = OriginalData, aes(tm_ng_ml, tm_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'TM')

#opg and ox LDL
oxldl_opg <- glm(opg_pg_ml~log2(ox_ldl_ng_ml) + log2(age_at_diagnosis_years) + 
                   log2(time_since_diagnosis_years) + log2(age_years) + 
                   log2(bmi_kg_m2) + log2(ifn_type1_iu_ml) + menopausal_status, 
                data = OriginalData)
ggplot(data = OriginalData, aes(ox_ldl_ng_ml, oxldl_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'ox LDL')

#opg and svcam1
svcam1_opg <- glm(opg_pg_ml~log2(svcam1_ng_ml) + log2(age_at_diagnosis_years) + 
                    log2(time_since_diagnosis_years) + log2(age_years) +
                   menopausal_status, 
                 data = OriginalData)
ggplot(data = OriginalData, aes(svcam1_ng_ml, svcam1_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 's-VCAM1')

#opg and ldh
ldh_opg <- glm(opg_pg_ml~log2(ldh_u_l) + log2(age_at_diagnosis_years) + 
                 log2(time_since_diagnosis_years) + log2(age_years) +
                 log2(ifn_type1_iu_ml) + menopausal_status, 
              data = OriginalData)
ggplot(data = OriginalData, aes(ldh_u_l, ldh_opg$fitted.values)) +
  geom_point()+ 
  geom_smooth() +
  labs(x = 'True Values', y = 'Fitted Values OPG', title = 'LDH')

results_opg_biomarkers <- list(summary(vwf_opg), summary(sdc1_opg), 
                               summary(tm_opg), summary(oxldl_opg), 
                               summary(svcam1_opg), summary(ldh_opg))
names(results_opg_biomarkers) <- biomarkers
rm(vwf_opg, sdc1_opg, tm_opg, oxldl_opg, svcam1_opg, ldh_opg)

##rosner test

##end 