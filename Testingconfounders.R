#Alessandra Mencos
library(tidyverse)
library(readxl)
library(bestNormalize)
library(writexl)
OriginalData <- read_excel("/Users/alessandramencos/BBIM01/Research Project/Research-Project-New/g1_s1_dataset_v251007.xlsx")
View(OriginalData)
OriginalData <- OriginalData[OriginalData$age_at_diagnosis_years >=18, ]
##question 1: is SLEDAI score related to biomarker expression/

OriginalData$sledai_score <- cut(OriginalData$sledai_score, 
                                 breaks = c(-1, 0, 5, 10, 30), include.lowest = TRUE, 
                                 labels = c('no activity', 'mild activity', 
                                            'moderate activity', 
                                            'high activity'))
OriginalData <- OriginalData %>% mutate(ethnicity = factor(ethnicity))
OriginalData <- OriginalData %>% mutate(menopausal_status = factor(menopausal_status))

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
write_xlsx(IQR_DESCRIPTIVE_CONTINOUS, path = "/Users/alessandramencos/BBIM01/Research Project/Results (xlsx)/Continuous Variables.xlsx")

biomarkers <- c('vwf_iu_dl', 'sdc1_ng_ml', 'tm_ng_ml', 
                'ox_ldl_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')
shapiro_opg <- shapiro.test(OriginalData$opg_pg_ml)
shapiro_opg <- data.frame(shapiro_opg$p.value, shapiro_opg$statistic)
colnames(shapiro_opg) <- c('P value', 'W statistic')
print(hist(OriginalData$opg_pg_ml, main = 'Distribution of OPG', xlab = 'OPG [pg/mL]'))
##OPG is normally distributed, leave as is. 

shapiro_df_biomarkers <- data.frame(
  p_value = sapply(biomarkers, function(col) 
    shapiro.test(OriginalData[[col]])$p.value), 
  w_statistic = sapply(biomarkers, function(col) 
    shapiro.test(OriginalData[[col]])$statistic))
shapiro_df_biomarkers <- cbind(term = biomarkers, shapiro_df_biomarkers)
##most of the biomarkers (sans OXLDL) don't have a normal distributions, 
  #check for left or right with histograms
histograms_biomarkers <- lapply(biomarkers, function(col) {
  print(hist(OriginalData[[col]], data = OriginalData, xlab = col, 
             main = paste('Distribution of ', col)))
})
##most of the histograms show that the distribution is ± left shifted, 
  #so we take the log of the biomarkers (sans OX LDL) to normalize the data and check again
biomarkers_sans_oxldl <- c('vwf_iu_dl', 'sdc1_ng_ml', 'tm_ng_ml', 
                           'svcam1_ng_ml', 'ldh_u_l')
shapiro_df_biomarkers_log2 <- data.frame(
  p_value = sapply(biomarkers_sans_oxldl, function(col) 
    shapiro.test(log2(OriginalData[[col]]))$p.value), 
  w_statistic = sapply(biomarkers_sans_oxldl, function(col) 
    shapiro.test(log2(OriginalData[[col]]))$statistic))
shapiro_df_biomarkers_log2 <- cbind(term = biomarkers_sans_oxldl, shapiro_df_biomarkers_log2)
#check histograms
histograms_biomarkers_log2 <- lapply(biomarkers_sans_oxldl, function(col) {
  print(hist(log2(OriginalData[[col]]), data = OriginalData, 
             xlab = paste('Log ', col), main = paste('Distribution of log', col)))
})
#seems to be normally distributed now: all except vWF
best_normalization_vWF <- bestNormalize(OriginalData$vwf_iu_dl) %>% print()
#normalize according to results
vwf_iu_dl <- arcsinh_x(OriginalData$vwf_iu_dl)
vwf_shapiro <- shapiro.test(predict(vwf_iu_dl))
print(hist(predict(vwf_iu_dl), main = 'Linearized vWF', xlab = 'vWF linearized values'))
vwf_shapiro <- data.frame(vwf_shapiro$statistic, vwf_shapiro$p.value)
vwf_shapiro <- cbind('vwf_iu_dl', vwf_shapiro)
colnames(vwf_shapiro) <- c('Term', 'W', 'P-value')
rm(histograms_biomarkers)

confounders <- c('age_at_diagnosis_years', 'time_since_diagnosis_years', 'age_years', 
                 'bmi_kg_m2', 'ifn_type1_iu_ml', 'ethnicity', 'menopausal_status')

#I'm gonna check the distribution of the confounders
shapiro_df_confounders <- data.frame(
  p_value = sapply(confounders, function(col) {
    if (is.numeric(OriginalData[[col]])) {
    shapiro.test(OriginalData[[col]])$p.value}
    else {
      NA
    }}), 
  w_statistic = sapply(confounders, function(col) {
    if (is.numeric(OriginalData[[col]])) {
      shapiro.test(OriginalData[[col]])$statistic }
    else {
      NA
    }}))
shapiro_df_confounders <- cbind(term = confounders, shapiro_df_confounders)
##only age seems to be normally distributed, check histograms
histograms_confounders <- lapply(confounders, function(col) {
  if (is.numeric(OriginalData[[col]])) {
  print(hist(OriginalData[[col]], data = OriginalData, xlab = col, 
             main = paste('Distribution of ', col)))}
  else {
    NA
  }
})
shapiro_original <- rbind(shapiro_df_biomarkers, shapiro_df_confounders)
##they're all pretty heavily shifted to the left, (sans age) 
  #so we'll take the log and check again
confounders_sans_age <- c('age_at_diagnosis_years', 'time_since_diagnosis_years',
                 'bmi_kg_m2', 'ifn_type1_iu_ml', 'ethnicity', 'menopausal_status')
shapiro_df_confounderslog2 <- data.frame(
  p_value = sapply(confounders_sans_age, function(col) {
    if (is.numeric(OriginalData[[col]])) {
      shapiro.test(log2(OriginalData[[col]]))$p.value}
    else {
      NA
    }}), 
  w_statistic = sapply(confounders_sans_age, function(col) {
    if (is.numeric(OriginalData[[col]])) {
      shapiro.test(log2(OriginalData[[col]]))$statistic}
    else {
      NA
    }}))
histograms_confounderslog2 <- lapply(confounders_sans_age, function(col) {
  if (is.numeric(OriginalData[[col]])) {
    print(hist(log2(OriginalData[[col]]), data = OriginalData, xlab = col, 
               main = paste('Distribution of log ', col)))}
  else {
    NA
  }
})
##it didn't really make them into a normal distribution, so we'll use bestnormalize
  #to check if taking the log is really the best thing
bestNormalization <- lapply(confounders_sans_age, function(col) {
  if(is.numeric(OriginalData[[col]])) {
      bn <- bestNormalize(OriginalData[[col]])}
  else {
    NULL
  }
  })
names(bestNormalization) <- confounders_sans_age
#we'll normalize based on the suggestion and adjust the original data: 
age_norm <- arcsinh_x(OriginalData$age_at_diagnosis_years)
OriginalData$age_at_diagnosis_years <- predict(age_norm)
time_norm <- orderNorm(OriginalData$time_since_diagnosis_years)
OriginalData$time_since_diagnosis_years <- predict(time_norm)
bmi_norm <- yeojohnson(OriginalData$bmi_kg_m2)
OriginalData$bmi_kg_m2 <- predict(bmi_norm)
OriginalData$vwf_iu_dl <- predict(vwf_iu_dl)
rm(age_norm, bmi_norm, time_norm, vwf_iu_dl)
#for those that have to be log transformed, we'll also adjust the data
biomarkers_confounders_to_log <- c('ifn_type1_iu_ml', 'ethnicity',
                                   'menopausal_status', 'sdc1_ng_ml',
                                   'tm_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')

for(col in biomarkers_confounders_to_log) {
  if(is.numeric(OriginalData[[col]])) {
    OriginalData[[col]] <- log2(OriginalData[[col]])
  }
}

#run the shapiro again
confounders_biomarkers <- c('age_at_diagnosis_years', 'time_since_diagnosis_years', 
                            'age_years', 'bmi_kg_m2', 'ifn_type1_iu_ml', 'ethnicity', 
                            'menopausal_status', 'vwf_iu_dl', 'sdc1_ng_ml','tm_ng_ml', 
                            'ox_ldl_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')
shapiro_df_norm <- data.frame(
  p_value = sapply(confounders_biomarkers, function(col) {
    if (is.numeric(OriginalData[[col]])) {
      shapiro.test(OriginalData[[col]])$p.value
    } else {
      NA
    }}),
  w_statistic = sapply(confounders_biomarkers, function(col) {
    if (is.numeric(OriginalData[[col]])) {
      shapiro.test(OriginalData[[col]])$statistic
    } else {
      NA
    }
  }))
shapiro_df_norm <- cbind(term = confounders_biomarkers, shapiro_df_norm)
histograms_norm <- lapply(confounders_biomarkers, function(col) {
  if (is.numeric(OriginalData[[col]])) {
    print(hist(OriginalData[[col]], data = OriginalData, xlab = col, 
               main = paste('Distribution of normalized', col)))}
  else {
    NA
  }
})
shapiro_tests <- list(shapiro_original, shapiro_df_norm)
names(shapiro_tests) <- c('Original', 'Linearized')
##it seems that age_at_diagnosis_years and vwf cannot be fully normalized, 
  #but we'll use it as it's linearized like all other variables. 
rm(shapiro_df_confounders, histograms_confounders, histograms_confounderslog2, 
   shapiro_df_confounderslog2, histograms_norm, biomarkers_sans_oxldl, 
   confounders_sans_age, shapiro_df_biomarkers, shapiro_df_biomarkers_log2, vwf_shapiro, 
   shapiro_df_norm, bestNormalization, shapiro_original, col, biomarkers_confounders_to_log, 
   best_normalization_vWF, histograms_biomarkers_log2)
#now that they're normalized the best they can be, we can compare the models. 

##VWF
par(mfrow = c(2,2))
vwf_sledai <- glm(vwf_iu_dl~sledai_score, data = OriginalData)
plot(vwf_sledai, main = 'vWF unadjusted', which = c(2,4))
vwf_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("vwf_iu_dl ~ sledai_score + ", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, main = paste('vWF', col), which = c(2,4))
  aov_results <- anova(vwf_sledai, adjusted)
  list(model = adjusted, anova = aov_results)})
names(vwf_confounding) <- confounders

anova_pvals <- sapply(vwf_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_vwf <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_vwf$significant <- anova_summary_vwf$p_value < 0.05

##sdc1
sdc1_sledai <- glm(sdc1_ng_ml~sledai_score, data = OriginalData)
plot(sdc1_sledai, main = 'sDC1 unadjusted', which = c(2,4))
sdc1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("sdc1_ng_ml ~ sledai_score +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, main = paste('sDC1', col), which = c(2,4))
  aov_results <- anova(sdc1_sledai, adjusted)
  list(model = adjusted, anova = aov_results)})
names(sdc1_confounding) <- confounders

anova_pvals <- sapply(sdc1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_sdc1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_sdc1$significant <- anova_summary_sdc1$p_value < 0.05

#tm
tm_sledai <- glm(tm_ng_ml~sledai_score, data = OriginalData)
plot(tm_sledai, main = 'TM unadjusted', which = c(2,4))
tm_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("tm_ng_ml ~ sledai_score +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, main = paste('TM', col), which = c(2,4))
  aov_results <- anova(tm_sledai, adjusted)
  list(model = adjusted, anova = aov_results)})
names(tm_confounding) <- confounders

anova_pvals <- sapply(tm_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_tm <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_tm$significant <- anova_summary_tm$p_value < 0.05

#oxldl
oxldl_sledai <- glm(ox_ldl_ng_ml~sledai_score, data = OriginalData)
plot(oxldl_sledai, main = 'ox LDL unadjusted', which = c(2,4))
oxldl_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("ox_ldl_ng_ml ~ sledai_score +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, main = paste('ox LDL', col), which = c(2,4))
  aov_results <- anova(oxldl_sledai, adjusted)
  list(model = adjusted, anova = aov_results)})
names(oxldl_confounding) <- confounders

anova_pvals <- sapply(oxldl_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_oxldl <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_oxldl$significant <- anova_summary_oxldl$p_value < 0.05

#svcam1
svcam1_sledai <- glm(svcam1_ng_ml~sledai_score, data = OriginalData)
plot(svcam1_sledai, main = 's-VCAM1 unadjusted', which = c(2,4))
svcam1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("svcam1_ng_ml ~ sledai_score +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, main = paste('sVCAM1', col), which = c(2,4))
  aov_results <- anova(svcam1_sledai, adjusted)
  list(model = adjusted, anova = aov_results)})
names(svcam1_confounding) <- confounders

anova_pvals <- sapply(svcam1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_svcam1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_svcam1$significant <- anova_summary_svcam1$p_value < 0.05

#ldh
ldh_sledai <- glm(ldh_u_l~sledai_score, data = OriginalData)
plot(ldh_sledai, main = 'LDH unadjusted', which = c(2,4))
ldh_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("ldh_u_l ~ sledai_score +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, main = paste('LDH', col), which = c(2,4))
  aov_results <- anova(ldh_sledai, adjusted)
  list(model = adjusted, anova = aov_results)})
names(ldh_confounding) <- confounders

anova_pvals <- sapply(ldh_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_ldh <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_ldh$significant <- anova_summary_ldh$p_value < 0.05

summaries_biomarkerssledai_conf <- list(anova_summary_ldh, 
                                        anova_summary_sdc1, anova_summary_vwf, 
                                        anova_summary_oxldl, anova_summary_tm, 
                                        anova_summary_svcam1)
names(summaries_biomarkerssledai_conf) <- biomarkers
rm(anova_summary_ldh, 
   anova_summary_sdc1, anova_summary_vwf, 
   anova_summary_oxldl, anova_summary_tm, 
   anova_summary_svcam1, ldh_sledai, tm_sledai, vwf_sledai, svcam1_sledai, oxldl_sledai, 
   sdc1_sledai, oxldl_confounding, svcam1_confounding, sdc1_confounding, vwf_confounding, 
   tm_confounding, ldh_confounding)

#opg and sledai
opg_sledai <- glm(opg_pg_ml~sledai_score, data = OriginalData)
plot(opg_sledai, which = c(2, 4), main = 'OPG unadjusted')
summary(opg_sledai)
opg_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ sledai_score +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, which = c(2, 4), main = paste('OPG', col))
  aov_results <- anova(opg_sledai, adjusted)
  list(model = adjusted, anova = aov_results)})
names(opg_confounding) <- confounders

anova_pvals <- sapply(opg_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_opg <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_opg$significant <- anova_summary_opg$p_value < 0.05
rm(opg_sledai, opg_confounding)

##opg and biomarkers
#opg and vwf
opg_vwf <- glm(opg_pg_ml~vwf_iu_dl, data = OriginalData)
plot(opg_vwf, which = c(2,4), main = 'OPG ~ vWF unadjusted')
opg_vwf_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ vwf_iu_dl +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, which = c(2,4), main = paste('OPG ~ vWF', col))
  aov_results <- anova(opg_vwf, adjusted)
  list(model = adjusted, anova = aov_results)})
names(opg_vwf_confounding) <- confounders

anova_pvals <- sapply(opg_vwf_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_opgvwf <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_opgvwf$significant <- anova_summary_opgvwf$p_value < 0.05

#opg and sdc1
opg_sdc1 <- glm(opg_pg_ml~sdc1_ng_ml, data = OriginalData)
plot(opg_sdc1, which = c(2,4), main = 'OPG ~ SDC-1 unadjusted')
opg_sdc1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ sdc1_ng_ml +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, which = c(2,4), main = paste('OPG ~ SDC-1', col))
  aov_results <- anova(opg_sdc1, adjusted)
  list(model = adjusted, anova = aov_results)})
names(opg_sdc1_confounding) <- confounders

anova_pvals <- sapply(opg_sdc1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_opgsdc1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_opgsdc1$significant <- anova_summary_opgsdc1$p_value < 0.05

#opg and tm
opg_tm <- glm(opg_pg_ml~tm_ng_ml, data = OriginalData)
plot(opg_sdc1, which = c(2,4), main = 'OPG ~ TM unadjusted')
opg_tm_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ tm_ng_ml +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, which = c(2,4), main = paste('OPG ~ TM', col))
  aov_results <- anova(opg_tm, adjusted)
  list(model = adjusted, anova = aov_results)})
names(opg_tm_confounding) <- confounders

anova_pvals <- sapply(opg_tm_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_opgtm <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_opgtm$significant <- anova_summary_opgtm$p_value < 0.05

#opg and ox LDL
opg_oxLDL <- glm(opg_pg_ml~ox_ldl_ng_ml, data = OriginalData)
plot(opg_sdc1, which = c(2,4), main = 'OPG ~ ox-LDL unadjusted')
opg_oxLDL_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ ox_ldl_ng_ml +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, which = c(2,4), main = paste('OPG ~ ox-LDL', col))
  aov_results <- anova(opg_oxLDL, adjusted)
  list(model = adjusted, anova = aov_results)})
names(opg_oxLDL_confounding) <- confounders

anova_pvals <- sapply(opg_oxLDL_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_opgoxLDL <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_opgoxLDL$significant <- anova_summary_opgoxLDL$p_value < 0.05

#opg and svcam1
opg_svcam1 <- glm(opg_pg_ml~svcam1_ng_ml, data = OriginalData)
plot(opg_sdc1, which = c(2,4), main = 'OPG ~ sVCAM1 unadjusted')
opg_svcam1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ svcam1_ng_ml +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, which = c(2,4), main = paste('OPG ~ sVCMA1', col))
  aov_results <- anova(opg_svcam1, adjusted)
  list(model = adjusted, anova = aov_results)})
names(opg_svcam1_confounding) <- confounders

anova_pvals <- sapply(opg_svcam1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})
anova_summary_opgsvcam1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_opgsvcam1$significant <- anova_summary_opgsvcam1$p_value < 0.05

#opg and LDH
opg_ldh <- glm(opg_pg_ml~ldh_u_l, data = OriginalData)
plot(opg_sdc1, which = c(2,4), main = 'OPG ~ LDH unadjusted')
opg_ldh_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ ldh_u_l +", col))
  adjusted <- glm(fml, data = OriginalData)
  plot(adjusted, which = c(2,4), main = paste('OPG ~ LDH', col))
  aov_results <- anova(opg_ldh, adjusted)
  list(model = adjusted, anova = aov_results)})
names(opg_ldh_confounding) <- confounders

anova_pvals <- sapply(opg_ldh_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]})

anova_summary_opgldh <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals)
anova_summary_opgldh$significant <- anova_summary_opgldh$p_value < 0.05

summaries_opgbiomarkers_conf <- list(anova_summary_opgldh, anova_summary_opgoxLDL, 
                                     anova_summary_opgsdc1, anova_summary_opgsvcam1,
                                     anova_summary_opgtm, anova_summary_opgvwf)
names(summaries_opgbiomarkers_conf) <- biomarkers
rm(anova_summary_opgldh, anova_summary_opgoxLDL, anova_summary_opgsdc1, 
   anova_summary_opgsvcam1, anova_summary_opgtm, anova_summary_opgvwf, 
   opg_ldh, opg_oxLDL, opg_sdc1, opg_svcam1, opg_tm, opg_vwf, opg_ldh_confounding, 
   opg_oxLDL_confounding, opg_sdc1_confounding, opg_svcam1_confounding, 
   opg_tm_confounding, opg_vwf_confounding, anova_pvals, col, biomarkers, 
   confounders)

#now I'm going to export the data frame to make it easier for the actual data analysis
OriginalData <- read_excel("/Users/alessandramencos/BBIM01/Research Project/Research-Project-New/g1_s1_dataset_v251007.xlsx")
View(OriginalData)
OriginalData <- OriginalData[OriginalData$age_at_diagnosis_years >=18, ]
age_norm <- arcsinh_x(OriginalData$age_at_diagnosis_years)
OriginalData$age_at_diagnosis_years <- predict(age_norm)
time_norm <- orderNorm(OriginalData$time_since_diagnosis_years)
OriginalData$time_since_diagnosis_years <- predict(time_norm)
bmi_norm <- yeojohnson(OriginalData$bmi_kg_m2)
OriginalData$bmi_kg_m2 <- predict(bmi_norm)
vwf_iu_dl <- arcsinh_x(OriginalData$vwf_iu_dl)
OriginalData$vwf_iu_dl <- predict(vwf_iu_dl)
rm(age_norm, bmi_norm, time_norm, vwf_iu_dl)
for(col in biomarkers_confounders_to_log) {
  if(is.numeric(OriginalData[[col]])) {
    OriginalData[[col]] <- log2(OriginalData[[col]])
  }}
write_xlsx(OriginalData, 
           path = '/Users/alessandramencos/BBIM01/Research Project/OriginalData_norm.xlsx')