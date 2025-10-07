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
OriginalData <- OriginalData %>% mutate(ethnicity = factor(ethnicity))
OriginalData <- OriginalData %>% mutate(menopausal_status = factor(menopausal_status))

#test each confounder with the linear model
#If the coefficient of disease_activity changes by >10–15%, consider the variable a confounder. 

biomarkers <- c('vwf_iu_dl', 'sdc1_ng_ml', 'tm_ng_ml', 'ox_ldl_ng_ml', 'svcam1_ng_ml', 'ldh_u_l')
confounders <- c('age_at_diagnosis_years', 'time_since_diagnosis_years', 'age_years', 
                 'bmi_kg_m2', 'ifn_type1_iu_ml', 'ethnicity', 'menopausal_status')

##VWF
vwf_sledai <- lm(vwf_iu_dl~sledai_score, data = OriginalData)
summary(vwf_sledai)
vwf_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("vwf_iu_dl ~ sledai_score +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(vwf_sledai, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(vwf_confounding) <- confounders

anova_pvals <- sapply(vwf_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_vwf <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_vwf$significant <- anova_summary_vwf$p_value < 0.05

##sdc1
sdc1_sledai <- lm(sdc1_ng_ml~sledai_score, data = OriginalData)
summary(vwf_sledai)
sdc1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("sdc1_ng_ml ~ sledai_score +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(sdc1_sledai, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(sdc1_confounding) <- confounders

anova_pvals <- sapply(sdc1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_sdc1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_sdc1$significant <- anova_summary_sdc1$p_value < 0.05

#tm
tm_sledai <- lm(tm_ng_ml~sledai_score, data = OriginalData)
summary(vwf_sledai)
tm_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("tm_ng_ml ~ sledai_score +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(tm_sledai, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(tm_confounding) <- confounders

anova_pvals <- sapply(tm_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_tm <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_tm$significant <- anova_summary_tm$p_value < 0.05

#oxldl
oxldl_sledai <- lm(ox_ldl_ng_ml~sledai_score, data = OriginalData)
summary(vwf_sledai)
oxldl_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("ox_ldl_ng_ml ~ sledai_score +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(oxldl_sledai, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(oxldl_confounding) <- confounders

anova_pvals <- sapply(oxldl_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_oxldl <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_oxldl$significant <- anova_summary_oxldl$p_value < 0.05

#svcam1
svcam1_sledai <- lm(svcam1_ng_ml~sledai_score, data = OriginalData)
summary(vwf_sledai)
svcam1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("svcam1_ng_ml ~ sledai_score +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(svcam1_sledai, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(svcam1_confounding) <- confounders

anova_pvals <- sapply(svcam1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_svcam1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_svcam1$significant <- anova_summary_svcam1$p_value < 0.05

#ldh
ldh_sledai <- lm(ldh_u_l~sledai_score, data = OriginalData)
summary(ldh_sledai)
ldh_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("ldh_u_l ~ sledai_score +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(ldh_sledai, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(ldh_confounding) <- confounders

anova_pvals <- sapply(ldh_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_ldh <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_ldh$significant <- anova_summary_ldh$p_value < 0.05
