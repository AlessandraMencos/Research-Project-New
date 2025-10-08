#Alessandra Mencos
library(tidyr)
library(readxl)
library(dplyr)
OriginalData <- read_excel("~/BBIM01/Research Project/Research-Project/g1_s1_dataset_v251001.xlsx")
View(OriginalData)
##question 1: is SLEDAI score related to biomarker expression/

OriginalData$sledai_score <- cut(OriginalData$sledai_score, 
                                 breaks = c(-1, 0, 5, 10, 30), include.lowest = TRUE, 
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
summary(sdc1_sledai)
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
summary(tm_sledai)
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
summary(oxldl_sledai)
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
opg_sledai <- lm(opg_pg_ml~sledai_score, data = OriginalData)
summary(opg_sledai)
opg_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ sledai_score +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(opg_sledai, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(opg_confounding) <- confounders

anova_pvals <- sapply(opg_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_opg <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_opg$significant <- anova_summary_opg$p_value < 0.05
rm(opg_sledai, opg_confounding)

##opg and biomarkers
#opg and vwf
opg_vwf <- lm(opg_pg_ml~vwf_iu_dl, data = OriginalData)
opg_vwf_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ vwf_iu_dl +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(opg_vwf, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(opg_vwf_confounding) <- confounders

anova_pvals <- sapply(opg_vwf_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_opgvwf <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_opgvwf$significant <- anova_summary_opgvwf$p_value < 0.05

#opg and sdc1
opg_sdc1 <- lm(opg_pg_ml~sdc1_ng_ml, data = OriginalData)
opg_sdc1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ sdc1_ng_ml +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(opg_sdc1, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(opg_sdc1_confounding) <- confounders

anova_pvals <- sapply(opg_sdc1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_opgsdc1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_opgsdc1$significant <- anova_summary_opgsdc1$p_value < 0.05

#opg and tm
opg_tm <- lm(opg_pg_ml~tm_ng_ml, data = OriginalData)
opg_tm_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ tm_ng_ml +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(opg_tm, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(opg_tm_confounding) <- confounders

anova_pvals <- sapply(opg_tm_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_opgtm <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_opgtm$significant <- anova_summary_opgtm$p_value < 0.05

#opg and ox LDL
opg_oxLDL <- lm(opg_pg_ml~ox_ldl_ng_ml, data = OriginalData)
opg_oxLDL_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ ox_ldl_ng_ml +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(opg_oxLDL, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(opg_oxLDL_confounding) <- confounders

anova_pvals <- sapply(opg_oxLDL_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_opgoxLDL <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_opgoxLDL$significant <- anova_summary_opgoxLDL$p_value < 0.05

#opg and svcam1
opg_svcam1 <- lm(opg_pg_ml~svcam1_ng_ml, data = OriginalData)
opg_svcam1_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ svcam1_ng_ml +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(opg_svcam1, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(opg_svcam1_confounding) <- confounders

anova_pvals <- sapply(opg_svcam1_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_opgsvcam1 <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_opgsvcam1$significant <- anova_summary_opgsvcam1$p_value < 0.05

#opg and LDH
opg_ldh <- lm(opg_pg_ml~ldh_u_l, data = OriginalData)
opg_ldh_confounding <- lapply(confounders, function(col) {
  fml <- as.formula(paste("opg_pg_ml ~ ldh_u_l +", col))
  adjusted <- lm(fml, data = OriginalData)
  aov_results <- anova(opg_ldh, adjusted)
  list(model = adjusted, anova = aov_results)
})
names(opg_ldh_confounding) <- confounders

anova_pvals <- sapply(opg_ldh_confounding, function(res) {
  res$anova[["Pr(>F)"]][2]  # p-value from second row of ANOVA table
})
anova_summary_opgldh <- data.frame(
  confounder = names(anova_pvals),
  p_value = anova_pvals
)
anova_summary_opgldh$significant <- anova_summary_opgldh$p_value < 0.05

summaries_opgbiomarkers_conf <- list(anova_summary_opgldh, anova_summary_opgoxLDL, 
                                     anova_summary_opgsdc1, anova_summary_opgsvcam1,
                                     anova_summary_opgtm, anova_summary_opgvwf)
names(summaries_opgbiomarkers_conf) <- biomarkers
rm(anova_summary_opgldh, anova_summary_opgoxLDL, 
   anova_summary_opgsdc1, anova_summary_opgsvcam1,
   anova_summary_opgtm, anova_summary_opgvwf, opg_ldh, opg_oxLDL, opg_sdc1, opg_svcam1, 
   opg_tm, opg_vwf, opg_ldh_confounding, opg_oxLDL_confounding, opg_sdc1_confounding, 
   opg_svcam1_confounding, opg_tm_confounding, opg_vwf_confounding, anova_pvals)