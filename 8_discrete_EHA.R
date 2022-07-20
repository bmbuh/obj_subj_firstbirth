#Coded by: Brian Buh
#Started on: 20.07.2022
#Last Updated: 

# Note: this script continues the model changes after the feedback from eval.parent
# See the script "8_discrete_EHA_draft1" for the first run of the script

# install.packages("interactions")
# install.packages ("huxtable")

library(tidyverse)
# library(sjPlot)
library(janitor)
library(jtools)
library(effects)
library(interactions)
library(huxtable) #needed for export_summs command

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")


###########################################################################
# Discrete Time Hazard Model ----------------------------------------------
###########################################################################

# My modeling strategy will looks like this:
## (Also stratified by sex)

# Analysis 1 - Present Financial Situation
## 1. Objective employment conditions (OEC) + present financial situation + controls
## 2. OEC + present financial situation + partneremp + partnership status + controls
## 3. OEC*partneremp + present financial situation + partnership status + controls

# Analysis 2 - Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Future financial situation + controls + CCI
## 3. Model 2 + Partner variables

# Analysis 3 - Income
## 1. Analysis 1 Model 2 + Income Quintile
## 2. Analysis 1 Model 3 + Income Quintile
## 3. Analysis 2 Model 2 + Income Quintile
## 4. Analysis 2 Model 3 + Income Quintile


##########################################################################
# Analysis 1 Finnow-------------------------------------------------------
##########################################################################

# ------------------------------------------------------------------------------------------------------
# Model 1. Objective employment conditions (OEC) + present financial situation + controls --------------
# ------------------------------------------------------------------------------------------------------

a1m1m <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a1m1m)

summ(a1m1m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m1m, exp = T)
plot_model(a1m1m, type = "pred", terms = c("empstat2", "edu"))

a1m1f <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a1m1f)
summ(a1m1f, exp = TRUE)
plot_summs(a1m1f, exp = T)

plot_summs(a1m1m, a1m1f, exp = T)

# ---------------------------------------------------------------------------------------
# Model 2. OEC + present financial situation + partneremp + partnership status + controls
# ---------------------------------------------------------------------------------------

a1m2m <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a1m2m)
summ(a1m2m, exp = TRUE)
plot_summs(a1m2m, exp = T)

a1m2f <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a1m2f)
summ(a1m2f, exp = TRUE)
plot_summs(a1m2f, exp = T)

plot_summs(a1m2m, a1m2f, exp = T)

# -------------------------------------------------------------------------------------
# Model 3. OEC*partneremp + present financial situation + partnership status + controls
# -------------------------------------------------------------------------------------

a1m3m <- glm(formula = event ~ t3 + empstat2*parjbstat + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a1m3m)
summ(a1m3m, exp = TRUE) 
plot_summs(a1m3m, exp = T)

a1m3f <- glm(formula = event ~ t3 + empstat2*parjbstat + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a1m3f)
summ(a1m3f, exp = TRUE)
plot_summs(a1m3f, exp = T)

plot_summs(a1m3m, a1m3f, exp = T)

# -------------------------------------------------------------------------
# Analysis 1 - All plots Odds Ratios --------------------------------------
# -------------------------------------------------------------------------

export_summs(a1m1m, a1m2m, a1m3m, a1m1f, a1m2f, a1m3f,
             model.names = c("Men 1", "Men 2", "Men 3", "Women 1", "Women 2", "Women 3"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t3",
             #           "Part-time" = "empstat2part time",
             #           "Self-employed" = "empstat2self-employed",
             #           "Unemployment" = "empstat2unemployment",
             #           "Out of the LF" = "empstat2out of LF",
             #           "Finding it difficult" = "difficultDifficult",
             #           # "Worse off" = "worseWorse",
             #           "Low" = "edulow",
             #           "Medium" = "edumedium",
             #           "Low-skilled White-collar" = "ol5catlow-skilled white-collar",
             #           "High-skilled Blue-collar" = "ol5cathigh-skilled blue collar",
             #           "Low-skilled Blue-collar" = "ol5catlow-skilled blue collar",
             #           "No info" = "ol5catno info",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "Immigrant" = "immigrant1",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "CCI" = "cci"),
             exp = TRUE,
             to.file = "html",
             file.name = "A1_OR_S8_20-07-2022.html")

##########################################################################
# Analysis 2 Better-------------------------------------------------------
##########################################################################

# ------------------------------------------------------------------------------------------------------
# Model 1. Objective employment conditions (OEC) + present financial situation + controls --------------
# ------------------------------------------------------------------------------------------------------

a2m1m <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + edu + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a2m1m)

summ(a2m1m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m1m, exp = T)
plot_model(a2m1m, type = "pred", terms = c("empstat2", "edu"))

a2m1f <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + edu + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a2m1f)
summ(a2m1f, exp = TRUE)
plot_summs(a2m1f, exp = T)

plot_summs(a2m1m, a2m1f, exp = T)

# ---------------------------------------------------------------------------------------
# Model 2. OEC + present financial situation + partneremp + partnership status + controls
# ---------------------------------------------------------------------------------------

a2m2m <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a2m2m)
summ(a2m2m, exp = TRUE)
plot_summs(a2m2m, exp = T)

a2m2f <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a2m2f)
summ(a2m2f, exp = TRUE)
plot_summs(a2m2f, exp = T)

plot_summs(a2m2m, a2m2f, exp = T)

# -------------------------------------------------------------------------------------
# Model 3. OEC*partneremp + present financial situation + partnership status + controls
# -------------------------------------------------------------------------------------

a2m3m <- glm(formula = event ~ t3 + empstat2*parjbstat + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a2m3m)
summ(a2m3m, exp = TRUE) 
plot_summs(a2m3m, exp = T)

a2m3f <- glm(formula = event ~ t3 + empstat2*parjbstat + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a2m3f)
summ(a2m3f, exp = TRUE)
plot_summs(a2m3f, exp = T)

plot_summs(a2m3m, a2m3f, exp = T)

# -------------------------------------------------------------------------
# Analysis 2 - All plots Odds Ratios --------------------------------------
# -------------------------------------------------------------------------

export_summs(a2m1m, a2m2m, a2m3m, a2m1f, a2m2f, a2m3f,
             model.names = c("Men 1", "Men 2", "Men 3", "Women 1", "Women 2", "Women 3"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t3",
             #           "Part-time" = "empstat2part time",
             #           "Self-employed" = "empstat2self-employed",
             #           "Unemployment" = "empstat2unemployment",
             #           "Out of the LF" = "empstat2out of LF",
             #           # "Finding it difficult" = "difficultDifficult",
             #           "Worse off" = "worseWorse",
             #           "Low" = "edulow",
             #           "Medium" = "edumedium",
             #           "Low-skilled White-collar" = "ol5catlow-skilled white-collar",
             #           "High-skilled Blue-collar" = "ol5cathigh-skilled blue collar",
             #           "Low-skilled Blue-collar" = "ol5catlow-skilled blue collar",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "Immigrant" = "immigrant1",
             #           "No info" = "ol5catno info",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "CCI" = "cci"),
             exp = TRUE,
             to.file = "html",
             file.name = "A2_OR_S8_20-07-2022.html")

##########################################################################
# Analysis 3 finnow + finfut ---------------------------------------------
##########################################################################

# -------------------------------------------------------------------------
# 1. Analysis 1 Model 3. + Income --------------------------------------------
# -------------------------------------------------------------------------

a3m1m <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat + incquin,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m1m)
summ(a3m1m, exp = TRUE)
plot_summs(a3m1m, exp = T)

a3m1f <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat + incquin,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m1f)
summ(a3m1f, exp = TRUE)
plot_summs(a3m1f, exp = T)

plot_summs(a3m1m, a3m1f, exp = T)

# -------------------------------------------------------------------------
# 2. Analysis 1 Model 4. + Income -----------------------------------------
# -------------------------------------------------------------------------

a3m2m <- glm(formula = event ~ t3 + empstat2*parjbstat + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + incquin,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m2m)
summ(a3m2m, exp = TRUE)
plot_summs(a3m2m, exp = T)

a3m2f <- glm(formula = event ~ t3 + empstat2*parjbstat + difficult + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + incquin,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m2f)
summ(a3m2f, exp = TRUE)
plot_summs(a3m2f, exp = T)

plot_summs(a3m2m, a3m2f, exp = T)

# -------------------------------------------------------------------------
# 3. Analysis 2 Model 3. + Income -----------------------------------------
# -------------------------------------------------------------------------

a3m3m <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat + incquin,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m3m)
summ(a3m3m, exp = TRUE)
plot_summs(a3m3m, exp = T)

a3m3f <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + parjbstat + incquin,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m3f)
summ(a3m3f, exp = TRUE)
plot_summs(a3m3f, exp = T)

plot_summs(a3m3m, a3m3f, exp = T)

# -------------------------------------------------------------------------
# 4. Analysis 2 Model 4. + Income -----------------------------------------
# -------------------------------------------------------------------------

a3m4m <- glm(formula = event ~ t3 + empstat2*parjbstat + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + incquin,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m4m)
summ(a3m4m, exp = TRUE)
plot_summs(a3m4m, exp = T)

a3m4f <- glm(formula = event ~ t3 + empstat2*parjbstat + better + agemn + agesq + immigrant + edu + ol5cat + cci + marstat + incquin,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m4f)
summ(a3m4f, exp = TRUE)
plot_summs(a3m4f, exp = T)

plot_summs(a3m4m, a3m4f, exp = T)


# -------------------------------------------------------------------------
# Analysis 3 - All plots Odds Ratios --------------------------------------
# -------------------------------------------------------------------------

export_summs(a3m1m, a3m2m, a3m3m, a3m4m, a3m1f, a3m2f, a3m3f, a3m4f,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t3",
             #           "Part-time" = "empstat2part time",
             #           "Self-employed" = "empstat2self-employed",
             #           "Unemployment" = "empstat2unemployment",
             #           "Out of the LF" = "empstat2out of LF",
             #           "Finding it difficult" = "difficultDifficult",
             #           "Worse off" = "worseWorse",
             #           "Low" = "edulow",
             #           "Medium" = "edumedium",
             #           "Low-skilled White-collar" = "ol5catlow-skilled white-collar",
             #           "High-skilled Blue-collar" = "ol5cathigh-skilled blue collar",
             #           "Low-skilled Blue-collar" = "ol5catlow-skilled blue collar",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "Immigrant" = "immigrant1",
             #           "No info" = "ol5catno info",
             #           "Second Quintile" = "incquinSecond",
             #           "Third Quintile" = "incquinThird",
             #           "Fourth Quintile" = "incquinFourth",
             #           "Fifth Quintile" = "incquinFifth",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "CCI" = "cci"),
             exp = TRUE,
             to.file = "html",
             file.name = "A3_OR_S8_20-07-2022.html")



##########################################################################
# Final OR tables --------------------------------------------------------
##########################################################################


# -------------------------------------------------------------------------
# All plots Odds Ratios - As in manuscript  -------------------------------
# -------------------------------------------------------------------------

# difficult
export_summs(a1m1m, a1m2m, a1m3m, a3m2m, a1m1f, a1m2f, a1m3f, a3m2f,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t3",
             #           "Part-time" = "empstat2part time",
             #           "Self-employed" = "empstat2self-employed",
             #           "Unemployment" = "empstat2unemployment",
             #           "Out of the LF" = "empstat2out of LF",
             #           "Finding it difficult" = "difficultDifficult",
             #           "Worse off" = "worseWorse",
             #           "Low" = "edulow",
             #           "Medium" = "edumedium",
             #           "Low-skilled White-collar" = "ol5catlow-skilled white-collar",
             #           "High-skilled Blue-collar" = "ol5cathigh-skilled blue collar",
             #           "Low-skilled Blue-collar" = "ol5catlow-skilled blue collar",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "Immigrant" = "immigrant1",
             #           "No info" = "ol5catno info",
             #           "Second Quintile" = "incquinSecond",
             #           "Third Quintile" = "incquinThird",
             #           "Fourth Quintile" = "incquinFourth",
             #           "Fifth Quintile" = "incquinFifth",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "CCI" = "cci"),
             exp = TRUE,
             to.file = "html",
             file.name = "all_diff_OR_S8_20-07-2022.html")

# better
export_summs(a2m1m, a2m2m, a2m3m, a3m4m, a2m1f, a2m2f, a2m3f, a3m4f,
             model.names = c("Men 1", "Men 2", "Men 3", "Men 4", "Women 1", "Women 2", "Women 3", "Women 4"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, '+' = 0.1), 
             # coefs = c("Time since Education" = "t3",
             #           "Part-time" = "empstat2part time",
             #           "Self-employed" = "empstat2self-employed",
             #           "Unemployment" = "empstat2unemployment",
             #           "Out of the LF" = "empstat2out of LF",
             #           "Finding it difficult" = "difficultDifficult",
             #           "Worse off" = "worseWorse",
             #           "Low" = "edulow",
             #           "Medium" = "edumedium",
             #           "Low-skilled White-collar" = "ol5catlow-skilled white-collar",
             #           "High-skilled Blue-collar" = "ol5cathigh-skilled blue collar",
             #           "Low-skilled Blue-collar" = "ol5catlow-skilled blue collar",
             #           "Age in Months" = "agemn",
             #           "Age Squared" = "agesq",
             #           "Immigrant" = "immigrant1",
             #           "No info" = "ol5catno info",
             #           "Second Quintile" = "incquinSecond",
             #           "Third Quintile" = "incquinThird",
             #           "Fourth Quintile" = "incquinFourth",
             #           "Fifth Quintile" = "incquinFifth",
             #           "Cohab - Employed" = "combocohab-employed",
             #           "Cohab - Non-employed" = "combocohab-non-employed",
             #           "Cohab - Unknown" = "combocohab-unknown",
             #           "Married - Employed" = "combomarried-employed",
             #           "Married - Non-employed" = "combomarried-non-employed",
             #           "Married - Unknown" = "combomarried-unknown",
             #           "CCI" = "cci"),
             exp = TRUE,
             to.file = "html",
             file.name = "all_better_OR_S8_20-07-2022.html")






