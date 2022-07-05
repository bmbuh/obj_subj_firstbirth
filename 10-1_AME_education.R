#Coded by: Brian Buh
#Started on: 16.05.2022
#Last Updated: 30.06.2022


library(margins)
library(data.table)
library(tidyverse)
library(haven)
library(jtools)
library(effects)
library(interactions)
library(modelsummary)
# library(mfx)
library(gt) #Use tab functions for outputs
library(sjPlot) #Use for the plot_model
library(huxtable)
library(openxlsx) #For priting huxtable outputs in xlsx

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")


###########################################################################
# Subsetting Education ----------------------------------------------------
###########################################################################

#Using the 'at' command to extract MER/MEM
### NOTE: This does not find the subsample AME!
ma1m3medu <- margins(a1m3m, at = list(edunum = 1:3))
summary(ma1m3medu)

#Unlike STATA, in R, to find subsample AME we need to subset the sample directly

##Option 1: Using the split command to subset the output of the martgins command
split(ma1m3m, ma1m3m$edu)
#Hard to use for an output

##Option 2: create separate df and calculate AME with margins

###Men
surv4mlow <- surv4m %>% filter(edu == "low")
surv4mmedium <- surv4m %>% filter(edu == "medium")
surv4mhigh <- surv4m %>% filter(edu == "high")

###Women
surv4flow <- surv4f %>% filter(edu == "low")
surv4fmedium <- surv4f %>% filter(edu == "medium")
surv4fhigh <- surv4f %>% filter(edu == "high")

surv4mlow %>% count(better)

# -------------------------------------------------------------------------
# Education Sub samples ---------------------------------------------------
# -------------------------------------------------------------------------

# I create separate samples for each 
## All Analysis are sex-specific
### I look only at the models with partnership as it is clear that partnership has an important mediating effect
### However, after adding partnership, it lowers predicted probabilities but doesn't change th shape of relationship

# Analysis 3 - Women Present Financial Situation
## 1. OEC + Present financial situation + controls + CCI + Partner variables
## 2. Model 1 + Income Quintile

# Analysis 4 - Men Present Financial Situation
## 1. OEC + Future financial situation + controls + CCI + Partner variables
## 2. Model 1 + Income Quintile

# Analysis 5 - Women Future Financial Situation
## 1. OEC + Present financial situation + controls + CCI + Partner variables
## 2. Model 1 + Income Quintile

# Analysis 6 - Men Future Financial Situation
## 1. OEC + Future financial situation + controls + CCI + Partner variables
## 2. Model 1 + Income Quintile

###########################################################################
# Analysis ----------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Education Subset - Analysis 3 Women -------------------------------------
# -------------------------------------------------------------------------

#Low Education
##Women
###Model 1
ma3m1flow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma3m1flow)
###Model 2
ma3m2flow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma3m2flow)

#Medium Education
##Women
###Model 1
ma3m1fmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4fmedium))
summary(ma3m1fmedium)
###Model 2
ma3m2fmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4fmedium))
summary(ma3m2fmedium)

#High Education
##Women
###Model 1
ma3m1fhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4fhigh))
summary(ma3m1fhigh)
###Model 2
ma3m2fhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                            family = binomial(link = "logit"),
                            data = surv4fhigh))
summary(ma3m2fhigh)

#Output
a3modedu <- list(ma3m1flow,ma3m2flow,ma3m1fmedium,ma3m2fmedium,ma3m1fhigh,ma3m2fhigh)
cm4 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         "difficultDifficult" = "Difficult",
         # "worseWorse" = "Worse",
         "edulow" = "Low",
         "edumedium" = "Medium",
         "ol5catlow-skilled white-collar" = "Low-skilled White-collar",
         "ol5cathigh-skilled blue collar" = "High-skilled Blue-collar",
         "ol5catlow-skilled blue collar" = "Low-skilled Blue-collar",
         "ol5catno info" = "No info",
         "agemn" = "Age in Months",
         "agesq" = "Age Squared",
         "immigrant"  = "Immigrant",
         "cci" = "CCI",
         "combocohab-employed" = "Cohab - Employed",
         "combocohab-non-employed" = "Cohab - Non-employed",
         "combocohab-unknown" = "Cohab - Unknown",
         "combomarried-employed" = "Married - Employed",
         "combomarried-non-employed" = "Married - Non-employed",
         "combomarried-unknown" = "Married - Unknown",
         "incquinSecond" = "Second Quintile",
         "incquinThird" = "Third Quintile",
         "incquinFourth" = "Fourth Quintile",
         "incquinFifth" = "Fifth Quintile")
modelsummary(a3modedu, coef_map = cm3, output = "A3diff_womenedusubset_AME_S10-1_23-05-2022.html", stars = TRUE)
a3diff_women <- modelsummary(a3modedu, coef_map = cm3, output = "huxtable", stars = TRUE)
quick_docx(a3diff_women, file = "A3diff_womenedusubset_AME_S10-1_23-05-2022.docx", open = FALSE)
quick_xlsx(a3diff_women, file = "A3diff_womenedusubset_AME_S10-1_23-05-2022.xlsx", open = FALSE)


# -------------------------------------------------------------------------
# Education Subset - Analysis 4 Men -------------------------------------
# -------------------------------------------------------------------------

#Low Education
##Men
###Model 1
ma4m1mlow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma4m1mlow)
###Model 2
ma4m2mlow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma4m2mlow)

#Medium Education
##Men
###Model 1
ma4m1mmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4mmedium))
summary(ma4m1mmedium)
###Model 2
ma4m2mmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                            family = binomial(link = "logit"),
                            data = surv4mmedium))
summary(ma4m2mmedium)

#High Education
##Men
###Model 1
ma4m1mhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                          family = binomial(link = "logit"),
                          data = surv4mhigh))
summary(ma4m1mhigh)
###Model 2
ma4m2mhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                          family = binomial(link = "logit"),
                          data = surv4mhigh))
summary(ma4m2mhigh)

#Output
a4modedu <- list(ma4m1mlow,ma4m2mlow,ma4m1mmedium,ma4m2mmedium,ma4m1mhigh,ma4m2mhigh)
cm3 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         "difficultDifficult" = "Difficult",
         # "worseWorse" = "Worse",
         "edulow" = "Low",
         "edumedium" = "Medium",
         "ol5catlow-skilled white-collar" = "Low-skilled White-collar",
         "ol5cathigh-skilled blue collar" = "High-skilled Blue-collar",
         "ol5catlow-skilled blue collar" = "Low-skilled Blue-collar",
         "ol5catno info" = "No info",
         "agemn" = "Age in Months",
         "agesq" = "Age Squared",
         "immigrant"  = "Immigrant",
         "cci" = "CCI", 
         "combocohab-employed" = "Cohab - Employed",
         "combocohab-non-employed" = "Cohab - Non-employed",
         "combocohab-unknown" = "Cohab - Unknown",
         "combomarried-employed" = "Married - Employed",
         "combomarried-non-employed" = "Married - Non-employed",
         "combomarried-unknown" = "Married - Unknown",
         "incquinSecond" = "Second Quintile",
         "incquinThird" = "Third Quintile",
         "incquinFourth" = "Fourth Quintile",
         "incquinFifth" = "Fifth Quintile")
modelsummary(a4modedu, coef_map = cm3, output = "A4diff_menedusubset_AME_S10-1_23-05-2022.html", stars = TRUE)
a4diff_men <- modelsummary(a4modedu, coef_map = cm3, output = "huxtable", stars = TRUE)
quick_docx(a4diff_men, file = "A4diff_menedusubset_AME_S10-1_23-05-2022.docx", open = FALSE)
quick_xlsx(a4diff_men, file = "A4diff_menedusubset_AME_S10-1_23-05-2022.xlsx", open = FALSE)




# -------------------------------------------------------------------------
# Education Subset - Analysis 5 Women better ------------------------------
# -------------------------------------------------------------------------

#Low Education
##Women
###Model 1
ma5m1flow <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma5m1flow)
###Model 2
ma5m2flow <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma5m2flow)

#Medium Education
##Women
###Model 1
ma5m1fmedium <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4fmedium))
summary(ma5m1fmedium)
###Model 2
ma5m2fmedium <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                            family = binomial(link = "logit"),
                            data = surv4fmedium))
summary(ma5m2fmedium)

#High Education
##Women
###Model 1
ma5m1fhigh <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo,
                          family = binomial(link = "logit"),
                          data = surv4fhigh))
summary(ma5m1fhigh)
###Model 2
ma5m2fhigh <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                          family = binomial(link = "logit"),
                          data = surv4fhigh))
summary(ma5m2fhigh)

#Output
a5modedu <- list(ma5m1flow,ma5m2flow,ma5m1fmedium,ma5m2fmedium,ma5m1fhigh,ma5m2fhigh)
cm4 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         # "difficultDifficult" = "Difficult",
         "betterSame or worse" = "Same or worse",
         "edulow" = "Low",
         "edumedium" = "Medium",
         "ol5catlow-skilled white-collar" = "Low-skilled White-collar",
         "ol5cathigh-skilled blue collar" = "High-skilled Blue-collar",
         "ol5catlow-skilled blue collar" = "Low-skilled Blue-collar",
         "ol5catno info" = "No info",
         "agemn" = "Age in Months",
         "agesq" = "Age Squared",
         "immigrant"  = "Immigrant",
         "cci" = "CCI",
         "combocohab-employed" = "Cohab - Employed",
         "combocohab-non-employed" = "Cohab - Non-employed",
         "combocohab-unknown" = "Cohab - Unknown",
         "combomarried-employed" = "Married - Employed",
         "combomarried-non-employed" = "Married - Non-employed",
         "combomarried-unknown" = "Married - Unknown",
         "incquinSecond" = "Second Quintile",
         "incquinThird" = "Third Quintile",
         "incquinFourth" = "Fourth Quintile",
         "incquinFifth" = "Fifth Quintile")
modelsummary(a5modedu, coef_map = cm4, output = "A5better_womenedusubset_AME_S10-1_30-06-2022.html", stars = TRUE)
a5better_women <- modelsummary(a5modedu, coef_map = cm4, output = "huxtable", stars = TRUE)
quick_docx(a5better_women, file = "A5better_womenedusubset_AME_S10-1_30-06-2022.docx", open = FALSE)
quick_xlsx(a5better_women, file = "A5better_womenedusubset_AME_S10-1_30-06-2022.xlsx", open = FALSE)


# -------------------------------------------------------------------------
# Education Subset - Analysis 6 Men better  --------------------------------
# -------------------------------------------------------------------------

#Low Education
##Men
###Model 1
ma6m1mlow <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma6m1mlow)
###Model 2
ma6m2mlow <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma6m2mlow)

#Medium Education
##Men
###Model 1
ma6m1mmedium <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4mmedium))
summary(ma6m1mmedium)
###Model 2
ma6m2mmedium <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                            family = binomial(link = "logit"),
                            data = surv4mmedium))
summary(ma6m2mmedium)

#High Education
##Men
###Model 1
ma6m1mhigh <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo,
                          family = binomial(link = "logit"),
                          data = surv4mhigh))
summary(ma6m1mhigh)
###Model 2
ma6m2mhigh <- margins(glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                          family = binomial(link = "logit"),
                          data = surv4mhigh))
summary(ma6m2mhigh)

#Output
a6modedu <- list(ma6m1mlow,ma6m2mlow,ma6m1mmedium,ma6m2mmedium,ma6m1mhigh,ma6m2mhigh)
cm4 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         # "difficultDifficult" = "Difficult",
         "betterSame or worse" = "Same or worse",
         "edulow" = "Low",
         "edumedium" = "Medium",
         "ol5catlow-skilled white-collar" = "Low-skilled White-collar",
         "ol5cathigh-skilled blue collar" = "High-skilled Blue-collar",
         "ol5catlow-skilled blue collar" = "Low-skilled Blue-collar",
         "ol5catno info" = "No info",
         "agemn" = "Age in Months",
         "agesq" = "Age Squared",
         "immigrant"  = "Immigrant",
         "cci" = "CCI", 
         "combocohab-employed" = "Cohab - Employed",
         "combocohab-non-employed" = "Cohab - Non-employed",
         "combocohab-unknown" = "Cohab - Unknown",
         "combomarried-employed" = "Married - Employed",
         "combomarried-non-employed" = "Married - Non-employed",
         "combomarried-unknown" = "Married - Unknown",
         "incquinSecond" = "Second Quintile",
         "incquinThird" = "Third Quintile",
         "incquinFourth" = "Fourth Quintile",
         "incquinFifth" = "Fifth Quintile")
modelsummary(a6modedu, coef_map = cm4, output = "A6better_menedusubset_AME_S10-1_30-06-2022.html", stars = TRUE)
a6better_men <- modelsummary(a6modedu, coef_map = cm4, output = "huxtable", stars = TRUE)
quick_docx(a6better_men, file = "a6better_menedusubset_AME_S10-1_30-06-2022.docx", open = FALSE)
quick_xlsx(a6better_men, file = "a6better_menedusubset_AME_S10-1_30-06-2022.xlsx", open = FALSE)

