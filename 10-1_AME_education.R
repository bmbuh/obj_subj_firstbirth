#Coded by: Brian Buh
#Started on: 16.05.2022
#Last Updated:


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


# -------------------------------------------------------------------------
# Education Subset - Analysis 1 Model 3 -----------------------------------
# -------------------------------------------------------------------------

#Low Education
##Men
ma1m3mlow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma1m3mlow)

##Women
ma1m3flow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma1m3flow)

#Medium Education
##Men
ma1m3mmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4mmedium))
summary(ma1m3mmedium)

##Women
ma1m3fmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4fmedium))
summary(ma1m3fmedium)

#High Education
##Men
ma1m3mhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                          family = binomial(link = "logit"),
                          data = surv4mhigh))
summary(ma1m3mhigh)

##Women
ma1m3fhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo,
                          family = binomial(link = "logit"),
                          data = surv4fhigh))
summary(ma1m3fhigh)

#Output
a1modedu <- list(ma1m3mlow, ma1m3mmedium, ma1m3mhigh, ma1m3flow, ma1m3fmedium, ma1m3fhigh)
cm1 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         "difficultDifficult" = "Finding it difficult",
         # "worseWorse" = "Worse off",
         "edulow" = "Low",
         "edumedium" = "Medium",
         "ol5catlow-skilled white-collar" = "Low-skilled White-collar",
         "ol5cathigh-skilled blue collar" = "High-skilled Blue-collar",
         "ol5catlow-skilled blue collar" = "Low-skilled Blue-collar",
         "ol5catno info" = "No info",
         "agemn" = "Age in Months",
         "agesq" = "Age Squared",
         "immigrant1"  = "Immigrant",
         "combocohab-employed" = "Cohab - Employed",
         "combocohab-non-employed" = "Cohab - Non-employed",
         "combocohab-unknown" = "Cohab - Unknown",
         "combomarried-employed" = "Married - Employed",
         "combomarried-non-employed" = "Married - Non-employed",
         "combomarried-unknown" = "Married - Unknown",
         "cci" = "CCI")
modelsummary(a1modedu, coef_map = cm1, output = "A1diff_edusubset_AME_S10-1_15-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Education Subset - Analysis 2 Model 3 -----------------------------------
# -------------------------------------------------------------------------

#Low Education
##Men
ma2m3mlow <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma2m3mlow)

##Women
ma2m3flow <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma2m3flow)

#Medium Education
##Men
ma2m3mmedium <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4mmedium))
summary(ma2m3mmedium)

##Women
ma2m3fmedium <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
                            family = binomial(link = "logit"),
                            data = surv4fmedium))
summary(ma2m3fmedium)

#High Education
##Men
ma2m3mhigh <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
                          family = binomial(link = "logit"),
                          data = surv4mhigh))
summary(ma2m3mhigh)

##Women
ma2m3fhigh <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
                          family = binomial(link = "logit"),
                          data = surv4fhigh))
summary(ma2m3fhigh)

#Output
a2modedu <- list(ma2m3mlow, ma2m3mmedium, ma2m3mhigh, ma2m3flow, ma2m3fmedium, ma2m3fhigh)
cm2 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         # "difficultDifficult" = "Finding it difficult",
         "worseWorse" = "Worse off",
         "edulow" = "Low",
         "edumedium" = "Medium",
         "ol5catlow-skilled white-collar" = "Low-skilled White-collar",
         "ol5cathigh-skilled blue collar" = "High-skilled Blue-collar",
         "ol5catlow-skilled blue collar" = "Low-skilled Blue-collar",
         "ol5catno info" = "No info",
         "agemn" = "Age in Months",
         "agesq" = "Age Squared",
         "immigrant1"  = "Immigrant",
         "combocohab-employed" = "Cohab - Employed",
         "combocohab-non-employed" = "Cohab - Non-employed",
         "combocohab-unknown" = "Cohab - Unknown",
         "combomarried-employed" = "Married - Employed",
         "combomarried-non-employed" = "Married - Non-employed",
         "combomarried-unknown" = "Married - Unknown",
         "cci" = "CCI")
modelsummary(a2modedu, coef_map = cm2, output = "A2worse_edusubset_AME_S10-1_15-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Education Subset - Analysis 3 Model 2 -----------------------------------
# -------------------------------------------------------------------------

#Low Education
##Men
ma3m2mlow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma3m2mlow)

##Women
ma3m2flow <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma3m2flow)

#Medium Education
##Men
ma3m2mmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                            family = binomial(link = "logit"),
                            data = surv4mmedium))
summary(ma3m2mmedium)

##Women
ma3m2fmedium <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                            family = binomial(link = "logit"),
                            data = surv4fmedium))
summary(ma3m2fmedium)

#High Education
##Men
ma3m2mhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                          family = binomial(link = "logit"),
                          data = surv4mhigh))
summary(ma3m2mhigh)

##Women
ma3m2fhigh <- margins(glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                          family = binomial(link = "logit"),
                          data = surv4fhigh))
summary(ma3m2fhigh)

#Output
a3m2edu <- list(ma3m2mlow, ma3m2mmedium, ma3m2mhigh, ma3m2flow, ma3m2fmedium, ma3m2fhigh)
cm3 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         "difficultDifficult" = "Finding it difficult",
         "worseWorse" = "Worse off",
         "edulow" = "Low",
         "edumedium" = "Medium",
         "ol5catlow-skilled white-collar" = "Low-skilled White-collar",
         "ol5cathigh-skilled blue collar" = "High-skilled Blue-collar",
         "ol5catlow-skilled blue collar" = "Low-skilled Blue-collar",
         "ol5catno info" = "No info",
         "agemn" = "Age in Months",
         "agesq" = "Age Squared",
         "immigrant1"  = "Immigrant",
         "incquinSecond" = "Second Quintile",
         "incquinThird" = "Third Quintile",
         "incquinFourth" = "Fourth Quintile",
         "incquinFifth" = "Fifth Quintile",
         "combocohab-employed" = "Cohab - Employed",
         "combocohab-non-employed" = "Cohab - Non-employed",
         "combocohab-unknown" = "Cohab - Unknown",
         "combomarried-employed" = "Married - Employed",
         "combomarried-non-employed" = "Married - Non-employed",
         "combomarried-unknown" = "Married - Unknown",
         "cci" = "CCI")
modelsummary(a3m2edu, coef_map = cm3, output = "A3diffinc_edusubset_AME_S10-1_15-05-2022.html", stars = TRUE)


# -------------------------------------------------------------------------
# Education Subset - Analysis 3 Model 4 -----------------------------------
# -------------------------------------------------------------------------

#Low Education
##Men
ma3m4mlow <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4mlow))
summary(ma3m4mlow)

##Women
ma3m4flow <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5cat + cci + combo + incquin,
                         family = binomial(link = "logit"),
                         data = surv4flow))
summary(ma3m4flow)

#Medium Education
##Men
ma3m4mmedium <- margins(glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + ol5