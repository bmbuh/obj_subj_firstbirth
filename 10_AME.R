#Coded by: Brian Buh
#Started on: 09.03.2022
#Last Updated: 12.05.2022

##NOTE: CHANGE "fINDING IT DIFFICULT" TO "dIFFCULT'

# install.packages("margins")
# install.packages("modelsummary")
# install.packages("mfx")
# install.packages("gt")

library(margins)
library(data.table)
library(tidyverse)
library(haven)
# library(lubridate)
# library(arsenal)
# library(janitor)
library(jtools)
library(effects)
library(interactions)
# library(stargazer)
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
# Discrete Time Hazard Model ----------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Full Sample -------------------------------------------------------------
# -------------------------------------------------------------------------

# My AME strategy follows the strategy of Script 8
## All Analysis are sex-specific

# Analysis 1 - Present Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + present financial situation + controls + CCI
## 3. Model 2 + Partner variables

# Analysis 2 - Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Future financial situation + controls + CCI
## 3. Model 2 + Partner variables

# Analysis 3 - Income
## 1. Analysis 1 Model 2 + Income Quintile
## 2. Analysis 1 Model 3 + Income Quintile
## 3. Analysis 2 Model 2 + Income Quintile
## 4. Analysis 2 Model 3 + Income Quintile

# -------------------------------------------------------------------------
# Education Sub samples ---------------------------------------------------
# -------------------------------------------------------------------------

# I create separate samples for each 
## All Analysis are sex-specific
### I look only at the models with partnership as it is clear that partnership has an important mediating effect
### Thus, Analysis 3 is A1 + A2 with income quintiles

# Analysis 1 - Present Financial Situation
## 3. OEC + Present financial situation + controls + CCI + Partner variables

# Analysis 2 - Future Financial Situation
## 3. OEC + Future financial situation + controls + CCI + Partner variables

# Analysis 3 - Income
## 2. Analysis 1 Model 3 + Income Quintile
## 4. Analysis 2 Model 3 + Income Quintile

###########################################################################
# Analysis 1 --------------------------------------------------------------
###########################################################################

cplot(a1m3m, "empstat2")
cplot(a1m3m, "difficult")
      
cplot(a1m3f, "empstat2")
cplot(a1m3f, "difficult")

# -------------------------------------------------------------------------
# Analysis 1 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

ma1m1m <- margins(a1m1m)
summary(ma1m1m)
# plot(ma1m1m, which = colnames(edu))
plot_model(a1m1m, type = "pred", terms = "edu")

ma1m2m <- margins(a1m2m)
summary(ma1m2m)
plot(ma1m2m)

ma1m3m <- margins(a1m3m)
summary(ma1m3m)
plot(ma1m3m)


#modelsummary gives me AME output (as matched with the summary command)
a1modm <- list(ma1m1m,ma1m2m,ma1m3m)
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
modelsummary(a1modm, coef_map = cm1, output = "A1diff_Men_AME_S10_12-05-2022.html", stars = TRUE) 

# %>% 
  # tab_spanner(label = "Men", columns = c("Model 1", "Model 2", "Model 3"))

# -------------------------------------------------------------------------
# Analysis 1 Women --------------------------------------------------------
# -------------------------------------------------------------------------

ma1m1f <- margins(a1m1f)
summary(ma1m1f)
plot(ma1m1f)


ma1m2f <- margins(a1m2f)
summary(ma1m2f)
plot(ma1m2f)

ma1m3f <- margins(a1m3f)
summary(ma1m3f)
plot(ma1m3f)


a1modf <- list(ma1m1f,ma1m2f,ma1m3f)
modelsummary(a1modf, coef_map = cm1, output = "A1diff_Women_AME_S10_12-05-2022.html", stars = TRUE)

###########################################################################
# Analysis 2 --------------------------------------------------------------
###########################################################################

cplot(a2m1m, "empstat2")
cplot(a2m2m, "worse")


# -------------------------------------------------------------------------
# Analysis 2 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

ma2m1m <- margins(a2m1m)
summary(ma2m1m)
plot(ma2m1m)

ma2m2m <- margins(a2m2m)
summary(ma2m2m)
plot(ma2m2m)

ma2m3m <- margins(a2m3m)
summary(ma2m3m)
plot(ma2m3m)

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

a2modm <- list(ma2m1m,ma2m2m,ma2m3m)
modelsummary(a2modm, coef_map = cm2, output = "A2worse_Men_AME_S10_12-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Analysis 2 Women --------------------------------------------------------
# -------------------------------------------------------------------------

ma2m1f <- margins(a2m1f)
summary(ma2m1f)
plot(ma2m1f)

ma2m2f <- margins(a2m2f)
summary(ma2m2f)
plot(ma2m2f)

ma2m3f <- margins(a2m3f)
summary(ma2m3f)
plot(ma2m3f)

a2modf <- list(ma2m1f,ma2m2f,ma2m3f)
modelsummary(a2modf, coef_map = cm2, output = "A2worse_Women_AME_S10_12-05-2022.html", stars = TRUE)

###########################################################################
# Analysis 3 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 3 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

ma3m1m <- margins(a3m1m)
summary(ma3m1m)
plot(ma3m1m)

ma3m2m <- margins(a3m2m)
summary(ma3m2m)
plot(ma3m2m)

ma3m3m <- margins(a3m3m)
summary(ma3m3m)
plot(ma3m3m)

ma3m4m <- margins(a3m4m)
summary(ma3m4m)
plot(ma3m4m)

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

a3modm <- list(ma3m1m,ma3m2m,ma3m3m,ma3m4m)
modelsummary(a3modm, coef_map = cm3, output = "A3inc_Men_AME_S10_12-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Analysis 3 Women --------------------------------------------------------
# -------------------------------------------------------------------------

ma3m1f <- margins(a3m1f)
summary(ma3m1f)
plot(ma3m1f)

ma3m2f <- margins(a3m2f)
summary(ma3m2f)
plot(ma3m2f)

ma3m3f <- margins(a3m3f)
summary(ma3m3f)
plot(ma3m3f)

ma3m4f <- margins(a3m4f)
summary(ma3m4f)
plot(ma3m4f)

a3modf <- list(ma3m1f,ma3m2f,ma3m3f,ma3m4f)
modelsummary(a3modf, coef_map = cm3, output = "A3inc_Women_AME_S10_11-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Combined Analysis -------------------------------------------------------
# -------------------------------------------------------------------------

a3mod <- list(ma3m1m,ma3m2m,ma3m3m,ma3m4m, ma3m1f,ma3m2f,ma3m3f,ma3m4f)
modelsummary(a3mod, coef_map = cm3, output = "A3inc_bothsexes_AME_S10_11-05-2022.html", stars = TRUE)


