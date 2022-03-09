#Coded by: Brian Buh
#Started on: 08.03.2022
#Last Updated: 09.03.2022

# install.packages("interactions")

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(zoo)
library(survival)
library(survminer)
library(survPen)
library(flexsurv)
library(coxme)
library(stargazer)
library(texreg)
library(forestplot)
library(sjPlot)
library(janitor)
library(lme4)
library(survey)
library(jtools)
library(ggstance)
library(broom.mixed)
library(effects)
library(interactions)

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")

surv4 %>% count(finnow.num)

###########################################################################
# Discrete Time Hazard Model ----------------------------------------------
###########################################################################

# My modeling strategy will looks like this:
## (Also stratified by sex)

# Analysis 1 - Present Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + present financial situation + controls + CCI
## 3. OEC + present financial situation*edu + controls + CCI
## 4. Model 3 + Partner variables

# Analysis 2 - Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Future financial situation + controls + CCI
## 3. OEC + present financial situation*edu + controls + CCI
## 4. Model 3 + Partner variables

# Analysis 3 - Present and Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Financial Situations + controls + CCI
## 3. OEC + present/future financial situation*edu + controls + CCI
## 4. Model 3 + Partner variables

# Analysis 4 - Income

##########################################################################
# Analysis 1 Finnow-------------------------------------------------------
##########################################################################

# -------------------------------------------------------------------------
# Model 1. Objective employment conditions (OEC) + controls --------------
# -------------------------------------------------------------------------

a1m1m <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a1m1m)
summ(a1m1m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m1m, exp = T)

a1m1f <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a1m1f)
summ(a1m1f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m1f, exp = T)

plot_summs(a1m1m, a1m1f, exp = T)

# -------------------------------------------------------------------------
# Model 2. OEC + Future financial situation + controls + CCI --------------
# -------------------------------------------------------------------------

a1m2m <- glm(formula = event ~ t2 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a1m2m)
summ(a1m2m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m2m, exp = T)

a1m2f <- glm(formula = event ~ t2 + empstat2 + difficult + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a1m2f)
summ(a1m2f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m2f, exp = T)

plot_summs(a1m2m, a1m2f, exp = T)

# -------------------------------------------------------------------------
# Model 3. OEC * present financial situation + controls + CCI -------------
# -------------------------------------------------------------------------

a1m3m <- glm(formula = event ~ t2 + empstat2 + difficult*edu + agemn + agesq + immigrant + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a1m3m)
summ(a1m3m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m3m, exp = T)

a1m3f <- glm(formula = event ~ t2 + empstat2 + difficult*edu + agemn + agesq + immigrant + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a1m3f)
summ(a1m3f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m3f, exp = T)

plot_summs(a1m3m, a1m3f, exp = T)
##There is no statisitical significance to any of the interaction terms 

# -------------------------------------------------------------------------
# Model 4. Model 3 + Partner variables ------------------------------------
# -------------------------------------------------------------------------

a1m4m <- glm(formula = event ~ t2 + empstat2 + difficult*edu + agemn + agesq + immigrant + ol5cat + cci + combo,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a1m4m)
summ(a1m4m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m4m, exp = T)

a1m4f <- glm(formula = event ~ t2 + empstat2 + difficult*edu + agemn + agesq + immigrant + ol5cat + cci + combo,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a1m4f)
summ(a1m4f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a1m4f, exp = T)

plot_summs(a1m4m, a1m4f, exp = T)

##########################################################################
# Analysis 2 finfut-------------------------------------------------------
##########################################################################

# -------------------------------------------------------------------------
# Model 1. Objective employment conditions (OEC) + controls --------------
# -------------------------------------------------------------------------

a2m1m <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a2m1m)
summ(a2m1m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m1m, exp = T)

a2m1f <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a2m1f)
summ(a2m1f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m1f, exp = T)

plot_summs(a2m1m, a2m1f, exp = T)

# -------------------------------------------------------------------------
# Model 2. OEC + Future financial situation + controls + CCI --------------
# -------------------------------------------------------------------------

a2m2m <- glm(formula = event ~ t2 + empstat2 + worse + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a2m2m)
summ(a2m2m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m2m, exp = T)

a2m2f <- glm(formula = event ~ t2 + empstat2 + worse + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a2m2f)
summ(a2m2f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m2f, exp = T)

plot_summs(a2m2m, a2m2f, exp = T)

# -------------------------------------------------------------------------
# Model 3. OEC * present financial situation + controls + CCI -------------
# -------------------------------------------------------------------------

a2m3m <- glm(formula = event ~ t2 + empstat2 + worse*edu + agemn + agesq + immigrant + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a2m3m)
summ(a2m3m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m3m, exp = T)

a2m3f <- glm(formula = event ~ t2 + empstat2 + worse*edu + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a2m3f)
summ(a2m3f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m3f, exp = T)

plot_summs(a2m3m, a2m3f, exp = T)
##There is no statisitical significance to any of the interaction terms 

# -------------------------------------------------------------------------
# Model 4. Model 3 + Partner variables ------------------------------------
# -------------------------------------------------------------------------

a2m4m <- glm(formula = event ~ t2 + empstat2 + worse*edu + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
           family = binomial(link = "logit"),
           data = surv4m)
summary(a2m4m)
summ(a2m4m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m4m, exp = T)

a2m4f <- glm(formula = event ~ t2 + empstat2 + worse*edu + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
           family = binomial(link = "logit"),
           data = surv4f)
summary(a2m4f)
summ(a2m4f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a2m4f, exp = T)

plot_summs(a2m4m, a2m4f, exp = T)

##########################################################################
# Analysis 3 finnow + finfut ---------------------------------------------
##########################################################################

# -------------------------------------------------------------------------
# Model 1. Objective employment conditions (OEC) + controls --------------
# -------------------------------------------------------------------------

a3m1m <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m1m)
summ(a3m1m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m1m, exp = T)

a3m1f <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m1f)
summ(a3m1f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m1f, exp = T)

plot_summs(a3m1m, a3m1f, exp = T)

# -------------------------------------------------------------------------
# Model 2. OEC + Future financial situation + controls + CCI --------------
# -------------------------------------------------------------------------

a3m2m <- glm(formula = event ~ t2 + empstat2 + difficult + worse + agemn + agesq + immigrant + edu + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m2m)
summ(a3m2m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m2m, exp = T)

a3m2f <- glm(formula = event ~ t2 + empstat2 + difficult + worse + agemn + agesq + immigrant + edu + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m2f)
summ(a3m2f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m2f, exp = T)

plot_summs(a3m2m, a3m2f, exp = T)

# -------------------------------------------------------------------------
# Model 3. OEC * present financial situation + controls + CCI -------------
# -------------------------------------------------------------------------

a3m3m <- glm(formula = event ~ t2 + empstat2 + difficult*edu + worse*edu + agemn + agesq + immigrant + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m3m)
summ(a3m3m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m3m, exp = T)

a3m3f <- glm(formula = event ~ t2 + empstat2 + difficult*edu + worse*edu + agemn + agesq + immigrant + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m3f)
summ(a3m3f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m3f, exp = T)

plot_summs(a3m3m, a3m3f, exp = T)
##There is no statisitical significance to any of the interaction terms 

# -------------------------------------------------------------------------
# Model 4. Model 3 + Partner variables ------------------------------------
# -------------------------------------------------------------------------

a3m4m <- glm(formula = event ~ t2 + empstat2 + difficult*edu + worse*edu + agemn + agesq + immigrant + ol5cat + cci + combo,
             family = binomial(link = "logit"),
             data = surv4m)
summary(a3m4m)
summ(a3m4m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m4m, exp = T)

a3m4f <- glm(formula = event ~ t2 + empstat2 + difficult*edu + worse*edu + agemn + agesq + immigrant + ol5cat + cci + combo,
             family = binomial(link = "logit"),
             data = surv4f)
summary(a3m4f)
summ(a3m4f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(a3m4f, exp = T)

plot_summs(a3m4m, a3m4f, exp = T)




