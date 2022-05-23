#Coded by: Brian Buh
#Started on: 09.03.2022
#Last Updated: 23.05.2022


# install.packages("margins")
# install.packages("modelsummary")
# install.packages("mfx")
# install.packages("gt")
# install.packages("flextable")

library(margins) #the package I will use for extracting Average Marginal Effects
library(tidyverse)
library(jtools)
library(effects)
library(interactions)
library(modelsummary)
library(flextable) #For docx outputs from modelsummary
# library(mfx) #This is an alternative package for extracting Marginal Effects
library(gt) #Use tab functions for outputs
library(sjPlot) #Use for the plot_model
# library(stargazer)

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

## All Analysis are sex-specific, the table outputs have Women first, then Men

# Analysis 1 - Present Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + present financial situation + controls + CCI
## 3. Model 2 + Partner variables
## 4. Model 3 + Income Quintiles

# Analysis 2 - Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Future financial situation + controls + CCI
## 3. Model 2 + Partner variables
## 4. Model 3 + Income Quintiles


###########################################################################
# Analysis 1 --------------------------------------------------------------
###########################################################################
test <- summary(ma1m1f)
test2 <- summary(ma1m2f)
stargazer(test, test2, type = "text", summary = TRUE)

# -------------------------------------------------------------------------
# Analysis 1 Women --------------------------------------------------------
# -------------------------------------------------------------------------

#Only Empstat
ma1m1f <- margins(a1m1f) 
summary(ma1m1f)
plot_model(a1m1f, type = "pred", terms = "edu")

#Empstat + Difficult
ma1m2f <- margins(a1m2f)
summary(ma1m2f)
plot(ma1m2f)

#Empstat + Difficult + Partner
ma1m3f <- margins(a1m3f)
summary(ma1m3f)
plot(ma1m3f)

#Empstat + Difficult + Partner + Incquin
ma1m4f <- margins(a3m2f)
summary(ma1m4f)
plot(ma1m4f)



a1modf <- list(ma1m1f,ma1m2f,ma1m3f, ma1m4f)
cm3 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         "difficultDifficult" = "Difficult",
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
modelsummary(a1modf, coef_map = cm3, output = "A1diff_Women_AME_S10_23-05-2022.html", stars = TRUE)

# tab_model(ma1m1f)

# -------------------------------------------------------------------------
# Analysis 1 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

#Only Empstat
ma1m1m <- margins(a1m1m) 
summary(ma1m1m)
plot_model(a1m1m, type = "pred", terms = "edu")

#Empstat + Difficult
ma1m2m <- margins(a1m2m)
summary(ma1m2m)
plot(ma1m2m)

#Empstat + Difficult + Partner
ma1m3m <- margins(a1m3m)
summary(ma1m3m)
plot(ma1m3m)

#Empstat + Difficult + Partner + Incquin
ma1m4m <- margins(a3m2m)
summary(ma1m4m)
plot(ma1m4m)

#modelsummary gives me AME output (as matched with the summary command)
a1modm <- list(ma1m1m,ma1m2m,ma1m3m, ma1m4m)
modelsummary(a1modm, coef_map = cm3, output = "A1diff_Men_AME_S10_23-05-2022.html", stars = TRUE) 

# -------------------------------------------------------------------------
# Analysis 1 Combined -----------------------------------------------------
# -------------------------------------------------------------------------

a1modb <- list(ma1m1f,ma1m2f,ma1m3f,ma1m4f,ma1m1m,ma1m2m,ma1m3m,ma1m4m)
modelsummary(a1modb, coef_map = cm3, output = "A2worse_Both_AME_S10_23-05-2022.html", stars = TRUE) 
modelsummary(a1modb, coef_map = cm3, output = "A2worse_Both_AME_S10_23-05-2022.docx", stars = TRUE) 



###########################################################################
# Analysis 2 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 2 Women --------------------------------------------------------
# -------------------------------------------------------------------------

#Only Empstat
ma2m1f <- margins(a2m1f) 
summary(ma2m1f)
plot_model(a2m1f, type = "pred", terms = "edu")

#Empstat + Difficult
ma2m2f <- margins(a2m2f)
summary(ma2m2f)
plot(ma2m2f)

#Empstat + Difficult + Partner
ma2m3f <- margins(a2m3f)
summary(ma2m3f)
plot(ma2m3f)

#Empstat + Difficult + Partner + Incquin
ma2m4f <- margins(a3m4f)
summary(ma2m4f)
plot(ma2m4f)



a2modf <- list(ma2m1f,ma2m2f,ma2m3f, ma2m4f)
cm4 <- c("t3" = "Time since Education",
         "empstat2part time" = "Part-time",
         "empstat2self-employed" = "Self-employed",
         "empstat2unemployment" = "Unemployment",
         "empstat2out of LF" = "Out of the LF",
         # "difficultDifficult" = "Difficult",
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
modelsummary(a2modf, coef_map = cm4, output = "A2worse_Women_AME_S10_23-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Analysis 2 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

#Only Empstat
ma2m1m <- margins(a2m1m) 
summary(ma2m1m)
plot_model(a2m1m, type = "pred", terms = "edu")

#Empstat + Difficult
ma2m2m <- margins(a2m2m)
summary(ma2m2m)
plot(ma2m2m)

#Empstat + Difficult + Partner
ma2m3m <- margins(a2m3m)
summary(ma2m3m)
plot(ma2m3m)

#Empstat + Difficult + Partner + Incquin
ma2m4m <- margins(a3m4m)
summary(ma2m4m)
plot(ma2m4m)

#modelsummary gives me AME output (as matched with the summary command)
a2modm <- list(ma2m1m,ma2m2m,ma2m3m, ma2m4m)
modelsummary(a2modm, coef_map = cm4, output = "A2worse_Men_AME_S10_23-05-2022.html", stars = TRUE) 

# -------------------------------------------------------------------------
# Analysis 2 Combined -----------------------------------------------------
# -------------------------------------------------------------------------

a2modb <- list(ma2m1f,ma2m2f,ma2m3f,ma2m4f,ma2m1m,ma2m2m,ma2m3m,ma2m4m)
modelsummary(a2modb, coef_map = cm4, output = "A2worse_Both_AME_S10_23-05-2022.html", stars = TRUE) 

# %>%
  # tab_spanner(label = 'Women', columns = c("ma2m1f","ma2m2f","ma2m3f","ma2m4f")) %>%
  # tab_spanner(label = 'Men', columns = c("ma2m1m","ma2m2m","ma2m3m","ma2m4m"))





