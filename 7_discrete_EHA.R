#Coded by: Brian Buh
#Started on: 02.03.2022
#Last Updated: 

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


#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4 %>% count(empstat2, event)
test <- surv4 %>% count(ol5cat, event, sex) #The number of events for armed forces is too small. remove that category


surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")

surv4m %>% count(empstat2, event)
surv4f %>% count(empstat2, event)

############################################################################
## Testing Gompertz GLM Model ----------------------------------------------
############################################################################

#Creating a baseline to see the time since education and event
baselinelogit <- glm(formula = event ~ t2,
                     family = binomial(link = "logit"),
                     data = surv4)


baselinecloglog <- glm(formula = event ~ t2,
                       family = binomial(link = "cloglog"),
                       data = surv4)

summary(baselinelogit)
summary(baselinecloglog)
#There is no difference between logit and cloglog
summ(baselinelogit, exp = TRUE) #takes a minute to process
#The strong relationship between t2 and event in this models
#signifies that the baseline hazard is the same for all individuals ( :-) )


testlogit <- glm(formula = event ~ t2 + sex + agemn + agesq + finnow3cat + finfut.imp + empstat + edu +combo,
                 family = binomial(link = "logit"),
                 data = surv4)

summary(testlogit)
summ(testlogit, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(testlogit, exp = T)

#Does the test model vary significantly from the baseline?
##Likelihood Ratio Test
anova(baselinelogit, testlogit, test = "Chisq")
##AIC
baselinelogit$aic
testlogit$aic
#The answer appears to be yes


plot(allEffects(testlogit))
ggsave("paramater_effect_25-22-20.png")


#Deviance Residuals
Data_DevResid <- tibble(Pred_Haz = predict(testlogit, type = "response"),
                        Event = pull(surv4, event),
                        ID = pull(surv4, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point()

###########################################################################
# Model fit testing -------------------------------------------------------
###########################################################################

# ----------------------------------------------------------------------
# Model for men --------------------------------------------------------
# ----------------------------------------------------------------------

baseline_men <- glm(formula = event ~ t2,
                    family = binomial(link = "logit"),
                    data = surv4m)
summ(baseline_men, exp = TRUE, scale = TRUE)
mlogit <- glm(formula = event ~ t2 + agemn + agesq + finnow.imp + finfut.imp + employed + edu,
              family = binomial(link = "logit"),
              data = surv4m)
summary(mlogit)
summ(mlogit, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(mlogit, exp = T)

#Likelihood Ratio Test
anova(baseline_men, mlogit, test = "Chisq")
#AIC
baseline_men$aic
mlogit$aic

#Deviance Residuals
Data_DevResid_m <- tibble(Pred_Haz = predict(mlogit, type = "response"),
                          Event = pull(surv4m, event),
                          ID = pull(surv4m, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_m %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Men - end of education") +
  ggsave("dev_resid_men_endedu_25-02-2022.png")


#######
#Test Age versus end of education
mglmage <- glm(formula = event ~ agemn + agesq + finnow.imp + finfut.imp + employed + edu,
               family = binomial(link = "logit"),
               data = surv4m)

anova(mlogit, mglmage, test = "Chisq")

mlogit$aic
mglmage$aic

Data_DevResid_m_age <- tibble(Pred_Haz = predict(mglmage, type = "response"),
                              Event = pull(surv4m, event),
                              ID = pull(surv4m, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_m_age %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Men - age") +
  ggsave("dev_resid_men_age.png")



# ----------------------------------------------------------------------
# Model for women ------------------------------------------------------
# ----------------------------------------------------------------------

baseline_women <- glm(formula = event ~ t2,
                      family = binomial(link = "logit"),
                      data = surv4f)
summ(baseline_women, exp = TRUE, scale = TRUE)
flogit<- glm(formula = event ~ t2 + agemn + agesq + finnow.imp + finfut.imp + employed + edu,
             family = binomial(link = "logit"),
             data = surv4f)
summary(flogit)
summ(flogit, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

#Likelihood Ratio Test
anova(baseline_women, flogit, test = "Chisq")

#AIC
baseline_women$aic
flogit$aic

#Deviance Residuals
Data_DevResid_f <- tibble(Pred_Haz = predict(flogit, type = "response"),
                          Event = pull(surv4f, event),
                          ID = pull(surv4f, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_f %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Women - time since end of education") +
  ggsave("dev_resid_women_endedu_25-02-2022.png")


#######
#Test Age versus end of education
fglmage <- glm(formula = event ~ agemn + agesq + finnow.imp + finfut.imp + employed + edu,
               family = binomial(link = "logit"),
               data = surv4f)

anova(flogit, fglmage, test = "Chisq")

flogit$aic
fglmage$aic

Data_DevResid_f_age <- tibble(Pred_Haz = predict(fglmage, type = "response"),
                              Event = pull(surv4f, event),
                              ID = pull(surv4f, pidp))%>%
  mutate(DevRes = if_else(Event == 0, 
                          -sqrt(-2*log(1-Pred_Haz)),
                          sqrt(-2*log(Pred_Haz))))

Data_DevResid_f_age %>%
  ggplot(aes(x = ID, y = DevRes)) +
  geom_point() +
  ggtitle("Deviance Residuals Women - age") +
  ggsave("dev_resid_women_age_25-02-2022.png")




###########################################################################
# Covariate Testing -------------------------------------------------------
###########################################################################

###Goodness-of-Fit tests
#AIC Test (comparison)
testlogit$aic
mlogit$aic
flogit$aic
#The AIC improve significantly sex stratification


plot_models(mlogit, flogit, 
            title = "Odds Ratios",
            m.labels = c("Men", "Women"),
            legend.title = "Model",
            # axis.labels = c(
            #  "finfut.num:employed", "finnow.num:employed",
            # #   # "Married - unknown", "Married - non-employed","Married - employed",
            # #   # "Cohab - non-employed", "Cohab - employed","Single",
            #   "Edu. Low", "Edu. Medium", "Edu. High",
            #   # "Job security",
            #   "Future Financial Sit",
            #   "Employed",
            #   "Present Financial Sit",
            #   "PJI",
            #   "Age Squared", "Age, in months", "Time"),
            # axis.lim = c(0.5, 1.4),
            dot.size = 6,
            #colors  = c("#2E9FDF", "#E7B800"), #in case you wanna change to the gold blue set
            p.shape = TRUE,
            grid = TRUE)

ggsave("logit_timesinceedu_m_f_25-02-2022.png")


###########################################################################
# basic frailty and cox models --------------------------------------------
###########################################################################

#First step is to plot the Baseline Gompertz Regression model
surv4 %>%
  group_by(t2) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = t2, 
             y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth()

# ----------------------------------------------------------------------
# Basic Frailty Model --------------------------------------------------
# ----------------------------------------------------------------------

#A basic frailty model - has an added random individual effect
frailty_baseline <- glmer(formula = event ~ t2 + (1|pidp),
                          family = binomial(logit),
                          data = surv4,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 2e5)))

summary(frailty_baseline)
#AIC = 17120
baselinelogit$aic
#AIC = 19548
#The frailty baseline is a slight improvement


#The similarity between the AIC in this model and the above GLM model suggest this "Basic Frailty Model" is unnecessary 
#I cannot include agemn or agesq because there is a scaling issue between t2 and those perdictor variables - would need to be rescaled
frailtylogit <- glmer(formula = event ~ t2 + sex + finnow.imp + finfut.imp + employed + edu + (1|pidp),
                      family = binomial(logit),
                      data = surv4,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5))) #This is to control for the warning "Model is nearly unidentifiable"

summary(frailtylogit)


# ----------------------------------------------------------------------
# CoxPH Model ----------------------------------------------------------
# ----------------------------------------------------------------------

# 1. Kaplan-Meier Test
kmtest <- survfit(Surv(t1, t2, event) ~ strata (sex), data = surv4, cluster = pidp)
summary(kmtest)
plot(kmtest)

ggsurvplot(kmtest, size = 1,   # change line size
           #ylim = c(0.69,1),
           # palette = c("#E7B800", "#2E9FDF"),# custom color palettes
           conf.int = TRUE,          # Add confidence interval
           # pval = TRUE,              # Add p-value
           risk.table = TRUE,        # Add risk table
           # risk.table.col = "strata",# Risk table color by groups
           legend.labs =
             c("Men", "Women"),    # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw()      # Change ggplot2 theme
) 


# 2. Cox Proportional Hazard Model
coxph <- coxph(formula = Surv(t1, t2, event) ~ sex + finnow.imp + finfut.imp + employed + edu, data = surv4, cluster = pidp, method = "breslow")
summary(coxph)
testph <- cox.zph(coxph)
summary(testph)

###########################################################################
# Discrete Time Hazard Model ----------------------------------------------
###########################################################################

# My modeling strategy will looks like this:
## (Also stratified by sex)

# Analysis 1 - Present Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + present financial situation + controls + CCI
## 3. OEC * present financial situation + controls + CCI
## 4. Model 3 + Partner variables

# Analysis 2 - Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Future financial situation + controls + CCI
## 3. OEC * Future financial situation + controls + CCI
## 4. Model 3 + Partner variables

# Analysis 3 - Present and Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Financial Situations + controls + CCI
## 3. OEC * Financial Situations + controls + CCI
## 4. Model 3 + Partner variables

# Analysis 4 - Present and Future Financial Situation
## 1. Objective employment conditions (OEC) + controls
## 2. OEC + Financial Situations + controls + CCI
## 3. OEC * Financial Situations + controls + CCI
## 4. Model 3 + Partner variables

# Analysis 5 - Income

##########################################################################
# Analysis 1 Finnow-------------------------------------------------------
##########################################################################

# -------------------------------------------------------------------------
# Model 1. Objective employment conditions (OEC) + controls --------------
# -------------------------------------------------------------------------

m1m <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
              family = binomial(link = "logit"),
              data = surv4m)
summary(m1m)
summ(m1m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m1m, exp = T)

m1f <- glm(formula = event ~ t2 + empstat2 + agemn + agesq + immigrant + edu + ol5cat,
           family = binomial(link = "logit"),
           data = surv4f)
summary(m1f)
summ(m1f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m1f, exp = T)

plot_summs(m1m, m1f, exp = T)

# -------------------------------------------------------------------------
# Model 2. OEC + Future financial situation + controls + CCI --------------
# -------------------------------------------------------------------------

m2m <- glm(formula = event ~ t2 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4m)
summary(m2m)
summ(m2m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m2m, exp = T)

m2f <- glm(formula = event ~ t2 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4f)
summary(m2f)
summ(m2f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m2f, exp = T)

plot_summs(m2m, m2f, exp = T)

# -------------------------------------------------------------------------
# Model 3. OEC * present financial situation + controls + CCI -------------
# -------------------------------------------------------------------------

m3m <- glm(formula = event ~ t2 + empstat2*finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4m)
summary(m3m)
summ(m3m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m3m, exp = T)

m3f <- glm(formula = event ~ t2 + empstat2*finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci,
           family = binomial(link = "logit"),
           data = surv4f)
summary(m3f)
summ(m3f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m3f, exp = T)

plot_summs(m3m, m3f, exp = T)
##There is no statisitical significance to any of the interaction terms 

# -------------------------------------------------------------------------
# Model 4. Model 3 + Partner variables ------------------------------------
# -------------------------------------------------------------------------

m4m <- glm(formula = event ~ t2 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
           family = binomial(link = "logit"),
           data = surv4m)
summary(m4m)
summ(m4m, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m4m, exp = T)

m4f <- glm(formula = event ~ t2 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
           family = binomial(link = "logit"),
           data = surv4f)
summary(m4f)
summ(m4f, exp = TRUE) #exp = TRUE means that we want exponentiated estimates
plot_summs(m4f, exp = T)

plot_summs(m4m, m4f, exp = T)










