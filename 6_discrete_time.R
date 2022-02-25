#Coded by: Brian Buh
#Started on: 12.03.2021
#Last Updated: 16.04.2021

# install.packages("lme4")
# install.packages("survey")
# install.packages("jtools")
# install.packages("ggstance")
# install.packages("joots") #for plotting visualisation of parameter effects
# install.packages("broom.mixed")\
# install.packages("effects")

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

library(lme4)
library(survey)
library(jtools)
library(ggstance)
# library(joots) #not available for this version of R
library(broom.mixed)
library(effects)

############################################################################
# Loading data Surv2 -------------------------------------------------------
############################################################################

surv2 <- file.choose()
surv2 <- readRDS(surv2)

# The variable "edu_cat" does not include immigrant education
# #This step updates the educational attainment variable
surv3 <- surv2 %>% 
  # filter(edu_cat != "other") %>% #I made the decision to remove other category as it is mainly people who were not raised in the UK
  mutate(worse = ifelse(finfut.num == -1, 1, 0)) %>%  #A binary variable for people who think their finances will get worse
  mutate(comf = ifelse(finnow.num > 2, 1, 0)) %>%  #Creates a binary for positive versus negative current financial stability
  mutate(employed = ifelse(is.na(employed), 0, employed)) %>% 
  mutate(edu = case_when(
    isced97 == 2 ~ "low",
    isced97 == 3 | isced97 == 4  ~ "medium",
    isced97 == 5 | isced97 == 6  ~ "high",
    is.na(isced97) ~ "other")) %>% #Other now represents all individuals with unknown educational backgrounds
  filter(edu != "other") #This removes 2513 observations from the DF

surv3 %>% count(finnow.num)
surv3 %>% count(finnow.imp)
surv3 %>% count(comf)
surv3 %>% count(edu)
surv3 %>% count(is.na(jbisco88_cc))

#Create separate data sets for men and women
#Removes "other" educational level
survm <- surv3 %>% filter(sex == 1)
survf <- surv3 %>% filter(sex == 2)

#Descriptive statistics of the sample
mycontrols <- tableby.control(test = FALSE)
surv3stats <-arsenal::tableby(sex ~ finnow.imp + finfut.imp + jbsec + edu + combo, data = surv3, control = mycontrols)
labels(surv2stats) <-  c(finnow.imp = "Present Financial Outlook", finfut.imp = "Future Financial Outlook",
                         jbsec = "Job Security", edu = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(surv3stats)
# write2word(surv3stats, "surv3stats.doc")
write2html(surv3stats, "surv3stats_25-02-2022.html") #UPDATE DATE

###########################################################################
# Dataframes for looking at ethnic/immigrant variance ---------------------
###########################################################################

# substat <- surv %>% 
#   mutate(finnow.imp = fct_relevel(finnow.imp, c("5 Finding it very difficult", "4 Finding it quite difficult",
#                                                 "3 Just getting by", "2 Doing alright", "1 Living comfortably"))) %>%
#   mutate(finnow.num = as.numeric(finnow.imp)) %>% 
#   #I change the scale of finfut to be centered at 0
#   mutate(finfut.imp = fct_relevel(finfut.imp, c( "Worse off", "About the same", "Better off"))) %>% 
#   mutate(finfut.num = as.numeric(finfut.imp)) %>% 
#   mutate(finfut.num = recode(finfut.num,
#                              "2" = "0",
#                              "1" = "-1",
#                              "3" = "1")) %>% 
#   mutate(finfut.num = as.integer(finfut.num)) %>% 
#   fill(jbsec, .direction = "downup") %>% #Note this si done for quick testing on the past slide. Consider its use!!!
#   mutate(jbsec = fct_relevel(jbsec, c("3 non-employed", "1 likely", "2 unlikely"))) %>%
#   mutate(jbsec2 = as.numeric(jbsec))
# 
# str(substat)
# 
# surv3 <- surv2 %>% 
#   mutate(gor_dv = as.character(gor_dv)) %>% 
#   #Create categories for ethnicity based on Kulu&Hannemann2016
#   mutate(ethnic = ifelse(racel_dv == 1, 1, #english, scottish, welsh, ni
#                          ifelse(racel_dv == 2 | racel_dv == 3 | racel_dv == 4, 2, #other white
#                                 ifelse(racel_dv == 9, 3, #indian
#                                        ifelse(racel_dv == 10, 4, #pakistani
#                                               ifelse(racel_dv == 11, 5, #bangladeshi
#                                                      ifelse(racel_dv == 14, 7, #carribean
#                                                             ifelse(racel_dv == 12 | racel_dv == 13, 6, #other asian
#                                                                    ifelse(racel_dv == 15, 8, 9))))))))) %>% #african or other
#   mutate(ethnic = as.character(ethnic)) %>%  
#   mutate(ethnic = recode(ethnic,
#                          "1" = "UK",
#                          "2" = "Other White",
#                          "3" = "Indian",
#                          "4" = "Pakistani",
#                          "5" = "Bangladeshi",
#                          "6" = "Other Asian",
#                          "7" = "Caribbean",
#                          "8" = "African",
#                          "9" = "Mixed/Other")) %>%
#   mutate(ethnic = fct_relevel(ethnic, c( "UK",
#                                          "Other White",
#                                          "Indian",
#                                          "Pakistani",
#                                          "Bangladeshi",
#                                          "Other Asian",
#                                          "Caribbean",
#                                          "African",
#                                          "Mixed/Other"))) %>% 
#   mutate(gen = ifelse(generation == 1, 1, 2)) %>% #Creates a binary of immigrants versus born UK
#   mutate(gen = recode(gen,
#                       "1" = "First Generation",
#                       "2" = "UK Born")) %>% 
#   mutate(gen = as.character(gen)) %>% 
#   unite(genethnic, ethnic, gen,  sep = "-", remove = FALSE) %>% 
#   unite(sexethnic, sex, ethnic, sep = "-", remove = FALSE) %>% 
#   unite(ethnicsex, ethnic, sex, sep = "-", remove = FALSE) %>% 
#   mutate(gor_dv = recode(gor_dv,
#                          "1" = "North East",
#                          "2" = "North West",
#                          "3" = "Yorkshire and Humberside",
#                          "4" = "East Midlands",
#                          "5" = "West Midlands",
#                          "6" = "East of England",
#                          "7" = "London",
#                          "8" = "South East",
#                          "9" = "South West",
#                          "10" = "Wales",
#                          "11" = "Scotland",
#                          "12" = "Northern Ireland",
#                          "-9" = "missing"))
# 
# 
# str(surv3)
# 
# surv4 <- surv3 %>% 
#   filter(t2 > 400, event == 1)
# 
# surv4 %>% 
#   count(event)
# 
# 
###########################################################################
# Graphing relationship of Hazard, Time & Covariate -----------------------
###########################################################################

###The Following graph the relationship between the hazard, time, and covariate
#Looking at the visual relationship between the birth hazard and time since end of education
#by sex
surv3 %>%
  group_by(t2, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = t2, y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)

#Sex and age
surv3 %>%
  mutate(sex = as.character(sex)) %>%
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>%
  group_by(agemn, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = agemn,
             y = log(-log(1-hazard)),
             col = sex)) +
  geom_point() +
  geom_smooth() +
  labs(col = "Sex") +
  ylab("log(-log(1 - hazard)))") +
  xlab("Age in months") +
  ggsave("glm_hazard_age_sex_25-02-2022.png") #UPDATE DATE

#Education and Time Model Fit
surv3 %>%
  group_by(t2, edu) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  # filter(event ==1) %>% 
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = edu)) +
  geom_point() +
  geom_smooth() +
  ylab("log(-log(1 - hazard)))") +
  xlab("Months since end of formal education") +
  ggsave("flm_hazard_edu_25-02-2022.png") #UPDATE DATE

#Hazards by time since end of education
surv3 %>%
  mutate(comf = as.character(comf)) %>% 
  group_by(t2, edu, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  # filter(event ==1) %>% 
  ggplot(aes(x = t2,
             y = log(-log(1-hazard)),
             col = edu)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)

#Hazards by age
surv3 %>%
  group_by(agemn, edu, sex) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = agemn,
             y = log(-log(1-hazard)),
             col = edu)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex)

# #Fit of Time since Edu and finnow
# survm %>%
#   group_by(t2, finnow.imp) %>%
#   summarise(event = sum(event),
#             total = n()) %>%
#   mutate(hazard = event/total) %>%
#   ggplot(aes(x = t2,
#              y = log(-log(1-hazard)),
#              col = finnow.imp)) +
#   geom_point() +
#   geom_smooth()


############################################################################
# Cleaning DF -------------------------------------------------------------
############################################################################

#remove columns that are not needed in the analysis - keep this as the final variable deciding location

surv4 <- surv3 %>%
  dplyr::select(pidp, wave, hhorig, sex, time1, time2, t1, t2, event, kdob, fb, fbyear, anychild_dv, 
                edu, dvage, agemn, agesq, marstat, ukborn, immigrant,
                jbstat, employed, permcon, jbhrs, parttime, jbot, priv, jbpl,
                finnow.imp, finfut.imp, jbsec,
                marstat, parjbstat, combo) %>%
  mutate(sex = as.character(sex)) %>%
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women"))

survm <- surv4 %>% filter(sex == "Men")
survf <- surv4 %>% filter(sex == "Women")

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


testlogit <- glm(formula = event ~ t2 + sex + agemn + agesq + finnow.imp + finfut.imp + employed + edu,
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
# Gender Specific models & Model fit testing ------------------------------
###########################################################################

# ----------------------------------------------------------------------
# Model for men --------------------------------------------------------
# ----------------------------------------------------------------------

baseline_men <- glm(formula = event ~ t2,
                     family = binomial(link = "logit"),
                     data = survm)
summ(baseline_men, exp = TRUE, scale = TRUE)
mlogit <- glm(formula = event ~ t2 + agemn + agesq + finnow.imp + finfut.imp + employed + edu,
            family = binomial(link = "logit"),
            data = survm)
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
                          Event = pull(survm, event),
                          ID = pull(survm, pidp))%>%
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
               data = survm)

anova(mlogit, mglmage, test = "Chisq")

mlogit$aic
mglmage$aic

Data_DevResid_m_age <- tibble(Pred_Haz = predict(mglmage, type = "response"),
                              Event = pull(survm, event),
                              ID = pull(survm, pidp))%>%
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
                     data = survf)
summ(baseline_women, exp = TRUE, scale = TRUE)
flogit<- glm(formula = event ~ t2 + agemn + agesq + finnow.imp + finfut.imp + employed + edu,
            family = binomial(link = "logit"),
            data = survf)
summary(flogit)
summ(flogit, exp = TRUE) #exp = TRUE means that we want exponentiated estimates

#Likelihood Ratio Test
anova(baseline_women, flogit, test = "Chisq")

#AIC
baseline_women$aic
flogit$aic

#Deviance Residuals
Data_DevResid_f <- tibble(Pred_Haz = predict(flogit, type = "response"),
                          Event = pull(survf, event),
                          ID = pull(survf, pidp))%>%
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
               data = survf)

anova(flogit, fglmage, test = "Chisq")

flogit$aic
fglmage$aic

Data_DevResid_f_age <- tibble(Pred_Haz = predict(fglmage, type = "response"),
                              Event = pull(survf, event),
                              ID = pull(survf, pidp))%>%
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




############
##########
################
# jbsec stats -------------------------------------------------------------



testemp <- surv3 %>% 
  group_by(pidp, sex, kdob) %>% 
  summarise(emp = mean(employed)) %>% 
  mutate(fullemp = ifelse(emp == 1, 1, 0)) %>% 
  ungroup() %>% 
  mutate(fb = ifelse(is.na(kdob), 0, 1))

testemp %>% count(sex, fullemp, fb)  

testemp %>% summary(emp)

testemp %>% 
  ggplot(aes(x = emp)) +
  geom_histogram(binwidth = 0.1)



# Subjective Measures -----------------------------------------------------

surv3 %>%
  group_by(t2, finnow.imp, employed) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  interaction.plot(x.factor = finnow.imp,
                   trace.factor = employed,
                   response = hazard,
                   fun = median)

surv3 %>% 
  ggplot(aes(x = ))