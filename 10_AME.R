#Coded by: Brian Buh
#Started on: 09.03.2022
#Last Updated: 04.05.2022

# install.packages("margins")
# install.packages("modelsummary")
# install.packages("mfx")

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

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")
# 
# testsurv4 <- surv4 %>%
#   mutate(edunum = recode(edu,
#                          "low" = "1",
#                          "medium" = "2",
#                          "high" = "3"),
#          edunum = as.numeric(edunum))
# 
# testsurv4m <- testsurv4 %>% filter(sex == "Men")
# testsurv4f <- testsurv4 %>% filter(sex == "Women")

###########################################################################
# Test 1 --------------------------------------------------------------
###########################################################################

example <- testm <- glm(formula = event ~ t3 + empstat2*edu + difficult*edu,
                        family = binomial(link = "logit"),
                        data = testsurv4m)
mexample <- margins(example)
summary(mexample)



testm <- glm(formula = event ~ t3 + empstat2 + difficult*edu + agemn + agesq + immigrant + ol5cat + cci,
             family = binomial(link = "logit"),
             data = testsurv4m)
summary(testm)
cplot(testm, "empstat2")
cplot(testm, "edu")

mtestm <- margins(testm, at = list(edu))
summary(mtestm)
plot(mtestm)


#logitmfx

# test2m <- logitmfx(formula = event ~ t3 + empstat2 + difficult*edu + agemn + agesq + immigrant + ol5cat + cci,
#                    data = surv4m)
# summary(test2m)



testf <- glm(formula = event ~ t3 + empstat2 + difficult*edunum + agemn + agesq + immigrant + ol5cat + cci,
             family = binomial(link = "logit"),
             data = testsurv4f)
summary(testf)

mtestf <- margins(testf, at = list(edunum = 1:3))
summary(mtestf)
plot(mtestf)
cplot(testf, "empstat2")

#modelsummary gives me AME output (as matched with the summary command)
test1 <- list(mtestm, mtestf)
modelsummary(test1, output = "test1.html", stars = TRUE)



###########################################################################
# Analysis 1 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 1 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

ma1m1m <- margins(a1m1m)
summary(ma1m1m)
plot(ma1m1m)
cplot(a1m1m, "empstat2")

ma1m2m <- margins(a1m2m)
summary(ma1m2m)
plot(ma1m2m)
cplot(a1m2m, "difficult")

ma1m3m <- margins(a1m3m)
summary(ma1m3m)
plot(ma1m3m)
cplot(a1m3m, "edu")


ma1m4m <- margins(a1m4m)
summary(ma1m4m)
plot(ma1m4m)
cplot(a1m4m, "combo")


#modelsummary gives me AME output (as matched with the summary command)
a1modm <- list(ma1m1m,ma1m2m,ma1m3m,ma1m4m)
modelsummary(a1modm, output = "A1_Men_AME_S10_11-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Analysis 1 Women --------------------------------------------------------
# -------------------------------------------------------------------------

ma1m1f <- margins(a1m1f)
summary(ma1m1f)
plot(ma1m1f)
cplot(a1m1f, "empstat2")

ma1m2f <- margins(a1m2f)
summary(ma1m2f)
plot(ma1m2f)
cplot(a1m2f, "difficult")

ma1m3f <- margins(a1m3f)
summary(ma1m3f)
plot(ma1m3f)
cplot(a1m3f, "edu")

ma1m4f <- margins(a1m4f)
summary(ma1m4f)
plot(ma1m4f)
cplot(a1m4f, "combo")

#modelsummary gives me AME output (as matched with the summary command)
a1modf <- list(ma1m1f,ma1m2f,ma1m3f,ma1m4f)
modelsummary(a1modf, output = "A1_Women_AME_S10_11-05-2022.html", stars = TRUE)

###########################################################################
# Analysis 2 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 2 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

ma2m1m <- margins(a2m1m)
summary(ma2m1m)
plot(ma2m1m)
cplot(a2m1m, "empstat2")

ma2m2m <- margins(a2m2m)
summary(ma2m2m)
plot(ma2m2m)
cplot(a2m2m, "worse")

ma2m3m <- margins(a2m3m)
summary(ma2m3m)
plot(ma2m3m)
cplot(a2m3m, "edu")


ma2m4m <- margins(a2m4m)
summary(ma2m4m)
plot(ma2m4m)
cplot(a2m4m, "combo")


#modelsummary gives me AME output (as matched with the summary command)
a2modm <- list(ma2m1m,ma2m2m,ma2m3m,ma2m4m)
modelsummary(a2modm, output = "A2_Men_AME_S10_11-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Analysis 2 Women --------------------------------------------------------
# -------------------------------------------------------------------------

ma2m1f <- margins(a2m1f)
summary(ma2m1f)
plot(ma2m1f)
cplot(a2m1f, "empstat2")

ma2m2f <- margins(a2m2f)
summary(ma2m2f)
plot(ma2m2f)
cplot(a2m2f, "worse")

ma2m3f <- margins(a2m3f)
summary(ma2m3f)
plot(ma2m3f)
cplot(a2m3f, "edu")

ma2m4f <- margins(a2m4f)
summary(ma2m4f)
plot(ma2m4f)
cplot(a2m4f, "combo")

#modelsummary gives me AME output (as matched with the summary command)
a2modf <- list(ma2m1f,ma2m2f,ma2m3f,ma2m4f)
modelsummary(a2modf, output = "A2_Women_AME_S10_11-05-2022.html", stars = TRUE)

###########################################################################
# Analysis 3 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 3 Men ----------------------------------------------------------
# -------------------------------------------------------------------------

ma3m1m <- margins(a3m1m)
summary(ma3m1m)
plot(ma3m1m)
cplot(a3m1m, "empstat2")

ma3m2m <- margins(a3m2m)
summary(ma3m2m)
plot(ma3m2m)
cplot(a3m2m, "difficult")

ma3m3m <- margins(a3m3m)
summary(ma3m3m)
plot(ma3m3m)
cplot(a3m3m, "edu")


ma3m4m <- margins(a3m4m)
summary(ma3m4m)
plot(ma3m4m)
cplot(a3m4m, "combo")


#modelsummary gives me AME output (as matched with the summary command)
a3modm <- list(ma3m1m,ma3m2m,ma3m3m,ma3m4m)
modelsummary(a3modm, output = "A3_Men_AME_S10_11-05-2022.html", stars = TRUE)

# -------------------------------------------------------------------------
# Analysis 3 Women --------------------------------------------------------
# -------------------------------------------------------------------------

ma3m1f <- margins(a3m1f)
summary(ma3m1f)
plot(ma3m1f)
cplot(a3m1f, "empstat2")

ma3m2f <- margins(a3m2f)
summary(ma3m2f)
plot(ma3m2f)
cplot(a3m2f, "difficult")

ma3m3f <- margins(a3m3f)
summary(ma3m3f)
plot(ma3m3f)
cplot(a3m3f, "edu")

ma3m4f <- margins(a3m4f)
summary(ma3m4f)
plot(ma3m4f)
cplot(a3m4f, "combo")

#modelsummary gives me AME output (as matched with the summary command)
a3modf <- list(ma3m1f,ma3m2f,ma3m3f,ma3m4f)
modelsummary(a3modf, output = "A3_Women_AME_S10_11-05-2022.html", stars = TRUE)
