#Coded by: Brian Buh
#Started on: 09.03.2022
#Last Updated: 

# install.packages("margins")
install.packages("modelsummary")

library(margins)
library(data.table)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(janitor)
library(jtools)
library(effects)
library(interactions)
library(stargazer)
library(modelsummary)

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")

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
modelsummary(a1modm, output = "testm.html", stars = TRUE)

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

#Stargazer doesn't seem to work well
# stargazer::stargazer(ma1m1f,ma1m2f,ma1m3f,ma1m4f, type = "text")

#modelsummary gives me AME output (as matched with the summary command)
a1modf <- list(ma1m1f,ma1m2f,ma1m3f,ma1m4f)
modelsummary(a1modf, output = "test.html", stars = TRUE)
