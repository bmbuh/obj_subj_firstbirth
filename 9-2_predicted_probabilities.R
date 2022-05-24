#Coded by: Brian Buh
#Started on: 24.05.2022
#Last Updated: 

#Scripts 9 & 9-1 build predicted probability plots for use in Section 8 of the paper
#This script looks to 

library(tidyverse)
library(janitor)
library(effects)
library(interactions)
library(huxtable) #needed for export_summs command

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")

###########################################################################
# Dataframe for calculating Predicted Probabilities -----------------------
###########################################################################

#When extract group predicted probabilities, I need to hold all other variables in the regression at their means
##This means I need numeric/integer variables

survpred <- surv4 %>% 
  mutate(sexnum = recode(sex,#There is an issue with as.numeric conversion
                         "Men" = "1",
                         "Women" = "2"),
         sexnum = as.numeric(sexnum),
         edufac = recode(edu,#There is an issue with as.numeric conversion
                         "low" = "1",
                         "medium" = "2",
                         "high" = "3"),
         edunum = as.numeric(edufac),
         empstat2fac = recode(empstat2,
                              "full time" = "1",
                              "part time" = "2",
                              "self-employed" = "3",
                              "unemployment" = "4",
                              "out of LF" = "5"),
         empstat2num = as.numeric(empstat2fac),
         difficultfac = recode(difficult, #There is an issue with as.numeric conversion
                           "Fine" = "1",
                           "Difficult" = "2"),
         difficultnum = as.numeric(difficult),
         worsefac = recode(worse, #There is an issue with as.numeric conversion
                           "Same or better" = "1",
                           "Worse" = "2"),
         worsenum = as.numeric(worsefac),
         immigrantnum = as.numeric(immigrant),
         ol5catnum = as.numeric(ol5cat),
         combofac = recode(combo,
                           "single-unknown" = "1",
                           "cohab-employed" = "2",
                           "cohab-non-employed" = "3",
                           "cohab-unknown" = "4",
                           "married-employed" = "5",
                           "married-non-employed" = "6",
                           "married-unknown" = "7"),
         combonum = as.numeric(combo),
         incquinnum = as.numeric(incquin))

survpred %>% count(combofac)

survpredm <- survpred %>% filter(sex == "Men")
survpredf <- survpred %>% filter(sex == "Women")

###Men
survpredmlow <- survpredm %>% filter(edu == "low")
survpredmmedium <- survpredm %>% filter(edu == "medium")
survpredmhigh <- survpredm %>% filter(edu == "high")

###Women
survpredflow <- survpredf %>% filter(edu == "low")
survpredfmedium <- survpredf %>% filter(edu == "medium")
survpredfhigh <- survpredf %>% filter(edu == "high")


###########################################################################
# Statistics for the paper ------------------------------------------------
###########################################################################


# -------------------------------------------------------------------------
# Section 8.1 Empstat -----------------------------------------------------
# -------------------------------------------------------------------------

# Women -------------------------------------------------------------------

###women - Low Edu
sec8.1flow <- glm(formula = event ~ t3 + empstat2fac + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                  family = binomial(link = "logit"),
                  data = survpredflow)
summary(sec8.1flow)

ndsec8.1flow <- with(survpredflow, data.frame(t3=mean(t3),
                                          empstat2fac=factor(1:5),
                                          # sexnum=mean(sexnum, na.rm=TRUE),
                                          difficultnum=mean(difficultnum, na.rm=TRUE),
                                          agemn=mean(agemn),
                                          agesq=mean(agesq),
                                          ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                          cci=mean(cci),
                                          immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                          combonum=mean(combonum, na.rm=TRUE)))

ndsec8.1flow$eventPP <- predict(sec8.1flow, ndsec8.1flow, type = "response")

###women - medium Edu
sec8.1fmedium <- glm(formula = event ~ t3 + empstat2fac + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                     family = binomial(link = "logit"),
                     data = survpredfmedium)
summary(sec8.1fmedium)

ndsec8.1fmedium <- with(survpredfmedium, data.frame(t3=mean(t3),
                                             empstat2fac=factor(1:5),
                                             # sexnum=mean(sexnum, na.rm=TRUE),
                                             difficultnum=mean(difficultnum, na.rm=TRUE),
                                             agemn=mean(agemn),
                                             agesq=mean(agesq),
                                             ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                             cci=mean(cci),
                                             immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                             combonum=mean(combonum, na.rm=TRUE)))

ndsec8.1fmedium$eventPP <- predict(sec8.1fmedium, ndsec8.1fmedium, type = "response")

###women - high Edu
sec8.1fhigh <- glm(formula = event ~ t3 + empstat2fac + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                   family = binomial(link = "logit"),
                   data = survpredfhigh)
summary(sec8.1fhigh)

ndsec8.1fhigh <- with(survpredfhigh, data.frame(t3=mean(t3),
                                           empstat2fac=factor(1:5),
                                           # sexnum=mean(sexnum, na.rm=TRUE),
                                           difficultnum=mean(difficultnum, na.rm=TRUE),
                                           agemn=mean(agemn),
                                           agesq=mean(agesq),
                                           ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                           cci=mean(cci),
                                           immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                           combonum=mean(combonum, na.rm=TRUE)))

ndsec8.1fhigh$eventPP <- predict(sec8.1flow, ndsec8.1fhigh, type = "response")

# Men ---------------------------------------------------------------------


###Men - Low Edu
sec8.1mlow <- glm(formula = event ~ t3 + empstat2fac + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
            family = binomial(link = "logit"),
            data = survpredmlow)
summary(sec8.1mlow)

ndsec8.1mlow <- with(survpredmlow, data.frame(t3=mean(t3),
                                     empstat2fac=factor(1:5),
                                     # sexnum=mean(sexnum, na.rm=TRUE),
                                     difficultnum=mean(difficultnum, na.rm=TRUE),
                                     agemn=mean(agemn),
                                     agesq=mean(agesq),
                                     ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                     cci=mean(cci),
                                     immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                     combonum=mean(combonum, na.rm=TRUE)))
                                     # edufac=factor(1:3))

ndsec8.1mlow$eventPP <- predict(sec8.1mlow, ndsec8.1mlow, type = "response")

###Men - medium Edu
sec8.1mmedium <- glm(formula = event ~ t3 + empstat2fac + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                  family = binomial(link = "logit"),
                  data = survpredmmedium)
summary(sec8.1mmedium)

ndsec8.1mmedium <- with(survpredmmedium, data.frame(t3=mean(t3),
                                          empstat2fac=factor(1:5),
                                          # sexnum=mean(sexnum, na.rm=TRUE),
                                          difficultnum=mean(difficultnum, na.rm=TRUE),
                                          agemn=mean(agemn),
                                          agesq=mean(agesq),
                                          ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                          cci=mean(cci),
                                          immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                          combonum=mean(combonum, na.rm=TRUE)))

ndsec8.1mmedium$eventPP <- predict(sec8.1mmedium, ndsec8.1mmedium, type = "response")

###Men - high Edu
sec8.1mhigh <- glm(formula = event ~ t3 + empstat2fac + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                  family = binomial(link = "logit"),
                  data = survpredmhigh)
summary(sec8.1mhigh)

ndsec8.1mhigh <- with(survpredmhigh, data.frame(t3=mean(t3),
                                          empstat2fac=factor(1:5),
                                          # sexnum=mean(sexnum, na.rm=TRUE),
                                          difficultnum=mean(difficultnum, na.rm=TRUE),
                                          agemn=mean(agemn),
                                          agesq=mean(agesq),
                                          ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                          cci=mean(cci),
                                          immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                          combonum=mean(combonum, na.rm=TRUE)))

ndsec8.1mhigh$eventPP <- predict(sec8.1mlow, ndsec8.1mhigh, type = "response")

# -------------------------------------------------------------------------
# Section 8.2 Difficult -----------------------------------------------------
# -------------------------------------------------------------------------

# Women -------------------------------------------------------------------

###women - Low Edu
sec8.2flow <- glm(formula = event ~ t3 + empstat2num + difficultfac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                  family = binomial(link = "logit"),
                  data = survpredflow)
summary(sec8.2flow)

ndsec8.2flow <- with(survpredflow, data.frame(t3=mean(t3),
                                              empstat2num=mean(empstat2num, na.rm=TRUE),
                                              # sexnum=mean(sexnum, na.rm=TRUE),
                                              difficultfac=factor(1:2),
                                              agemn=mean(agemn),
                                              agesq=mean(agesq),
                                              ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                              cci=mean(cci),
                                              immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                              combonum=mean(combonum, na.rm=TRUE)))

ndsec8.2flow$eventPP <- predict(sec8.2flow, ndsec8.2flow, type = "response")

###women - medium Edu
sec8.2fmedium <- glm(formula = event ~ t3 + empstat2num + difficultfac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                     family = binomial(link = "logit"),
                     data = survpredfmedium)
summary(sec8.2fmedium)

ndsec8.2fmedium <- with(survpredfmedium, data.frame(t3=mean(t3),
                                                    empstat2num=mean(empstat2num, na.rm=TRUE),
                                                    # sexnum=mean(sexnum, na.rm=TRUE),
                                                    difficultfac=factor(1:2),
                                                    agemn=mean(agemn),
                                                    agesq=mean(agesq),
                                                    ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                    cci=mean(cci),
                                                    immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                    combonum=mean(combonum, na.rm=TRUE)))

ndsec8.2fmedium$eventPP <- predict(sec8.2fmedium, ndsec8.2fmedium, type = "response")

###women - high Edu
sec8.2fhigh <- glm(formula = event ~ t3 + empstat2num + difficultfac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                   family = binomial(link = "logit"),
                   data = survpredfhigh)
summary(sec8.2fhigh)

ndsec8.2fhigh <- with(survpredfhigh, data.frame(t3=mean(t3),
                                                empstat2num=mean(empstat2num, na.rm=TRUE),
                                                # sexnum=mean(sexnum, na.rm=TRUE),
                                                difficultfac=factor(1:2),
                                                agemn=mean(agemn),
                                                agesq=mean(agesq),
                                                ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                cci=mean(cci),
                                                immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                combonum=mean(combonum, na.rm=TRUE)))

ndsec8.2fhigh$eventPP <- predict(sec8.2flow, ndsec8.2fhigh, type = "response")

# Men ---------------------------------------------------------------------

###Men - Low Edu
sec8.2mlow <- glm(formula = event ~ t3 + empstat2num + difficultfac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                  family = binomial(link = "logit"),
                  data = survpredmlow)
summary(sec8.2mlow)

ndsec8.2mlow <- with(survpredmlow, data.frame(t3=mean(t3),
                                              empstat2num=mean(empstat2num, na.rm=TRUE),
                                              # sexnum=mean(sexnum, na.rm=TRUE),
                                              difficultfac=factor(1:2),
                                              agemn=mean(agemn),
                                              agesq=mean(agesq),
                                              ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                              cci=mean(cci),
                                              immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                              combonum=mean(combonum, na.rm=TRUE)))

ndsec8.2mlow$eventPP <- predict(sec8.2mlow, ndsec8.2mlow, type = "response")

###Men - medium Edu
sec8.2mmedium <- glm(formula = event ~ t3 + empstat2num + difficultfac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                     family = binomial(link = "logit"),
                     data = survpredmmedium)
summary(sec8.2mmedium)

ndsec8.2mmedium <- with(survpredmmedium, data.frame(t3=mean(t3),
                                                    empstat2num=mean(empstat2num, na.rm=TRUE),
                                                    # sexnum=mean(sexnum, na.rm=TRUE),
                                                    difficultfac=factor(1:2),
                                                    agemn=mean(agemn),
                                                    agesq=mean(agesq),
                                                    ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                    cci=mean(cci),
                                                    immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                    combonum=mean(combonum, na.rm=TRUE)))

ndsec8.2mmedium$eventPP <- predict(sec8.2mmedium, ndsec8.2mmedium, type = "response")

###Men - high Edu
sec8.2mhigh <- glm(formula = event ~ t3 + empstat2num + difficultfac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                   family = binomial(link = "logit"),
                   data = survpredmhigh)
summary(sec8.2mhigh)

ndsec8.2mhigh <- with(survpredmhigh, data.frame(t3=mean(t3),
                                                empstat2num=mean(empstat2num, na.rm=TRUE),
                                                # sexnum=mean(sexnum, na.rm=TRUE),
                                                difficultfac=factor(1:2),
                                                agemn=mean(agemn),
                                                agesq=mean(agesq),
                                                ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                cci=mean(cci),
                                                immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                combonum=mean(combonum, na.rm=TRUE)))

ndsec8.2mhigh$eventPP <- predict(sec8.2mlow, ndsec8.2mhigh, type = "response")






# -------------------------------------------------------------------------
# Section 8.3 Worse -------------------------------------------------------
# -------------------------------------------------------------------------

# Women -------------------------------------------------------------------

###women - Low Edu
sec8.3flow <- glm(formula = event ~ t3 + empstat2num + worsefac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                  family = binomial(link = "logit"),
                  data = survpredflow)
summary(sec8.3flow)

ndsec8.3flow <- with(survpredflow, data.frame(t3=mean(t3),
                                              empstat2num=mean(empstat2num, na.rm=TRUE),
                                              # sexnum=mean(sexnum, na.rm=TRUE),
                                              worsefac=factor(1:2),
                                              agemn=mean(agemn),
                                              agesq=mean(agesq),
                                              ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                              cci=mean(cci),
                                              immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                              combonum=mean(combonum, na.rm=TRUE)))

ndsec8.3flow$eventPP <- predict(sec8.3flow, ndsec8.3flow, type = "response")

###women - medium Edu
sec8.3fmedium <- glm(formula = event ~ t3 + empstat2num + worsefac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                     family = binomial(link = "logit"),
                     data = survpredfmedium)
summary(sec8.3fmedium)

ndsec8.3fmedium <- with(survpredfmedium, data.frame(t3=mean(t3),
                                                    empstat2num=mean(empstat2num, na.rm=TRUE),
                                                    # sexnum=mean(sexnum, na.rm=TRUE),
                                                    worsefac=factor(1:2),
                                                    agemn=mean(agemn),
                                                    agesq=mean(agesq),
                                                    ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                    cci=mean(cci),
                                                    immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                    combonum=mean(combonum, na.rm=TRUE)))

ndsec8.3fmedium$eventPP <- predict(sec8.3fmedium, ndsec8.3fmedium, type = "response")

###women - high Edu
sec8.3fhigh <- glm(formula = event ~ t3 + empstat2num + worsefac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                   family = binomial(link = "logit"),
                   data = survpredfhigh)
summary(sec8.3fhigh)

ndsec8.3fhigh <- with(survpredfhigh, data.frame(t3=mean(t3),
                                                empstat2num=mean(empstat2num, na.rm=TRUE),
                                                # sexnum=mean(sexnum, na.rm=TRUE),
                                                worsefac=factor(1:2),
                                                agemn=mean(agemn),
                                                agesq=mean(agesq),
                                                ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                cci=mean(cci),
                                                immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                combonum=mean(combonum, na.rm=TRUE)))

ndsec8.3fhigh$eventPP <- predict(sec8.3flow, ndsec8.3fhigh, type = "response")

# Men ---------------------------------------------------------------------

###Men - Low Edu
sec8.3mlow <- glm(formula = event ~ t3 + empstat2num + worsefac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                  family = binomial(link = "logit"),
                  data = survpredmlow)
summary(sec8.3mlow)

ndsec8.3mlow <- with(survpredmlow, data.frame(t3=mean(t3),
                                              empstat2num=mean(empstat2num, na.rm=TRUE),
                                              # sexnum=mean(sexnum, na.rm=TRUE),
                                              worsefac=factor(1:2),
                                              agemn=mean(agemn),
                                              agesq=mean(agesq),
                                              ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                              cci=mean(cci),
                                              immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                              combonum=mean(combonum, na.rm=TRUE)))

ndsec8.3mlow$eventPP <- predict(sec8.3mlow, ndsec8.3mlow, type = "response")

###Men - medium Edu
sec8.3mmedium <- glm(formula = event ~ t3 + empstat2num + worsefac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                     family = binomial(link = "logit"),
                     data = survpredmmedium)
summary(sec8.3mmedium)

ndsec8.3mmedium <- with(survpredmmedium, data.frame(t3=mean(t3),
                                                    empstat2num=mean(empstat2num, na.rm=TRUE),
                                                    # sexnum=mean(sexnum, na.rm=TRUE),
                                                    worsefac=factor(1:2),
                                                    agemn=mean(agemn),
                                                    agesq=mean(agesq),
                                                    ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                    cci=mean(cci),
                                                    immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                    combonum=mean(combonum, na.rm=TRUE)))

ndsec8.3mmedium$eventPP <- predict(sec8.3mmedium, ndsec8.3mmedium, type = "response")

###Men - high Edu
sec8.3mhigh <- glm(formula = event ~ t3 + empstat2num + worsefac + agemn + agesq + ol5catnum + cci + immigrantnum + combonum,
                   family = binomial(link = "logit"),
                   data = survpredmhigh)
summary(sec8.3mhigh)

ndsec8.3mhigh <- with(survpredmhigh, data.frame(t3=mean(t3),
                                                empstat2num=mean(empstat2num, na.rm=TRUE),
                                                # sexnum=mean(sexnum, na.rm=TRUE),
                                                worsefac=factor(1:2),
                                                agemn=mean(agemn),
                                                agesq=mean(agesq),
                                                ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                cci=mean(cci),
                                                immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                combonum=mean(combonum, na.rm=TRUE)))

ndsec8.3mhigh$eventPP <- predict(sec8.3mlow, ndsec8.3mhigh, type = "response")





# -------------------------------------------------------------------------
# Section 8.4 Combo -------------------------------------------------------
# -------------------------------------------------------------------------

# Women -------------------------------------------------------------------

###women - Low Edu
sec8.4flow <- glm(formula = event ~ t3 + empstat2num + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combofac,
                  family = binomial(link = "logit"),
                  data = survpredflow)
summary(sec8.4flow)

ndsec8.4flow <- with(survpredflow, data.frame(t3=mean(t3),
                                              empstat2num=mean(empstat2num, na.rm=TRUE),
                                              # sexnum=mean(sexnum, na.rm=TRUE),
                                              difficultnum=mean(difficultnum, na.rm=TRUE),
                                              agemn=mean(agemn),
                                              agesq=mean(agesq),
                                              ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                              cci=mean(cci),
                                              immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                              combofac=factor(1:7)))

ndsec8.4flow$eventPP <- predict(sec8.4flow, ndsec8.4flow, type = "response")

###women - medium Edu
sec8.4fmedium <- glm(formula = event ~ t3 + empstat2num + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combofac,
                     family = binomial(link = "logit"),
                     data = survpredfmedium)
summary(sec8.4fmedium)

ndsec8.4fmedium <- with(survpredfmedium, data.frame(t3=mean(t3),
                                                    empstat2num=mean(empstat2num, na.rm=TRUE),
                                                    # sexnum=mean(sexnum, na.rm=TRUE),
                                                    difficultnum=mean(difficultnum, na.rm=TRUE),
                                                    agemn=mean(agemn),
                                                    agesq=mean(agesq),
                                                    ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                    cci=mean(cci),
                                                    immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                    combofac=factor(1:7)))

ndsec8.4fmedium$eventPP <- predict(sec8.4fmedium, ndsec8.4fmedium, type = "response")

###women - high Edu
sec8.4fhigh <- glm(formula = event ~ t3 + empstat2num + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combofac,
                   family = binomial(link = "logit"),
                   data = survpredfhigh)
summary(sec8.4fhigh)

ndsec8.4fhigh <- with(survpredfhigh, data.frame(t3=mean(t3),
                                                empstat2num=mean(empstat2num, na.rm=TRUE),
                                                # sexnum=mean(sexnum, na.rm=TRUE),
                                                difficultnum=mean(difficultnum, na.rm=TRUE),
                                                agemn=mean(agemn),
                                                agesq=mean(agesq),
                                                ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                cci=mean(cci),
                                                immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                combofac=factor(1:7)))

ndsec8.4fhigh$eventPP <- predict(sec8.4flow, ndsec8.4fhigh, type = "response")

# Men ---------------------------------------------------------------------

###Men - Low Edu
sec8.4mlow <- glm(formula = event ~ t3 + empstat2num + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combofac,
                  family = binomial(link = "logit"),
                  data = survpredmlow)
summary(sec8.4mlow)

ndsec8.4mlow <- with(survpredmlow, data.frame(t3=mean(t3),
                                              empstat2num=mean(empstat2num, na.rm=TRUE),
                                              # sexnum=mean(sexnum, na.rm=TRUE),
                                              difficultnum=mean(difficultnum, na.rm=TRUE),
                                              agemn=mean(agemn),
                                              agesq=mean(agesq),
                                              ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                              cci=mean(cci),
                                              immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                              combofac=factor(1:7)))

ndsec8.4mlow$eventPP <- predict(sec8.4mlow, ndsec8.4mlow, type = "response")

###Men - medium Edu
sec8.4mmedium <- glm(formula = event ~ t3 + empstat2num + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combofac,
                     family = binomial(link = "logit"),
                     data = survpredmmedium)
summary(sec8.4mmedium)

ndsec8.4mmedium <- with(survpredmmedium, data.frame(t3=mean(t3),
                                                    empstat2num=mean(empstat2num, na.rm=TRUE),
                                                    # sexnum=mean(sexnum, na.rm=TRUE),
                                                    difficultnum=mean(difficultnum, na.rm=TRUE),
                                                    agemn=mean(agemn),
                                                    agesq=mean(agesq),
                                                    ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                    cci=mean(cci),
                                                    immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                    combofac=factor(1:7)))

ndsec8.4mmedium$eventPP <- predict(sec8.4mmedium, ndsec8.4mmedium, type = "response")

###Men - high Edu
sec8.4mhigh <- glm(formula = event ~ t3 + empstat2num + difficultnum + agemn + agesq + ol5catnum + cci + immigrantnum + combofac,
                   family = binomial(link = "logit"),
                   data = survpredmhigh)
summary(sec8.4mhigh)

ndsec8.4mhigh <- with(survpredmhigh, data.frame(t3=mean(t3),
                                                empstat2num=mean(empstat2num, na.rm=TRUE),
                                                # sexnum=mean(sexnum, na.rm=TRUE),
                                                difficultnum=mean(difficultnum, na.rm=TRUE),
                                                agemn=mean(agemn),
                                                agesq=mean(agesq),
                                                ol5catnum=mean(ol5catnum, na.rm=TRUE),
                                                cci=mean(cci),
                                                immigrantnum=mean(immigrantnum, na.rm=TRUE),
                                                combofac=factor(1:7)))

ndsec8.4mhigh$eventPP <- predict(sec8.4mlow, ndsec8.4mhigh, type = "response")




