#Coded by: Brian Buh
#Started on: 30.06.2021
#Last Updated: 


library(margins)
library(tidyverse)

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")


###########################################################################
# Sensitivity Analysis - finnow -------------------------------------------
###########################################################################

# There are several alternative ways to group the finnow variables together
## 1. No grouping, 5 categories
## 2. Grouped into 3 categories - high, medium, low
## 3. Two groupings but move the middle "getting" by to the other group
## 4. Numerical

# -------------------------------------------------------------------------
# Test 1 - 5 categories ---------------------------------------------------
# -------------------------------------------------------------------------

## Men
### No partner
sa1t1m <- glm(formula = event ~ t3 + empstat2 + finnow.imp + agemn + agesq + immigrant + edu + ol5cat + cci,
             family = binomial(link = "logit"),
             data = surv4m)
summary(margins(sa1t1m))
## Result - no significant results

### with partner
sa1t1m2 <- glm(formula = event ~ t3 + empstat2 + finnow.imp + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
              family = binomial(link = "logit"),
              data = surv4m)
summary(margins(sa1t1m2))
## Result - no significant results

## Women
### No partner
sa1t1f <- glm(formula = event ~ t3 + empstat2 + finnow.imp + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4f)
summary(margins(sa1t1f))
## Result - no significant results

### with partner
sa1t1f2 <- glm(formula = event ~ t3 + empstat2 + finnow.imp + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4f)
summary(margins(sa1t1f2))
## Result - We see some significant results for "doing fine" when the reference is "very difficult" however it is negative and impossible to interpret

### Conclusion - 5 distinct groups gives no significant results and cannot be interpreted

# -------------------------------------------------------------------------
# Test 2 - 3 categories ---------------------------------------------------
# -------------------------------------------------------------------------

## Men
### No partner
sa1t2m <- glm(formula = event ~ t3 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4m)
summary(margins(sa1t2m))
## Result - We see the results that difficult = positive relationship

### with partner
sa1t2m2 <- glm(formula = event ~ t3 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4m)
summary(margins(sa1t2m2))
## Result - We see the results that difficult = positive relationship after partnership as well

## Women
### No partner
sa1t2f <- glm(formula = event ~ t3 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4f)
summary(margins(sa1t2f))
## Result - We see the results that the middle category "getting by" = positive significant relationship

### with partner
sa1t2f2 <- glm(formula = event ~ t3 + empstat2 + finnow3cat + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4f)
summary(margins(sa1t2f2))
## Result - After adding partnership no more significant results

### Conclusion - We get the confusing result for men and nothing interpretable for women

# -------------------------------------------------------------------------
# Test 3 - Alternative Difficult ------------------------------------------
# -------------------------------------------------------------------------

## Men
### No partner
sa1t3m <- glm(formula = event ~ t3 + empstat2 + difficultv2 + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4m)
summary(margins(sa1t3m))
## Result - difficult is positive and significant

### with partner
sa1t3m2 <- glm(formula = event ~ t3 + empstat2 + difficultv2 + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4m)
summary(margins(sa1t3m2))
## Result - difficult is positive and significant

## Women
### No partner
sa1t3f <- glm(formula = event ~ t3 + empstat2 + difficultv2 + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4f)
summary(margins(sa1t3f))
## Result - No significant result

### with partner
sa1t3f2 <- glm(formula = event ~ t3 + empstat2 + difficultv2 + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4f)
summary(margins(sa1t3f2))
## Result - No significant result

### Conclusion - When we put difficult on one side we seem to have significant results for men. This mirrors what we see with our analysis. The AME is slightly larger here, but nothing to reason to make a change.

# -------------------------------------------------------------------------
# Test 4 - Numerical ------------------------------------------------------
# -------------------------------------------------------------------------

## Men
### No partner
sa1t4m <- glm(formula = event ~ t3 + empstat2 + finnow.num + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4m)
summary(margins(sa1t4m))
## Result - We see a relationship as individuals get more optomistic and negative significant

### with partner
sa1t4m2 <- glm(formula = event ~ t3 + empstat2 + finnow.num + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4m)
summary(margins(sa1t4m2))
## Result - We see a relationship as individuals get more optomistic and negative significant

## Women
### No partner
sa1t4f <- glm(formula = event ~ t3 + empstat2 + finnow.num + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4f)
summary(margins(sa1t4f))
## Result - slight positive result and significant

### with partner
sa1t4f2 <- glm(formula = event ~ t3 + empstat2 + finnow.num + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4f)
summary(margins(sa1t4f2))
## Result - No significant result!

### Conclusion - We have a similar results. We do see positive results for women, but I struggle to intepret it as a continous variable, and NOT after adding partnership


###########################################################################
# Sensitivity Analysis 2 - finfut -----------------------------------------
###########################################################################

# There are two possible alternative groupings
## 1. No grouping
## 2. Group "Worse" and "same" together (called "better")

# -------------------------------------------------------------------------
# Test 2 - 3 categories ---------------------------------------------------
# -------------------------------------------------------------------------

## Men
### No partner
sa2t1m <- glm(formula = event ~ t3 + empstat2 + finfut.imp + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4m)
summary(margins(sa2t1m))
## Result - no significant results

### with partner
sa2t1m2 <- glm(formula = event ~ t3 + empstat2 + finfut.imp + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4m)
summary(margins(sa2t1m2))
## Result - no significant results; interesting the AME for "about the same" is close to 0 indicating it is close to the reference "better"

## Women
### No partner
sa2t1f <- glm(formula = event ~ t3 + empstat2 + finfut.imp + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4f)
summary(margins(sa2t1f))
## Result -  "About the same" & "Worse" - significant but the reference is "better" so that is hard to interpret

### with partner
sa2t1f2 <- glm(formula = event ~ t3 + empstat2 + finfut.imp + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4f)
summary(margins(sa2t1f2))
## Result - Same as without partner

### Conclusion - We see that there are a lot of people in the middle category that appear to swing towards the worse side


# -------------------------------------------------------------------------
# Test 2 - Worse ---------------------------------------------------------
# -------------------------------------------------------------------------

## Men
### No partner
sa2t2m <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4m)
summary(margins(sa2t2m))
## Result - no significant results

### with partner
sa2t2m2 <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4m)
summary(margins(sa2t2m2))
## Result - no significant results

## Women
### No partner
sa2t2f <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + edu + ol5cat + cci,
              family = binomial(link = "logit"),
              data = surv4f)
summary(margins(sa2t2f))
## Result -  Big result!!! Women who see themselves as same as worse have a positive relationship

### with partner
sa2t2f2 <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + immigrant + edu + ol5cat + cci + combo,
               family = binomial(link = "logit"),
               data = surv4f)
summary(margins(sa2t2f2))
## Result - Same as without partner

### Conclusion - Better likely gives more consitence significance and strong coefficents because it a larger proportion of the total versus worse (~50% to ~10%).
### ...This also helps with the general trend we see in the employment status with high educated women entering motherhood when their career starts to plain.




