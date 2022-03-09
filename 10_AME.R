#Coded by: Brian Buh
#Started on: 09.03.2022
#Last Updated: 

install.packages("margins")

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

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")


m <- margins(a1m2f)
summary(m)
plot(m)
