#Coded by: Brian Buh
#Started on: 25.02.2022
#Last Updated:

library(data.table)
library(padr)
library(tidyverse)
library(haven)
library(lubridate)
library(arsenal)
library(zoo)

###########################################################################
# Creating starting and ending dates --------------------------------------
###########################################################################

com_panel <- file.choose()
com_panel <- readRDS(com_panel)

enddate <- com_panel %>% 
  unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  mutate(intdate = parse_date_time(intdate, "my")) %>% 
  dplyr::select(pidp, wave, intdate)

foo <- function(x) coredata(na.approx(zoo(x), na.rm = FALSE))
enddate2 <- data.table(enddate)
enddate2[, intdate := foo(intdate), by = "pidp"]
# DT[, enddate := foo(enddate), by = "pidp"]

enddate3 <- enddate2 %>% 
  mutate(wave = wave - 1) %>% 
  filter(wave != 0) %>% 
  rename("enddate" = "intdate")

enddate4 <- 
  left_join(enddate, enddate3, by = c("pidp", "wave")) %>% 
  rename("startdate" = "intdate")

#In order to make sure I can quickly manipulate dates in the future for needed changes in dataset structure
saveRDS(enddate4, file = "intdates.rds")
intdates <- file.choose()
intdates <- readRDS(intdates)

#Combining the start/end dates back into the dataset
com_panel2 <- com_panel %>% 
  mutate(imp = ifelse(is.na(dvage), 1, 0)) %>% 
  left_join(., enddate4, by = c("pidp", "wave")) %>% 
  filter(fwtest >= 0, lwtest >= 0)%>% #removes rows before the first int or after the last
  # unite(intdate, c(intdatm_dv, intdaty_dv), sep = "-") %>% #combines the mnth & yr var to make int dates
  # mutate(intdate = parse_date_time(intdate, "my")) %>% 
  unite(dob, c(birthm, birthy), sep = "-") %>% #creates a dob
  mutate(dob = parse_date_time(dob, "my")) %>% 
  # mutate(age = (intdate - dob)/(365*24*60*60)) %>% #Done so I can use age at each wave
  group_by(pidp) %>%
  fill(fb_check) %>%
  fill(dvage) %>%
  fill(gor_dv) %>%
  fill(racel_dv) %>%
  fill(generation) %>%
  fill(combo) %>%
  ungroup() %>%
  filter(fb_check == 0 | fb_check == 2) #this variable takes the observed "anychild" and subtracts the binary "kdob observed" 1 = had child but no kdob or not had child but observed kdob
  # filter(case_when(sex == 1 ~ age_start <= 50, sex == 2 ~ age_start <= 45) %>% 
  # dplyr::select(pidp, wave, imp, hhorig, kdob, sex, dvage, dob, racel_dv, gor_dv, ppid, marstat, parjbstat, combo, jbsec, generation,
  #               edu_cat, isced97, jbisco88_cc, finnow.imp, finfut.imp, startdate, enddate)

com_panel2 %>% 
  count(combo)

#In order to fill in start dates from missing waves
#Calculated as the median point between the two waves
foo <- function(x) coredata(na.approx(zoo(x), na.rm = FALSE))
com_panel3 <- data.table(com_panel2)
com_panel3[, startdate := foo(startdate), by = "pidp"]
# DT[, enddate := foo(enddate), by = "pidp"]


#Some median dates are halfway between the month
#The "floor_date" function in lubridate didn't work, so this is a function to round to start of month
firstOfMonth <- function(dates) {
  as.Date(strftime(dates, format="%Y-%m-01"))}


com_panel4 <- com_panel3 %>% 
  mutate(startdate = firstOfMonth(startdate)) %>% 
  mutate(enddate = firstOfMonth(enddate)) %>% 
  mutate(gap = as.duration(startdate %--% enddate) / dmonths(1)) %>% 
  mutate(gap = round(gap, digits = 0)) %>% 
  group_by(pidp) %>% 
  mutate(rollgap = cumsum(gap)) %>% 
  mutate(condate = as.Date(kdob %m-% months(9))) %>% 
  mutate(fbgap = as.duration(startdate %--% condate) / dmonths(1)) %>% 
  mutate(test = ifelse(is.na(enddate), 1, 0)) %>% 
  fill(enddate) %>% 
  mutate(enddate = ifelse(test == 1, enddate %m+% months(1), enddate)) %>% 
  mutate(enddate = as.Date(enddate))

com_panel5 <- com_panel4 %>% 
  mutate(condate = ifelse(enddate <= condate, NA, condate)) %>% #The next several lines is done to remove waves after first birth
  mutate(condate = as.Date(condate)) %>% 
  mutate(condate = ifelse(is.na(condate), 1900-01-01, condate)) %>% 
  mutate(condate = as.Date(condate)) %>% 
  mutate(enddate2 = ifelse(condate <= enddate, condate, enddate)) %>% 
  mutate(enddate2 = ifelse(enddate2 == 1898, enddate, enddate2)) %>% 
  mutate(enddate2 = as.Date(enddate2)) %>% 
  mutate(cut = ifelse(startdate > enddate2, 1, 0)) %>% 
  filter(cut != 1) %>% 
  mutate(gap2 = as.duration(startdate %--% enddate2) / dmonths(1)) %>% 
  mutate(gap2 = round(gap2, digits = 0)) %>% 
  group_by(pidp) %>% 
  mutate(rollgap2 = cumsum(gap2)) %>% 
  mutate(time1 = rollgap2 - gap2) %>% 
  rename("time2" = "rollgap2") %>% 
  mutate(event = ifelse(condate == enddate2, 1, 0)) %>% 
  dplyr::select(-gap2, -cut, -enddate, -gap, -rollgap, -fbgap, -condate, -test) %>% 
  rename("enddate" = "enddate2") %>% 
  mutate(dob = as.Date(dob)) %>% 
  mutate(agemn = as.duration(dob %--% startdate) / dmonths(1)) %>% 
  mutate(agemn = round(agemn, digits = 0)) %>% 
  mutate(agesq = agemn*agemn) %>% 
  filter(case_when(sex == 1 ~ agemn <= 600, sex == 2 ~ agemn <= 540)) %>%  #filters men over 50 and women over 45
  ungroup()

#There is an issue where some end dates are the same as the start dates because the baby was born the same month as the interview
#This is solved by adding one month to the end date
com_panel6 <- com_panel5 %>% 
  mutate(test = time2 - time1) %>% 
  mutate(test2 = ifelse(test <= 0, 1, 0)) %>% 
  ungroup() %>% 
  mutate(time2 = test2 + time2) %>% 
  dplyr::select(-test, -test2)


#Save and load the combined individual data file as an RDS
saveRDS(com_panel6, file = "surv.rds") #If rerunning all code this needs to be uncommented
surv <- file.choose()
surv <- readRDS(surv)

# Load the RDS "surv"

###########################################################################
# Adjusting Education discrepencies due to mismatches in reporting --------
###########################################################################

#The following things have been added
#1. changes times for start of education
#2. add a numerical variable for the subjective measures


surv2 <- surv %>% 
  left_join(., edu_adj, by = c("pidp", "wave")) %>% 
  mutate(finnow.imp = fct_relevel(finnow.imp, c("5 Finding it very difficult", "4 Finding it quite difficult",
                                                "3 Just getting by", "2 Doing alright", "1 Living comfortably"))) %>%
  mutate(finnow.num = as.numeric(finnow.imp)) %>% 
  #I change the scale of finfut to be centered at 0
  mutate(finfut.imp = fct_relevel(finfut.imp, c( "Worse off", "About the same", "Better off"))) %>% 
  mutate(finfut.num = as.numeric(finfut.imp)) %>% 
  mutate(finfut.num = recode(finfut.num,
                             "2" = "0",
                             "1" = "-1",
                             "3" = "1")) %>% 
  mutate(finfut.num = as.integer(finfut.num)) %>% 
  group_by(pidp) %>% 
  fill(employed, .direction = "down") %>% 
  ungroup() %>% 
  mutate(neg = ifelse(t2 < 0, 1, 0)) %>% 
  mutate(negstu = ifelse(neg == 1 & jbstat == "Full-time student", 1, 0)) %>% 
  filter(negstu == 0, t2 > -13) %>%  #Removes students with reporting errors on finishing full-time education and people with an error bigger than 1 year
  #This removes 15,488 observations
  mutate(t2 = ifelse(t2 < 0, 0, t2)) #changes errors smaller than 1 year to a t2 of 0


saveRDS(surv2, file = "surv2.rds")
surv2 <- file.choose()
surv2 <- readRDS(surv2)

# -------------------------------------------------------------------------
# Testing to deal with the negative t2 numbers ----------------------------
# -------------------------------------------------------------------------

#There is clearly many people who have periods of being a full-time student after finishing their formal education
surv2 %>% 
  count(neg)

#Looking at negative values in my t2 distribution
negt2 <- surv2 %>% 
  filter(t2 <= 0) %>% 
  mutate(cutoff = ifelse(t2 > -12, 0, 1))

#10 births happen in "negative time"
#There are 2341 observations with negative time
#full-time students account for 2032 of these - to me this means that the self-report end of education 
#is off and the fulltime student observations should be thrown out
negt2 %>% 
  count(event)

#7 events occur for full-time 
negt2 %>% 
  count(jbstat, event)

summary(negt2$t2)

#an almost perfect even spread have less than 1 year off
negt2 %>% 
  count(cutoff)

#The histogram shows that most errors are clustered around 0, 12, and 24 months
#most likely there is a small mismatching months or false year reporting
negt2 %>% 
  ggplot(aes(t2)) +
  geom_histogram(binwidth = 2)

surv2 %>% 
  filter(t2 < 0) %>% 
  ggplot(aes(t2)) +
  geom_histogram(binwidth = 2)

negt2_2 <- negt2 %>% 
  filter(jbstat != "Full-time student")

negt2_2 %>% 
  count(cutoff)
