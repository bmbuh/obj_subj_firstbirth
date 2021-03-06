#Coded by: Brian Buh
#Started on: 25.02.2022
#Last Updated: 22.07.2022


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
# library(joots) #not available for this version of R
library(broom.mixed)
library(effects)
library(mice)


# Loading data surv2
surv2 <- file.choose()
surv2 <- readRDS(surv2)

#Load Consumer Confidence Index Data (OECD)
cci <- file.choose()
cci <- readRDS(cci)

############################################################################
# Create surv3 -------------------------------------------------------------
############################################################################

# -------------------------------------------------------------------------
# Extracting emphist and partnerhist --------------------------------------
# -------------------------------------------------------------------------


###surv3 adds in missing partnership and employment from imputed waves
#The DF "annual_his" and "part_his" from script 1 are needed

#First employment - this includes data about employment status and part time work
emp <- annual_his %>% 
  dplyr::select(pidp, Wave, Spell, start_m, start_y, end_m, end_y, int_m, int_y,
                Job_Hours, Status) %>% 
  mutate(wave = Wave - 18) %>%
  #Status 9 Govt training scheme and 11 Apprenticeship will be changed to education even though the often receive enumeration.
  ## This is because even though they get paid, it is still general seen as an educational time
  ### Also, often their status changes to "Full time student" in later observations and counting it as work misconstrues that age of enjd of education
  mutate(Status = ifelse(Status == 9 | Status == 11, 7, Status)) %>%
  mutate(jbstat2 = as.character(Status)) %>% 
  mutate(jbstat2 = recode(jbstat2,
                         "1" = "Self-employed",
                         "2" = "Paid employed",
                         "3" = "Unemployed",
                         "4" = "Retired",
                         "5" = "Maternity leave",
                         "6" = "Family care",
                         "7" = "Full-time student",
                         "8" = "Sick/disabled",
                         # "9" = "Govt training scheme",
                         "10" = "Unpaid fam bus",
                         # "11" = "Apprenticeship",
                         "97" = "Something else",
                         "100" = "Paid employed",
                         "101" = "Something else")) %>% 
  mutate(parttime2 = Job_Hours) %>% #1 = FT, 2 = PT
  mutate(parttime2 = ifelse(is.na(parttime2), NA, ifelse(parttime2 == 2, 1, 0))) %>% 
  dplyr::select(-Wave, -Status, -Job_Hours)


emp %>% count(Status)
emp %>% count(jbstat2)

emp2 <- emp %>% 
  select(pidp, wave, jbstat2, parttime2, start_y) %>% 
  rename("waveyr2" = "start_y") %>% 
  rename("waveck" = "wave")

part <- part_his %>% 
  select(pidp, status, starty) %>% 
  mutate(status2 = as.numeric(status)) %>% 
  mutate(status2 = ifelse(status2 == 2 | status2 == 3, "married", "cohab")) %>% 
  rename("waveyr2" = "starty") %>% 
  select(-status)


# -------------------------------------------------------------------------
# Making surv3 ------------------------------------------------------------
# -------------------------------------------------------------------------

#Section 5 - Education - Check how many others where fixed or dropped
check <- surv2 %>% count(isced97, edu_cat)

###surv3 fixes issues due to immigrant and foreign educational attainment
# The variable "edu_cat" does not include immigrant education
# #This step updates the educational attainment variable
surv3.1 <- surv2 %>% 
  mutate(jbstat = ifelse(jbstat == "Apprenticeship" | jbstat == "Govt training scheme", "Full-time student", jbstat)) %>%
  group_by(pidp) %>%
  mutate(rownum = row_number()) %>% 
  ungroup() %>%
  mutate(age = agemn/12) %>%
  relocate("rownum", .after = "wave") %>%
  relocate("age", .after = "sex") %>%
  relocate("jbstat", .after = "age") %>%
  ungroup() %>%
  mutate(waveyr = wave + 2008) %>% #The following steps are done to make sure that the years from the employment file match the waves here
  mutate(check = intdaty_dv - waveyr) %>% 
  mutate(waveyr2 = waveyr + check) %>% 
  mutate(check2 = intdaty_dv - waveyr) %>% 
  mutate(waveyr2 = ifelse(is.na(waveyr2), waveyr, waveyr2)) %>% 
  select(-check, -check2) %>% 
  relocate("waveyr2", .after = "wave") %>%
  #Adding in the employment histories to fill in NA
  left_join(., emp2, by = c("pidp", "waveyr2")) %>% #join the DF "emp2" created above
  group_by(pidp) %>% 
  fill(jbstat2, .direction = "down") %>% #The waves got joined based on employment starting date. Since I have a emphist we can assume no change occurs until the next observation
  distinct(wave, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(jbstat = ifelse(is.na(jbstat), jbstat2, jbstat)) %>% #replaces NA from imputed waves with known employment status
  mutate(parttime = ifelse(is.na(parttime) & jbstat == "Paid employed", parttime2, parttime)) %>% #replaces NA from imputed waves with known parttime work
  mutate(parttime = ifelse(jbstat == "Self-employed", 0, parttime)) %>% #since self-employment general means working hours and personal hours overlap, the job is theoretically always fulltime
  mutate(parttime = ifelse(is.na(parttime) & jbstat == "Paid employed", "x", parttime)) %>% #The following steps are for adding parttime to imputed waves
  mutate_at(vars(parttime), function(y) ifelse(y == "x", lag(y), y)) %>% #First observations going forward
  mutate_at(vars(parttime), function(y) ifelse(y == "x", lead(y), y)) %>% #Remainder observations going backwards
  mutate(parttime = ifelse(is.na(parttime), "z", ifelse(parttime == "x", NA, parttime))) %>% 
  group_by(pidp) %>% 
  fill(jbstat, .direction = "downup") %>% #Fills in observations where no change was seen in the emphist
  fill(parttime, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(parttime = ifelse(parttime == "z", NA, parttime)) %>% #Safely transforms NAs back to an NA
  select(-waveyr, -jbstat2, -parttime2) %>% 
  #Adding in the partnership histories
  left_join(., part, by = c("pidp", "waveyr2")) %>% 
  mutate(marstat = ifelse(is.na(marstat), status2, marstat)) %>%  #this removes a very small number of NA (226), meaning far less missing partnership changes in imputed waves
  group_by(pidp) %>% 
  distinct(wave, .keep_all = TRUE) %>% 
  fill(marstat, .direction = "downup") %>% #It can be assumed missing NA from imputed waves were non-changes in status from the previous wave
  ungroup() %>% 
  # filter(edu_cat != "other") %>% #I made the decision to remove other category as it is mainly people who were not raised in the UK
  mutate(worse = ifelse(finfut.num == -1, 1, 0)) %>%  #A binary variable for people who think their finances will get worse
  mutate(comf = ifelse(finnow.num > 2, 1, 0)) %>%  #Creates a binary for positive versus negative current financial stability
  mutate(employed = ifelse(is.na(employed), 0, employed)) %>% 
  mutate(edu = case_when(
    isced97 == 2 ~ "low",
    isced97 == 3 | isced97 == 4  ~ "medium",
    isced97 == 5 | isced97 == 6  ~ "high",
    is.na(isced97) ~ "other")) %>% #Other now represents all individuals with unknown educational backgrounds
  filter(edu != "other") %>% #This removes 2513 observations from the DF
  select(-combo) %>% #Final step is redoing the "combo" variable with the updated information
  group_by(pidp) %>% 
  fill(parjbstat, .direction = "downup") %>% 
  ungroup() %>% 
  unite(combo, marstat, parjbstat, sep = "-", remove = FALSE) %>% 
  #Adding CCI to the DF
  left_join(., cci, by = "startdate") %>% 
  filter(pidp != 749247131)

surv3.1 %>% count(jbstat)

# saveRDS(surv3.1, "surv3.rds")

##I appear to have quite a few "Full-time students" continuous from childhood in my sample still
#This DF notes which jbstat are "Full-time students" from first observation (7504 observations or 14.6%!!!)
eduissue <- surv3.1 %>%
  select(pidp, wave, rownum, age, jbstat, t2) %>%
  group_by(pidp) %>%
  mutate(rownumck = row_number()) %>%
  ungroup() %>%
  mutate(remove = ifelse(rownum == 1 & jbstat == "Full-time student", 1, 
                         ifelse(jbstat != "Full-time student", 0, NA))) %>%
  group_by(pidp) %>%
  fill(remove, .direction = "down") %>%
  ungroup() %>%
  mutate(remove = ifelse(is.na(remove), 0, remove)) %>%
  select(pidp, wave, remove)

#An example of a respondent who went back to school
test <- eduissue %>%
  filter(pidp == 68155063) %>%
  select(pidp, wave, remove)

##After further examination, my school ending dates seem to be wrong
#I extracted the school and further education ending dates from the xwave file (Script 1)
eduend <- xwave %>%
  select(pidp, hhorig, sex, birthm, birthy, scend_dv, feend_dv) %>%
  filter(hhorig == 1 | hhorig == 2 | hhorig == 7) %>%
  mutate(schend = ifelse(scend_dv <= 0, NA, scend_dv)) %>%
  mutate(feend = ifelse(feend_dv <= 0, NA, feend_dv)) %>%
  mutate(eduend = ifelse(is.na(feend), schend, feend)) %>%
  select(pidp, eduend, scend_dv)

#The way of measuring the end of education used in script 5 appears to be deficent
#End of education (school and further education) was extracted from the xwave file and then used to recaluate school leaving age
fixedu <- surv3.1 %>%
  left_join(., eduissue, by = c("pidp", "wave")) %>%
  mutate(remove2 = ifelse(remove == 1, 1, NA)) %>%
  group_by(pidp) %>%
  fill(remove2, .direction = "down") %>%
  mutate(remove2 = ifelse(is.na(remove2), 0, remove2)) %>%
  ungroup() %>%
  left_join(., eduend, by = "pidp") %>%
  filter(remove != 1) %>%
  group_by(pidp) %>%
  mutate(rownum = row_number()) %>%
  ungroup() %>%
  mutate(age2 = round(age)) %>%
  mutate(eduend = ifelse(rownum == 1 & is.na(eduend), age2, eduend)) %>%
  group_by(pidp) %>%
  fill(eduend, .direction = "down") %>%
  select(-rownum) %>%
  mutate(rownum = row_number()) %>%
  ungroup() %>%
  select(-remove, -remove2, -age2) %>%
  relocate(eduend, .after = "t2") %>%
  mutate(eduendmn = eduend*12) %>%
  mutate(t3 = agemn - eduendmn)

#See the distribution of t3
#There are 1434 negative numbers
fixedu %>%
  ggplot(aes(x = t3)) +
  geom_histogram()

#The negative numbers appear to occur when individuals return to school
#Thus it can be assumed that school leaving age these individuals is a better metric than further education
negobs <- fixedu %>%
  filter(t3 <= -1) %>%
  select(pidp, wave, rownum, age, jbstat, t3, scend_dv, eduendmn) %>%
  mutate(large12 = ifelse(t3 <= -13, 1, 0)) %>% #This helps evaluate how many discrepancies are larger than 12 months
  mutate(schend = ifelse(scend_dv <=0, NA, scend_dv)) %>%
  mutate(schendmn = schend*12) %>%
  select(pidp, wave, schendmn)
  
#789 observations are off by more than 1 year
negobs %>% count(large12)
negobs %>% count(schend)
fixedu %>% count(scend_dv)

surv3 <- fixedu %>%
  left_join(., negobs, by = c("pidp", "wave")) %>%
  group_by(pidp) %>%
  fill(schendmn, .direction = "down") %>%
  ungroup() %>%
  mutate(eduendmn2 = ifelse(!is.na(schendmn), schendmn, eduendmn)) %>%
  # filter(!is.na(schendmn)) %>%
  select(-eduendmn, -t3, -schendmn) %>%
  mutate(t3 = agemn - eduendmn2) %>%
  #Those observations still with negative times are simply a mismatch that cannot be solved and must be excluded
  filter(t3 >= 0) %>%
  #Finally, one last count of number of rows and observations per individual
  select(-rownum) %>%
  group_by(pidp) %>%
  mutate(rownum = row_number()) %>%
  mutate(countobs = length(rownum)) %>%
  ungroup()

surv3 %>% count(eduendmn2)
surv3 %>% count(t3)

#Histogram of the clock
surv3 %>%
  ggplot(aes(x = t3)) +
  geom_histogram()

#Histogram of the number of observations per individual
surv3 %>%
  filter(rownum ==1) %>%
  ggplot(aes(x = countobs)) +
  geom_histogram(binwidth = 1)

  
#Note this resaves "surv3". It is important that the script is run in order!
saveRDS(surv3, "surv3.rds")


#Descriptive statistics of the sample
mycontrols <- tableby.control(test = FALSE)
surv3stats <-arsenal::tableby(sex ~ finnow.imp + finfut.imp + jbsec + edu + combo, data = surv3, control = mycontrols)
labels(surv2stats) <-  c(finnow.imp = "Present Financial Outlook", finfut.imp = "Future Financial Outlook",
                         jbsec = "Job Security", edu = "Educational Attainment", combo = "Partnership, Partner's Job Status")
summary(surv3stats)
# write2word(surv3stats, "surv3stats.doc")
write2html(surv3stats, "surv3stats_25-02-2022.html") #UPDATE DATE



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



############################################################################
# Cleaning DF -------------------------------------------------------------
############################################################################

# Function for later extracting first digit
getFirstDigit <- function(x) {
  floor(x / (10 ^ floor(log10(x))))
}

# Function for later creating income quintiles
quant <- function(inc) {
  breaks <- quantile(inc, probs = seq(0, 1, .2), na.rm = TRUE)
  index <- findInterval(inc, breaks, rightmost.closed = TRUE, all.inside = TRUE)
}

# Household Income script variable extraction, available from script 4
hhinc2 <- hhinc %>%
  select(pidp, wave, oecdeq4) #I selected net hh income - hb adj. & ind/hh deductions, OECD equalized

#remove columns that are not needed in the analysis - keep this as the final variable deciding location
surv4 <- surv3 %>%
  dplyr::select(pidp, wave, rownum, countobs, startdate, hhorig, sex, age, time1, time2, t1, t2, t3, event, kdob, fb, fbyear, anychild_dv, 
                edu, agemn, agesq, marstat, ukborn, immigrant, jbisco88_cc,
                jbstat, employed, permcon, jbhrs, parttime, jbot, priv, jbpl,
                finnow.imp, finfut.imp, cci, jbsec,
                marstat, parjbstat, combo) %>%
  left_join(., hhinc2, by = c("pidp", "wave")) %>%
  group_by(pidp) %>%
  fill(oecdeq4, .direction = "downup") %>%
  ungroup() %>%
  group_by(wave) %>% #Grouping by waves ensures that the quintiles are created inside the same year
  mutate(incquin = quant(oecdeq4),
         incquin = as.character(incquin),
         incquin = recode(incquin,
                          "1" = "Fifth",
                          "2" = "Fourth",
                          "3" = "Third",
                          "4" = "Second",
                          "5" = "First"),
         incquin = fct_relevel(incquin, c("First", 
                                          "Second", 
                                          "Third", 
                                          "Fourth", 
                                          "Fifth"))) %>%
  ungroup() %>%
  mutate(sex = as.character(sex)) %>%
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  mutate(immigrant = as.numeric(immigrant)) %>% #the plot_model function in script 9 is pick about this for some reason
  # Alternative time variable - months since age 16
  mutate(t16 = agemn - 192) %>% 
  relocate("t16", .after = "event") %>%
  #Cleaning the variable "jbsec" to remove coded NA
  mutate(jbsec = ifelse(jbsec <= 0, NA, jbsec)) %>%
  mutate(jbsec = as.character(jbsec)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Very likely",
                      "2" = "Likely",
                      "3" = "Unlikely",
                      "4" = "Very unlikely")) %>% 
  #From the Working Paper; recategorizing the finnow variable from 5 to 3 categories
  mutate(finnow.num = recode(finnow.imp,
                             "1 Living comfortably" = "1",
                             "2 Doing alright" = "2",
                             "3 Just getting by" = "3",
                             "4 Finding it quite difficult" = "4",
                             "5 Finding it very difficult" = "5")) %>% 
  mutate(finnow.num = as.numeric(finnow.num)) %>% 
  # 1 = Living comfortable & Doing alright, 2 = Just getting by, 3 = Fing it quite (very) difficult
  mutate(finnow3cat = ifelse(finnow.num <= 2, 1, ifelse(finnow.num == 3, 2, 3))) %>% 
  mutate(finnow3cat = as.character(finnow3cat)) %>% 
  mutate(finnow3cat = recode(finnow3cat,
                             "1" = "Finding it difficult",
                             "2" = "Getting by",
                             "3" = "Doing fine")) %>% 
  group_by(pidp) %>% #For constant variables I can fill in missing observations
  fill(hhorig, .direction = "updown") %>% 
  fill(ukborn, .direction = "updown") %>%
  fill(immigrant, .direction = "updown") %>%
  ungroup() %>% 
  #Making new categorical variables for employment conditions
  mutate(empstat = ifelse(jbstat == "Unemployed", "unemployed",
                          ifelse(jbstat == "Self-employed", "self-employed",
                                 ifelse(jbstat == "Paid employed", "paid employment", "our of the labour force")))) %>% 
  #Adding in part-time work into the categories
  mutate(pt2 = ifelse(empstat != "paid employment", NA, parttime)) %>% 
  mutate(pt2 = ifelse(is.na(pt2), 3, pt2)) %>% 
  mutate(pt2 = ifelse(pt2 == 1, "part-time",
                      ifelse(pt2 == 3, "NA", "full-time"))) %>%
  group_by(pidp) %>% 
  fill(pt2, .direction = "downup") %>% 
  ungroup() %>% 
  unite(empstat2, empstat, pt2, sep = "-", remove = FALSE) %>% 
  mutate(empstat2 = recode(empstat2,
                             "paid employment-full-time" = "full time",
                             "paid employment-part-time" = "part time",
                             "paid employment-NA" = "full time",
                             "self-employed-NA" = "self-employed",
                             "unemployed-NA" = "unemployment",
                             "our of the labour force-NA" = "out of LF")) %>% 
  mutate(empstat2 = fct_relevel(empstat2, c("full time",
                                            "part time",
                                            "self-employed",
                                            "unemployment",
                                            "out of LF"))) %>% 
  # mutate(empstat2 = ifelse(empstat2 == "paid - NA", "full time", empstat2)) %>% 
  #Making the categories for ISCO88 classifications
  mutate(occlevel = ifelse(jbisco88_cc <= 0, NA, jbisco88_cc)) %>% 
  mutate(occlevel = ifelse(occlevel == 10, NA, occlevel)) %>% #The ISCO88 Code for Armed Forces is 10 - Must be removed not to confuse with the 1 digit code "1"
  mutate(occlevel = getFirstDigit(occlevel)) %>%  #This gives me the 1 digit ISCO88 code
  mutate(occlevel = ifelse(is.na(occlevel) & jbisco88_cc == 10, 10, occlevel)) %>% #Readd the Armed Forces
  mutate(ol5cat = ifelse(occlevel <=3 | occlevel == 10, "high-skilled white-collar",
                         ifelse(occlevel == 4 | occlevel == 5, "low-skilled white-collar",
                                ifelse(occlevel == 6 | occlevel == 7, "high-skilled blue collar",
                                       ifelse(occlevel == 8 | occlevel == 9, "low-skilled blue collar", "no info"))))) %>% 
  group_by(pidp) %>% 
  fill(ol5cat, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(ol5cat = ifelse(is.na(ol5cat), "no info", ol5cat)) %>% 
  mutate(ol5cat = fct_relevel(ol5cat, c( "high-skilled white-collar",
                                         "low-skilled white-collar",
                                         "high-skilled blue collar",
                                         "low-skilled blue collar",
                                         # "armed-forces",
                                         "no info"))) %>%
  mutate(combo = fct_relevel(combo, c("single-unknown", 
                                      "cohab-employed", 
                                      "cohab-non-employed", 
                                      "cohab-unknown", 
                                      "married-employed", 
                                      "married-non-employed", 
                                      "married-unknown"))) %>%
  mutate(finnow3cat = fct_relevel(finnow3cat, c("Doing fine",
                                                "Getting by",
                                                "Finding it difficult"))) %>%
  mutate(finfut.imp = fct_relevel(finfut.imp, c("Better off",
                                              "About the same",
                                              "Worse off"))) %>%
  unite(empfinnow, empstat2, finnow3cat, sep = "-", remove = FALSE) %>% 
  unite(empfinfut, empstat2, finfut.imp, sep = "-", remove = FALSE) %>%
  mutate(worse = ifelse(finfut.imp == "Worse off", "Worse", "Same or better")) %>%
  mutate(better = ifelse(finfut.imp == "Better off", "Better", "Same or worse")) %>%
  unite(empworse, empstat2, worse, sep = "-", remove = FALSE) %>%
  mutate(difficult = ifelse(finnow.num <= 3, "Difficult", "Fine")) %>%
  unite(empdiff, empstat2, difficult, sep = "-", remove = FALSE) %>%
  mutate(difficult = fct_relevel(difficult, c("Fine",
                                                "Difficult"))) %>% 
  mutate(difficultv2 = ifelse(finnow.num <= 2, "Difficult", "Getting by")) %>%
  mutate(difficultv2 = fct_relevel(difficultv2, c("Getting by",
                                              "Difficult"))) %>% 
  unite(sexedu, sex, edu, sep = "-", remove = FALSE) %>% #For the descriptive output
  mutate(eventfct = as.factor(event)) %>% 
  mutate(sexedu = fct_relevel(sexedu, c("Women-low", "Women-medium", "Women-high",
                                        "Men-low", "Men-medium", "Men-high"))) %>%
  #Making new test variables for empstat at t-1 and age groups
  group_by(pidp) %>%
  mutate(empstat_t1 = lag(empstat2, n = 1L, default = NA)) %>% #lagged t-1 empstat2
  ungroup() %>%
  mutate(agedummyU25 = ifelse(age < 25, 1, 0), # 1 = less than 25
         agedummyU30 = ifelse(age < 30, 1, 0), # 1 = less than 30
         agedummyU35 = ifelse(age < 35, 1, 0), # 1 = less than 35
         agedummyU40 = ifelse(age < 40, 1, 0)) %>% # 1 = less than 40
  mutate(edunum = recode(edu, #This is need in the AME (script 10) to create subsamples by education
                         "low" = "1",
                         "medium" = "2",
                         "high" = "3"),
         edunum = as.numeric(edunum),
         edualpha = recode(edu, #This is need in Script 9 for plot_model
                             "low" = "a",
                             "medium" = "b",
                             "high" = "c"),
         marstat = fct_relevel(marstat, c("single",
                                          "cohab",
                                          "married")))

saveRDS(surv4, file = "surv4.rds")

# -------------------------------------------------------------------------
# Descriptive statistics of the sample Section "5. Measures" --------------
# -------------------------------------------------------------------------

mycontrols <- tableby.control(test = FALSE)
surv4stats <-arsenal::tableby(sexedu ~ empstat2 + difficult + better + age + immigrant + ol5cat + combo + incquin, data = surv4, control = mycontrols)
labels(surv4stats) <-  c(sex = "Sex", eventfct = "First Birth", age = "Age",
                         empstat2 = "Activity Status", difficult = "Present Financial Outlook", worse = "Future Financial Outlook",
                         edu = "Educational Attainment", combo = "Partnership, Partner's Job Status", immigrant = "UK Born",
                         ol5cat = "Occupational Class", incquin = "Income Quintiles")
summary(surv4stats)
# write2word(surv4stats, "surv4stats.doc")
write2html(surv4stats, "surv4stats_16-05-2022.html") #UPDATE DATE
write2word(surv4stats, "surv4stats_21-03-2022.docx") #UPDATE DATE


#Stats based on a category of obj. employment conditions
surv4stats2 <-arsenal::tableby(empstat2 ~ sex + event + t3 + t16 + age + jbstat + finnow3cat + finfut.imp + jbsec + parttime + permcon + edu + combo + immigrant + ol5cat, data = surv4, control = mycontrols)
summary(surv4stats2)
write2html(surv4stats2, "surv4stats2_03-03-2022.html") #UPDATE DATE

# ------------------------------------------------------------------------------
### Stats for the section "4. Data and Sample"  and "5. Measures ---------------
# ------------------------------------------------------------------------------


#Count number of individuals
surv4stat <- surv4 %>% 
  filter(rownum == 1)
#Number of individuals by sex
surv4stat %>% count(sex)
#Number of first birth events by sex
surv4 %>% count(event, sex)
#Average number of observations by sex
surv4stat2 <- surv4 %>% 
  group_by(pidp) %>% 
  mutate(numobs = length(rownum)) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  summarise(numobsmean = mean(numobs), numobssd = sd(numobs))
#Counts of subjective measures
surv4 %>% count(difficult)

#Number of events and the number of months post education the occur at. 
surv4stat3 <- surv4 %>% 
  filter(event == 1) %>% 
  group_by(sex) %>% 
  summarise(t3mean = mean(t3), t2sd = sd(t3))

surv4 %>% 
  filter(event == 1) %>% 
  mutate(t3year = t3/12) %>% 
  mutate(edu = recode(edu, "high" = "High", 
                      "medium" = "Medium", 
                      "low" = "Low")) %>% 
  mutate(edu = fct_relevel(edu, c("High", "Medium", "Low"))) %>%
  mutate(t3year = round(t3year)) %>% 
  ggplot(aes(x = t3year, fill = edu)) +
  geom_bar() +
  facet_wrap(~sex) +
  scale_fill_manual(values = c("#49B7FC", "#FF7B00", "#17D898")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Highest Education:") +
  ggtitle("") +
  xlab("Years since end of formal education") +
  ylab("") +
  ggsave("distribution_event_t3_21-03-2022.png", dpi = 300)




