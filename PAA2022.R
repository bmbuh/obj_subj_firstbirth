#Coded by: Brian Buh
#Started on: 17.03.2022
#Last Updated: 


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


###########################################################################
# Descriptives ------------------------------------------------------------
###########################################################################

mycontrols <- tableby.control(test = FALSE)
surv4stats <-arsenal::tableby(sex ~ eventfct + age + empstat2 + difficult + worse + edu + combo + immigrant + ol5cat, data = surv4, control = mycontrols)
labels(surv4stats) <-  c(sex = "Sex", event = "First Birth", age = "Age",
                         empstat2 = "Activity Status", difficult = "Present Financial Outlook", worse = "Future Financial Outlook",
                         edu = "Educational Attainment", combo = "Partnership, Partner's Job Status", immigrant = "UK Born", ol5cat = "Occupational Class")
summary(surv4stats)
# write2word(surv4stats, "surv4stats.doc")
write2html(surv4stats, "paastats_21-03-2022.html") #UPDATE DATE


###
# Plot - Age and Employment Status -----------------------------------------

surv4 %>% 
  mutate(empstat2 = recode(empstat2, "full time" = "Full-time", 
                            "part time" = "Part-time", 
                            # "paid - NA" = "Employed - NA", 
                            "self-employed" = "Self-employed", 
                            "unemployment" = "Unemployed", 
                            "out of LF" = "Out of LF")) %>% 
  ggplot(aes(x = age, fill = empstat2)) +
  geom_histogram(binwidth = 1, color = "black", size = 1) +
  scale_x_continuous(breaks = c(16, 20, 25, 30, 35, 40, 45, 50)) +
  facet_wrap(~sex) +
  # annotate("text", x=.15, y=5000, size = 6, label= "First Births are concentrated here") +
  # annotate("text", x=.25, y=2000, size = 6, label= "0 = No Jobless Spells") +
  # annotate("text", x=.8, y=1200, size = 6, label= "1 = Completely Jobless") +
  scale_fill_manual(values = c("#DF5900", "#FB8500", "#FFB703", "#023047", "#219EBC", "#8ECAE6")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Activity Status") +
  # scale_fill_manual(labels = c("Full-time", "Part-time", "Employed - NA", "Self-employed", "Unemployed", "Out of LF"),
  #                   values = c("full time", "part time", "paid - NA", "self-employed", "unemployed", "out of LF")) +
  ggtitle("") +
  xlab("Age") +
  ylab("Count") +
  ggsave("paa_empstat_age_21-03-2022.png", dpi = 300)

###
# Plot - Subjective measures and education -----------------------------------------

###Present
surv4 %>% 
  mutate(edu = recode(edu, "high" = "High", 
                           "medium" = "Medium", 
                           "low" = "Low")) %>% 
  mutate(edu = fct_relevel(edu, c("Low", "Medium", "High"))) %>% 
  mutate(age = round(age)) %>% 
  ggplot() +
  geom_bar(aes(x = age, fill = difficult), position = "fill", width = .85) +
  scale_x_continuous(breaks = c(16, 20, 25, 30, 35, 40, 45, 50)) +
  facet_wrap(~edu) +
  scale_fill_manual(values = c("#9C82F5", "#6956A7")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Present Financial Situation") +
  ggtitle("") +
  xlab("Age") +
  ylab("Count") +
  ggsave("paa_difficult_age_21-03-2022.png", dpi = 300)

###Future
surv4 %>% 
  mutate(edu = recode(edu, "high" = "High", 
                      "medium" = "Medium", 
                      "low" = "Low")) %>% 
  mutate(edu = fct_relevel(edu, c("Low", "Medium", "High"))) %>% 
  mutate(age = round(age)) %>% 
  ggplot() +
  geom_bar(aes(x = age, fill = worse), position = "fill", width = .85) +
  scale_x_continuous(breaks = c(16, 20, 25, 30, 35, 40, 45, 50)) +
  facet_wrap(~edu) +
  scale_fill_manual(values = c("#079EE9", "#013C5A")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Present Financial Situation") +
  ggtitle("") +
  xlab("Age") +
  ylab("Count") +
  ggsave("paa_worse_age_21-03-2022.png", dpi = 300)



###########################################################################
# Predicted Probability Plots ---------------------------------------------
###########################################################################
surv4 <- surv4 %>% 
  mutate(event = as.numeric(event))



# -------------------------------------------------------------------------
### Analysis 1 Model 3 difficult*edu

a1m3 <- glm(formula = event ~ t3 + empstat2 + difficult*edu*sex + agemn + agesq + immigrant + ol5cat + cci,
            family = binomial(link = "logit"),
            data = surv4)

#The cat_plot function allows for predicted interactions. Thus, we can see the effect of sex plotted together.
cat_plot(a1m3, pred = difficult, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.25,
         errorbar.width = 0.25,
         modx.values = c("high", "medium", "low"),
         pred.labels = c("Fine", "Difficult"),
         modx.labels = c("High", "Medium", "Low"),
         mod2.labels = c("Men", "Women"),
         x.label = "version 1",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  ggsave("paa_interaction_difficult_21-03-2022.png", dpi = 300)

# -------------------------------------------------------------------------
### Analysis 1 Model 3 difficult*edu version 2
# 
# a1m3v2 <- glm(formula = event ~ t3 + empstat2 + difficultv2*edu*sex + agemn + agesq + immigrant + ol5cat + cci,
#             family = binomial(link = "logit"),
#             data = surv4)
# 
# #The cat_plot function allows for predicted interactions. Thus, we can see the effect of sex plotted together.
# cat_plot(a1m3v2, pred = difficultv2, modx = edu, mod2 = sex,
#          point.size = 2,
#          line.thickness = 0.8,
#          geom.alpha = 1,
#          dodge.width = 0.25,
#          errorbar.width = 0.25,
#          modx.values = c("high", "medium", "low"),
#          pred.labels = c("Fine", "Difficult"),
#          modx.labels = c("High", "Medium", "Low"),
#          mod2.labels = c("Men", "Women"),
#          x.label = "version 2",
#          y.label = "Pr(Conceiving a First Child)",
#          legend.main = "Education") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
#         axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
#         legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) 


# -------------------------------------------------------------------------
### Analysis 1 Model 4 difficult*edu + combo

a1m4 <- glm(formula = event ~ t3 + empstat2 + difficult*edu*sex + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
            family = binomial(link = "logit"),
            data = surv4)

#The cat_plot function allows for predicted interactions. Thus, we can see the effect of sex plotted together.
cat_plot(a1m4, pred = difficult, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.25,
         errorbar.width = 0.25,
         modx.values = c("high", "medium", "low"),
         pred.labels = c("Fine", "Difficult"),
         modx.labels = c("High", "Medium", "Low"),
         mod2.labels = c("Men", "Women"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  ggsave("paa_interaction_difficult_partner_21-03-2022.png", dpi = 300)

# -------------------------------------------------------------------------
#Analysis 2 Model 3 worse*edu

a2m3 <- glm(formula = event ~ t3 + empstat2 + sex*worse*edu + agemn + agesq + immigrant + ol5cat + cci,
            family = binomial(link = "logit"),
            data = surv4)

cat_plot(a2m3, pred = worse, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.25,
         errorbar.width = 0.25,
         modx.values = c("high", "medium", "low"),
         # pred.labels = c("Same or better", "Worse"),
         modx.labels = c("High", "Medium", "Low"),
         mod2.labels = c("Men", "Women"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  ggsave("paa_interaction_worse_21-03-2022.png", dpi = 300)

# -------------------------------------------------------------------------
#Analysis 2 Model 4 worse*edu + partnership

a2m4 <- glm(formula = event ~ t3 + empstat2 + difficult + sex*worse*edu + agemn + agesq + immigrant + ol5cat + cci + combo,
            family = binomial(link = "logit"),
            data = surv4)

cat_plot(a2m4, pred = worse, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.25,
         errorbar.width = 0.25,
         modx.values = c("high", "medium", "low"),
         # pred.labels = c("Same or better", "Worse"),
         modx.labels = c("High", "Medium", "Low"),
         mod2.labels = c("Men", "Women"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  ggsave("paa_interaction_worse_partner_21-03-2022.png", dpi = 300)

# -------------------------------------------------------------------------
#Analysis 3 Model 4 empstat2 + worse*edu + partnership

a3m4 <- glm(formula = event ~ t3 + empstat2*sex*edu + difficult + worse + agemn + agesq + immigrant + ol5cat + cci + combo,
            family = binomial(link = "logit"),
            data = surv4)

cat_plot(a3m4, pred = empstat2, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.25,
         errorbar.width = 0.25,
         pred.labels = c("Full time", "Part time", "Self-Employed", "Unemployed", "Out of LF"),
         modx.labels = c("High", "Medium", "Low"),
         mod2.labels = c("Men", "Women"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), axis.text.x = element_text(angle = 70, hjust = 1)) +
  ggsave("paa_empstat_subj_partner_21-03-2022.png", dpi = 300)


###########################################################################
# AME Appendix ------------------------------------------------------------
###########################################################################


