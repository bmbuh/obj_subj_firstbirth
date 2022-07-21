#Coded by: Brian Buh
#Started on: 12.07.2022
#Last Updated: 21.07.2022

### This script changes script 9.1 to group the interactions be education rather than emp/subj. fin

install.packages("rlang")

library(tidyverse)
library(sjPlot) #for using the plot_model function
# library(jtools) #for using effectplots
library(interactions) #for using cat_plot
library(margins)

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")

# Options for creating plots
## effectplots in jtools
## plot_model in sjPlot
## cat_plot in interactions


###########################################################################
# Analysis 1 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 1 empstat2 -----------------------------------------------------
# -------------------------------------------------------------------------

#The terms of interest MUST be interacted or the difference won't be plotted correct
a1m1 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m1))

cat_plot(a1m1, pred = edu, modx = empstat2, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         modx.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         modx.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         mod2.values = c("Women", "Men"),
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Employment Status",
         colors = c("#06080F", "#2F4175", "#4C65BD", "#7A8CCD", "#B5BFE3")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) + 
  coord_flip()
ggsave("a1m1_int_empstat_edu_(mono)S9_21-07-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 1 empstat2 + partnership ---------------------------------------
# -------------------------------------------------------------------------

a1m2 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant + marstat + parjbstat,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m2))

interactions::cat_plot(a1m2, pred = edu, modx = empstat2, mod2 = sex,
                       point.size = 2,
                       line.thickness = 0.8,
                       geom.alpha = 1,
                       dodge.width = 0.4,
                       errorbar.width = 0.25,
                       pred.values = c("low", "medium", "high"),
                       pred.labels = c("Low", "Medium", "High"),
                       modx.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
                       modx.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
                       mod2.values = c("Women", "Men"),
                       mod2.labels = c("Women", "Men"),
                       x.label = "",
                       y.label = "Pr(Conceiving a First Child)",
                       legend.main = "Employment Status",
                       colors = c("#06080F", "#2F4175", "#4C65BD", "#7A8CCD", "#B5BFE3")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) + 
  coord_flip()
ggsave("a1m2_int_empstat_edu_partner(mono)S9_21-07-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 1 difficult ----------------------------------------------------
# -------------------------------------------------------------------------

a1m1diff <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m1diff))

cat_plot(a1m1diff, pred = edu, modx = difficult, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("Difficult", "Fine"),
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Is your present financial situation:",
         colors = c("#2F4175", "#4C65BD")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Perceived Present Financial Security") +
  coord_flip()
ggsave("a1m1diff_int_difficult_edu_(mono)S9_21-07-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 1 difficult + partnership --------------------------------------
# -------------------------------------------------------------------------

a1m2diff <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant + marstat + parjbstat,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m2diff))

cat_plot(a1m2diff, pred = edu, modx = difficult, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("Difficult", "Fine"),
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Is your present financial situation:",
         colors = c("#2F4175", "#4C65BD")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Perceived Present Financial Security") +
  coord_flip()
ggsave("a1m2diff_int_difficult_edu_partner(mono)S9_21-07-2022.png", dpi = 300)

###########################################################################
# Analysis 2 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 2 better --------------------------------------------------------
# -------------------------------------------------------------------------

a2m1better <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*better*edu + agemn + agesq + ol5cat + cci + immigrant,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a2m1))

cat_plot(a2m1better, pred = edu, modx = better, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("Same or worse", "Better"),
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Will your future financial situation be:",
         colors = c("#2F4175", "#4C65BD")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Perceived Future Financial Security") +
  coord_flip()
ggsave("a2m2_int_better_edu_(mono)S9_30-06-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 2 better + partnership ------------------------------------------
# -------------------------------------------------------------------------

a2m2better <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*better*edu + agemn + agesq + ol5cat + cci + immigrant + marstat + parjbstat,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a2m2better))

cat_plot(a2m2better, pred = edu, modx = better, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("Same or worse", "Better"),
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Will your future financial situation be:",
         colors = c("#2F4175", "#4C65BD")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Perceived Future Financial Security") +
  coord_flip()
ggsave("a2m2better_int_better_edu_partner(mono)S9_30-06-2022.png", dpi = 300)  


###########################################################################
# Partnership -------------------------------------------------------------
###########################################################################

# In this case I need to separate men and women in order to reduce the number of interactions


a1m3par <- glm(formula = event ~ t3 + empstat2*sex*edu*parjbstat + difficult + agemn + agesq + ol5cat + cci + immigrant + marstat,
            family = binomial(link = "logit"),
            data = surv4)

a1m3ego <- glm(formula = event ~ t3 + empstat2*sex*edu*parjbstat + difficult + agemn + agesq + ol5cat + cci + immigrant + marstat,
            family = binomial(link = "logit"),
            data = surv4)
#Separated by sex
##Wome
a1m3egof <- glm(formula = event ~ t3 + empstat2*edu*parjbstat + difficult + agemn + agesq + ol5cat + cci + immigrant + marstat,
               family = binomial(link = "logit"),
               data = surv4f)

##Men
a1m3egom <- glm(formula = event ~ t3 + empstat2*edu*parjbstat + difficult + agemn + agesq + ol5cat + cci + immigrant + marstat,
               family = binomial(link = "logit"),
               data = surv4m)

# #Men
# a1m3 <- glm(formula = event ~ t3 + empstat2*edu*parjbstat + difficult + agemn + agesq + ol5cat + cci + immigrant + marstat,
#             family = binomial(link = "logit"),
#             data = surv4m)
# 
# a2m3 <- glm(formula = event ~ t3 + empstat2*edu*parjbstat + better + agemn + agesq + ol5cat + cci + immigrant + marstat,
#             family = binomial(link = "logit"),
#             data = surv4m)

#Partnership Analysis partner's employment
cat_plot(a1m3par, pred = edu, modx = parjbstat, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         # int.width = 0.01,
         errorbar.width = 0.25,
         modx.values = c("unknown", "non-employed", "employed"),
         modx.labels = c("Unknown", "Non-employed", "Employed"),
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Partner's Employment Status",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partners' Employment Status by Sex and Educational Attainment") +
  coord_flip()
ggsave("a1m3par_int_partnership_diff(mono)S9_21-07-2022.png", dpi = 300)  

#Partnership Analysis ego's employment
cat_plot(a1m3egof, pred = edu, modx = empstat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         # int.width = 0.01,
         errorbar.width = 0.25,
         modx.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         modx.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Partner's Employment Status",
         colors = c("#06080F", "#2F4175", "#4C65BD", "#7A8CCD", "#B5BFE3")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Women's Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a1m3egof_int_partnership_diff(mono)S9_21-07-2022.png", dpi = 300)

#Note if you compare this to the not interacted empstat2 we see that it increases the error bars but DOESN'T change the relationship


#Partnership Analysis ego's employment WOMEN
cat_plot(a1m3egom, pred = edu, modx = empstat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         int.width = 0.01,
         errorbar.width = 0.25,
         modx.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         modx.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         pred.values = c("low", "medium", "high"),
         pred.labels = c("Low", "Medium", "High"),
         # mod2.values = c("Women", "Men"),
         # mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Partner's Employment Status",
         colors = c("#06080F", "#2F4175", "#4C65BD", "#7A8CCD", "#B5BFE3")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a1m3egom_int_partnership_diff(mono)S9_21-07-2022.png", dpi = 300)


#Note if you compare this to the not interacted empstat2 we see that it increases the error bars but DOESN'T change the relationship.
#Unlike women there are some changes, but the error bars get so large for those variables that do change, we can rule out any significant change


###########################################################################
# Income Quintile ---------------------------------------------------------
###########################################################################

#Note: I change the model names as the interactions change the emphasis
# 
# a3m5 <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + ol5cat + cci + immigrant + incquin*sex*edu,
#             family = binomial(link = "logit"),
#             data = surv4)
# 
# a3m6 <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + ol5cat + cci + immigrant + incquin*sex*edu,
#             family = binomial(link = "logit"),
#             data = surv4)
# 
# a3m7 <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + ol5cat + cci + immigrant + combo + incquin*sex*edu,
#             family = binomial(link = "logit"),
#             data = surv4)
# 
# a3m8 <- glm(formula = event ~ t3 + empstat2 + better + agemn + agesq + ol5cat + cci + immigrant + combo + incquin*sex*edu,
#             family = binomial(link = "logit"),
#             data = surv4)
# 
# 
# #Partnership Analysis 1
# cat_plot(a3m5, pred = incquin, modx = edu, mod2 = sex,
#          point.size = 2,
#          line.thickness = 0.8,
#          geom.alpha = 1,
#          dodge.width = 0.4,
#          errorbar.width = 0.25,
#          pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
#          modx.values = c("low", "medium", "high"),
#          modx.labels = c("Low", "Medium", "High"),
#          mod2.values = c("Women", "Men"),          
#          mod2.labels = c("Women", "Men"),
#          x.label = "",
#          y.label = "Pr(Conceiving a First Child)",
#          legend.main = "Education",
#          colors = c("#13213F", "#345AAD", "#819DD9")) +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
#         axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
#         legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
#   labs(title = "Partnership Status, Partners' Employment Status") +
#   coord_flip()
# ggsave("a3m5_int_incquin_diff(mono)S9_12-07-2022.png", dpi = 300)  
# 
# 
# #Partnership Analysis 2
# cat_plot(a3m6, pred = incquin, modx = edu, mod2 = sex,
#          point.size = 2,
#          line.thickness = 0.8,
#          geom.alpha = 1,
#          dodge.width = 0.4,
#          errorbar.width = 0.25,
#          pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
#          modx.values = c("low", "medium", "high"),
#          modx.labels = c("Low", "Medium", "High"),
#          mod2.values = c("Women", "Men"),          
#          mod2.labels = c("Women", "Men"),
#          x.label = "",
#          y.label = "Pr(Conceiving a First Child)",
#          legend.main = "Education",
#          colors = c("#13213F", "#345AAD", "#819DD9")) +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
#         axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
#         legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
#   labs(title = "Partnership Status, Partners' Employment Status") +
#   coord_flip()
# ggsave("a3m6_int_incquin_better(mono)S9_12-07-2022.png", dpi = 300)  
# 
# 
# ##There are only very very slight differences between the predicted probabilities in Analysis 1 & 2
# ##The biggest differences are in the error bars
# 
# #Partnership Analysis 3 - Difficult
# cat_plot(a3m7, pred = incquin, modx = edu, mod2 = sex,
#          point.size = 2,
#          line.thickness = 0.8,
#          geom.alpha = 1,
#          dodge.width = 0.4,
#          errorbar.width = 0.25,
#          pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
#          modx.values = c("low", "medium", "high"),
#          modx.labels = c("Low", "Medium", "High"),
#          mod2.values = c("Women", "Men"),          
#          mod2.labels = c("Women", "Men"),
#          x.label = "",
#          y.label = "Pr(Conceiving a First Child)",
#          legend.main = "Education",
#          colors = c("#13213F", "#345AAD", "#819DD9")) +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
#         axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
#         legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
#   labs(title = "Partnership Status, Partners' Employment Status") +
#   coord_flip()
# ggsave("a3m7_int_incquin_diffpart(mono)S9_12-07-2022.png", dpi = 300)  
# 
# 
# #Partnership Analysis 3 - better
# cat_plot(a3m8, pred = incquin, modx = edu, mod2 = sex,
#          point.size = 2,
#          line.thickness = 0.8,
#          geom.alpha = 1,
#          dodge.width = 0.4,
#          errorbar.width = 0.25,
#          pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
#          modx.values = c("low", "medium", "high"),
#          modx.labels = c("Low", "Medium", "High"),
#          mod2.values = c("Women", "Men"),
#          mod2.labels = c("Women", "Men"),
#          x.label = "",
#          y.label = "Pr(Conceiving a First Child)",
#          legend.main = "Education",
#          colors = c("#13213F", "#345AAD", "#819DD9")) +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
#         axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
#         legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
#   labs(title = "Partnership Status, Partners' Employment Status") +
#   coord_flip()
# ggsave("a3m8_int_incquin_betterpart(mono)S9_12-07-2022.png", dpi = 300) 

##Surprisingly, adding partnership does not change the relationship of the variable, but lowers the Predicted Probabilities





# TEST
  # -------------------------------------------------------------------------
  # Analysis 2 better --------------------------------------------------------
  # -------------------------------------------------------------------------
  
  a2m1bettertest <- glm(formula = event ~ t3 + empstat2*sex*edu*better + agemn + agesq + ol5cat + cci + immigrant,
                    family = binomial(link = "logit"),
                    data = surv4)
  summary(a2m1bettertest)
  summary(margins(a2m1bettertest))
  
  cat_plot(a2m1bettertest, pred = edu, modx = better, mod2 = sex,
           point.size = 2,
           line.thickness = 0.8,
           geom.alpha = 1,
           dodge.width = 0.4,
           errorbar.width = 0.25,
           modx.values = c("Same or worse", "Better"),
           pred.values = c("low", "medium", "high"),
           pred.labels = c("Low", "Medium", "High"),
           mod2.values = c("Women", "Men"),          
           mod2.labels = c("Women", "Men"),
           x.label = "",
           y.label = "Pr(Conceiving a First Child)",
           legend.main = "Will your future financial situation be:",
           colors = c("#2F4175", "#4C65BD")) +
    theme_bw() +
    theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
          axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
          legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
    labs(title = "Perceived Future Financial Security") +
    coord_flip()
  ggsave("a2m2_int_better_edu_(mono)S9_30-06-2022.png", dpi = 300)
