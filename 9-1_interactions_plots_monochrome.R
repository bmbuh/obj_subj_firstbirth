#Coded by: Brian Buh
#Started on: 18.05.2022
#Last Updated: 

### This script mirrors Script "9_interaction_plots" with the only change being the color of the output

library(tidyverse)
library(sjPlot) #for using the plot_model function
# library(jtools) #for using effectplots
library(interactions) #for using cat_plot

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

cat_plot(a1m1, pred = empstat2, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("low", "medium", "high"),
         pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) + 
  coord_flip()
ggsave("a1m1_int_empstat_edu_(mono)S9_16-05-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 1 empstat2 + partnership ---------------------------------------
# -------------------------------------------------------------------------

a1m3 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant + combo,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m1))

cat_plot(a1m3, pred = empstat2, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("low", "medium", "high"),
         pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("Out of the LF", "Unemployment",  "Self-employed", "Part-time", "Full-time"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) + 
  coord_flip()
ggsave("a1m1_int_empstat_edu_partner(mono)S9_16-05-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 1 difficult ----------------------------------------------------
# -------------------------------------------------------------------------

a1m2 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m2))

cat_plot(a1m2, pred = difficult, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Difficult", "Fine"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Present Financial Security") +
  coord_flip()
ggsave("a1m2_int_difficult_edu_(mono)S9_16-05-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 1 difficult + partnership --------------------------------------
# -------------------------------------------------------------------------

a1m3 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant + combo,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m1))

cat_plot(a1m3, pred = difficult, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Difficult", "Fine"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Present Financial Security") + 
  coord_flip()
ggsave("a1m3_int_difficult_edu_partner(mono)S9_16-05-2022.png", dpi = 300)

###########################################################################
# Analysis 2 --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Analysis 2 worse --------------------------------------------------------
# -------------------------------------------------------------------------

a2m2 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*worse*edu + agemn + agesq + ol5cat + cci + immigrant,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m2))

cat_plot(a2m2, pred = worse, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Worse", "Same or better"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Future Financial Security") + 
  coord_flip()
ggsave("a2m2_int_worse_edu_(mono)S9_16-05-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 2 worse + partnership ------------------------------------------
# -------------------------------------------------------------------------

a2m3 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*worse*edu + agemn + agesq + ol5cat + cci + immigrant + combo,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a2m3))

cat_plot(a2m3, pred = worse, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Worse", "Same or better"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Future Financial Security") + 
  coord_flip()
ggsave("a2m3_int_worse_edu_partner(mono)S9_16-05-2022.png", dpi = 300)  


###########################################################################
# Partnership -------------------------------------------------------------
###########################################################################

#I will look at if it changes between Analysis 1, 2, & 3
### The plots show that neither present vs future financial security nor income influence the relationship of partnership to first birth

a1m4 <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + ol5cat + cci + immigrant + combo*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)

a2m4 <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + ol5cat + cci + immigrant + combo*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)

a3m2 <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + ol5cat + cci + immigrant + combo*sex*edu + incquin,
            family = binomial(link = "logit"),
            data = surv4)

a3m4 <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + ol5cat + cci + immigrant + combo*sex*edu + incquin,
            family = binomial(link = "logit"),
            data = surv4)

#Partnership Analysis 1
cat_plot(a1m4, pred = combo, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("single-unknown", "cohab-unknown", "cohab-non-employed", "cohab-employed", "married-unknown", "married-non-employed", "married-employed"),
         pred.labels = c("Single", "Cohab - Unknown", "Cohab - Non-employed", "Cohab - Employed", "Married - Unknown", "Married - Non-employed", "Married - Employed"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a1m4_int_partnership_diff(mono)S9_16-05-2022.png", dpi = 300)  


#Partnership Analysis 2
cat_plot(a2m4, pred = combo, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("single-unknown", "cohab-unknown", "cohab-non-employed", "cohab-employed", "married-unknown", "married-non-employed", "married-employed"),
         pred.labels = c("Single", "Cohab - Unknown", "Cohab - Non-employed", "Cohab - Employed", "Married - Unknown", "Married - Non-employed", "Married - Employed"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a2m4_int_partnership_worse(mono)S9_16-05-2022.png", dpi = 300)  


##There are only very very slight differences between the predicted probabilities in Analysis 1 & 2
##The biggest differences are in the error bars

#Partnership Analysis 3 - Difficult
cat_plot(a3m2, pred = combo, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("single-unknown", "cohab-unknown", "cohab-non-employed", "cohab-employed", "married-unknown", "married-non-employed", "married-employed"),
         pred.labels = c("Single", "Cohab - Unknown", "Cohab - Non-employed", "Cohab - Employed", "Married - Unknown", "Married - Non-employed", "Married - Employed"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a3m2_int_partnership_diffinc(mono)S9_16-05-2022.png", dpi = 300)  


#Partnership Analysis 3 - Worse
cat_plot(a3m4, pred = combo, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("single-unknown", "cohab-unknown", "cohab-non-employed", "cohab-employed", "married-unknown", "married-non-employed", "married-employed"),
         pred.labels = c("Single", "Cohab - Unknown", "Cohab - Non-employed", "Cohab - Employed", "Married - Unknown", "Married - Non-employed", "Married - Employed"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a3m4_int_partnership_worseinc(mono)S9_16-05-2022.png", dpi = 300)  

##Surprisingly, adding income really does very very little to change the predicted probabilities
##The biggest differences are in the error bars


###########################################################################
# Income Quintile ---------------------------------------------------------
###########################################################################

#Note: I change the model names as the interactions change the emphasis

a3m5 <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + ol5cat + cci + immigrant + incquin*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)

a3m6 <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + ol5cat + cci + immigrant + incquin*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)

a3m7 <- glm(formula = event ~ t3 + empstat2 + difficult + agemn + agesq + ol5cat + cci + immigrant + combo + incquin*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)

a3m8 <- glm(formula = event ~ t3 + empstat2 + worse + agemn + agesq + ol5cat + cci + immigrant + combo + incquin*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)


#Partnership Analysis 1
cat_plot(a3m5, pred = incquin, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a3m5_int_incquin_diff(mono)S9_16-05-2022.png", dpi = 300)  


#Partnership Analysis 2
cat_plot(a3m6, pred = incquin, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a3m6_int_incquin_worse(mono)S9_16-05-2022.png", dpi = 300)  


##There are only very very slight differences between the predicted probabilities in Analysis 1 & 2
##The biggest differences are in the error bars

#Partnership Analysis 3 - Difficult
cat_plot(a3m7, pred = incquin, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),          
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a3m7_int_incquin_diffpart(mono)S9_16-05-2022.png", dpi = 300)  


#Partnership Analysis 3 - Worse
cat_plot(a3m8, pred = incquin, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("Fifth", "Fourth", "Third", "Second", "First"),
         modx.values = c("low", "medium", "high"),
         modx.labels = c("Low", "Medium", "High"),
         mod2.values = c("Women", "Men"),
         mod2.labels = c("Women", "Men"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education",
         colors = c("#13213F", "#345AAD", "#819DD9")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15), plot.title = element_text(size = 15)) +
  labs(title = "Partnership Status, Partners' Employment Status") +
  coord_flip()
ggsave("a3m8_int_incquin_worsepart(mono)S9_16-05-2022.png", dpi = 300) 

##Surprisingly, adding partnership does not change the relationship of the variable, but lowers the Predicted Probabilities


