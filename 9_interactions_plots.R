#Coded by: Brian Buh
#Started on: 09.03.2022
#Last Updated: 12.05.2022

# library(data.table)
# library(padr)
library(tidyverse)
# library(haven)
# library(lubridate)
# library(arsenal)
# library(zoo)
# library(survival)
# library(survminer)
# library(survPen)
# library(flexsurv)
# library(coxme)
# library(stargazer)
# library(texreg)
# library(forestplot)
library(sjPlot) #for using the plot_model function
# library(janitor)
# library(lme4)
# library(survey)
# library(jtools)
# library(ggstance)
# library(broom.mixed)
# library(effects)
# library(interactions)

#Load data surv4
surv4 <- file.choose()
surv4 <- readRDS(surv4)

surv4m <- surv4 %>% filter(sex == "Men")
surv4f <- surv4 %>% filter(sex == "Women")

###########################################################################
# Predicted Probabilities -------------------------------------------------
###########################################################################

#The main text of the analysis will use Predicted Probability plots to show how the explanatory variables influence first birth timing

# -------------------------------------------------------------------------
# Analysis 1 --------------------------------------------------------------
# -------------------------------------------------------------------------

###Analysis 1 Model 2 finnow

effect_plot(a1m2m, pred = difficult, interval = TRUE)
effect_plot(a1m2f, pred = difficult, interval = TRUE)

#The interaction plots require all variables in one model
a1m2 <- glm(formula = event ~ t3 + empstat2 + sex*difficult + agemn + agesq + immigrant + edu + ol5cat + cci,
            family = binomial(link = "logit"),
            data = surv4)

#The cat_plot function allows for predicted interactions. Thus, we can see the effect of sex plotted together.
cat_plot(a1m2, pred = difficult, modx = sex,
         pred.labels = c("Fine", "Difficult"))


# -------------------------------------------------------------------------
### Analysis 1 Model 3 difficult*edu

effect_plot(a1m3m, pred = difficult, interval = TRUE)
effect_plot(a1m3f, pred = difficult, interval = TRUE)


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
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  ggsave("paper1.1_interaction_difficult_08-03-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Analysis 2 --------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#Analysis 2 Model 2 finnow
effect_plot(a2m2m, pred = worse, interval = TRUE)
effect_plot(a2m2f, pred = worse, interval = TRUE)


a2m2 <- glm(formula = event ~ t3 + sex + empstat2 + worse + agemn + agesq + immigrant + edu + ol5cat + cci,
            family = binomial(link = "logit"),
            data = surv4)
cat_plot(a2m2, pred = worse, modx = sex)


# -------------------------------------------------------------------------
#Analysis 2 Model 3 worse*edu
effect_plot(a2m3m, pred = worse, interval = TRUE)
effect_plot(a2m3f, pred = worse, interval = TRUE)

a2m3 <- glm(formula = event ~ t3 +  empstat2 + sex*worse*edu + agemn + agesq + immigrant + ol5cat + cci,
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
  ggsave("paper1.1_interaction_worse_08-03-2022.png", dpi = 300)

# -------------------------------------------------------------------------
#Analysis 2 Model 4 worse*edu + partnership
effect_plot(a2m4m, pred = worse, interval = TRUE)
effect_plot(a2m4f, pred = worse, interval = TRUE)

a2m4 <- glm(formula = event ~ t3 + empstat2 + sex*worse*edu + agemn + agesq + immigrant + ol5cat + cci + combo,
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
  ggsave("paper1.1_interaction_worse_partner_08-03-2022.png", dpi = 300)



###########################################################################
# Testing 12.05.2022 ------------------------------------------------------
###########################################################################

#! There is the simple issue that "plot_model" was not built to recognize factor level
# To solve this, I created a education variable called "edualpha" where high = c, medium = b, low = a (script 6)

#The terms of interest MUST be interacted or the difference won't be plotted correct
a1m2 <- glm(formula = event ~ t3 + empstat2*sex*edu + sex*difficult*edu + agemn + agesq + ol5cat + cci + immigrant,
            family = binomial(link = "logit"),
            data = surv4)
summary(margins(a1m2))

cat_plot(a1m2, pred = empstat2, modx = edu, mod2 = sex,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.25,
         errorbar.width = 0.25,
         modx.values = c("high", "medium", "low"),
         pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         # pred.labels = c("Same or better", "Worse"),
         modx.labels = c("High", "Medium", "Low"),
         mod2.labels = c("Men", "Women"),
         x.label = "",
         y.label = "Pr(Conceiving a First Child)",
         legend.main = "Education") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) + 
  coord_flip()
# set_theme()
plot_model(a1m2, type = "pred", terms = c("empstat2", "edu", "sex"),
           group.terms = c(2,3,1))


# +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  scale_x_discrete(color = c("high", "medium", "low"))

# +
  guides(color = guide_legend(c("high", "medium", "low")))
  # scale_color_discrete(labels = c("High", "Medium", "Low"))
  
#The cat_plot function allows for predicted interactions. Thus, we can see the effect of sex plotted together.
# cat_plot(a1m2, pred = empstat2, modx = edu, mod2 = "sex") # I don't love the catplot output...
plot_model(a1m2, type = "pred", terms = c("difficult", "edu", "sex"))

a1m3 <- glm(formula = event ~ t3 + empstat2*sex*edu + difficult*sex*edu + agemn + agesq + ol5cat + cci + combo*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)
plot_model(a1m3, type = "pred", terms = c("empstat2", "edu", "sex"))
plot_model(a1m3, type = "pred", terms = c("difficult", "edu", "sex"))
plot_model(a1m3, type = "pred", terms = c("combo", "edu", "sex"))


a2m2 <- glm(formula = event ~ t3 + empstat2*sex*edu + worse*sex*edu + agemn + agesq + ol5cat + cci,
            family = binomial(link = "logit"),
            data = surv4)
plot_model(a2m2, type = "pred", terms = c("empstat2", "edu", "sex"))
plot_model(a2m2, type = "pred", terms = c("worse", "edu", "sex"))

a2m3 <- glm(formula = event ~ t3 + empstat2*sex*edu + worse*sex*edu + agemn + agesq + ol5cat + cci + combo*sex*edu,
            family = binomial(link = "logit"),
            data = surv4)
plot_model(a2m3, type = "pred", terms = c("empstat2", "edu", "sex"))
plot_model(a2m3, type = "pred", terms = c("worse", "edu", "sex"))
plot_model(a2m3, type = "pred", terms = c("combo", "edu", "sex"))



