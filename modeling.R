library(tidyverse)
library(texreg)
library(estimatr)
library(AER)


load(here::here("data", "cps_clean.RData"))

model_vars <- cps_supplement %>% 
  dplyr::select(PEINHOME, PRTAGE, PTDTRACE, PEHSPNON, GEDIV, PEEDUCA,
         PREXPLF, PRDISFLG, PWSSWGT)

#releveling education variable
model_vars$PEEDUCA <- fct_other(model_vars$PEEDUCA, drop = c("Less than 1st grade", "1st - 3rd grade", "5th - 6th grade",
                                                             "7th - 8th grade", "9th grade", "10th grade", "11th grade",
                                                             "12th grade no diploma"), other_level = "Less than High School")
model_vars$PEEDUCA <- fct_other(model_vars$PEEDUCA, drop = c("Associate degree (academic)", "Associate degree (vocational)"),
                                other_level = "Associate's degree")

model_vars$PEEDUCA <- fct_relevel(model_vars$PEEDUCA, levels = c("Less than High School", "High school or GED", "Some college (no degree)",
                                                                 "Associate's degree", "Bachelor's degree", "Master's degree",
                                                                 "Professional degree", "Doctorate degree"))

#base mod (linear probability)
lpm <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + GEDIV + PEEDUCA + PREXPLF + PRDISFLG, data = model_vars, weights = PWSSWGT)

#nice table
screenreg(lpm, custom.model.names = "Base Model",
          custom.coef.names = c("(Intercept)", "Age", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander", "Other",
                                "Non-hispanic", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic",
                                "East South Central", "West South Central", "Mountain", "Pacific", "Highschool/GED", "Some College",
                                "Associate's", "Bachelor's", "Master's", "Professional", "Doctorate", "Unemployed", "No disability"),
          digits = 4, stars = c(.01, .05, .1),
          include.fstatistic = TRUE,
          custom.note = "%stars. \nF-statistic: 109.1 on 24 and 58429 DF, p-value: < 2.22e-16")

#nice table but latex
texreg(lpm, custom.model.names = "Base Model",
       custom.coef.names = c("(Intercept)", "Age", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander", "Other",
                                "Non-hispanic", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic",
                                "East South Central", "West South Central", "Mountain", "Pacific", "Highschool/GED", "Some College",
                                "Associate's", "Bachelor's", "Master's", "Professional", "Doctorate", "Unemployed", "No disability"),
       digits = 4, stars = c(.01, .05, .1),
       custom.note = "%stars.\\F-statistic: 109.1 on 24 and 58429 DF, p-value: < 2.22e-16",
       include.fstatistic = TRUE,
       label = NULL,
       caption = NULL,
       file = "output/basereg.tex")

#is GEDIV significant overall
lpm_nogeo <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, data = model_vars, weights = PWSSWGT)
anova(lpm, lpm_nogeo) #GEDIV is significant

#is race significant
lpm_norace <- lm(PEINHOME ~ PRTAGE + PEHSPNON + GEDIV + PEEDUCA + PREXPLF + PRDISFLG, data = model_vars, weights = PWSSWGT)
anova(lpm, lpm_norace) #race is significant

anova(lpm)
#TODO
#make age 18+?
#do something about model objectively awful r^2, f-statistic is significant though -_-
#add interaction term for GEDIV