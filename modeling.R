library(tidyverse)
library(texreg)
library(lmtest)
library(sandwich)

model_vars <- cps_supplement %>% 
  select(PEINHOME, PRTAGE, PTDTRACE, PEHSPNON, GEDIV, PEEDUCA,
         PREXPLF, PRDISFLG)

#releveling education variable
model_vars$PEEDUCA <- fct_other(model_vars$PEEDUCA, drop = c("Less than 1st grade", "1st - 3rd grade", "5th - 6th grade",
                                                             "7th - 8th grade", "9th grade", "10th grade", "11th grade",
                                                             "12th grade no diploma"), other_level = "Less than High School")
model_vars$PEEDUCA <- fct_other(model_vars$PEEDUCA, drop = c("Associate degree (academic)", "Associate degree (vocational)"),
                                other_level = "Associate's degree")

model_vars$PEEDUCA <- fct_relevel(model_vars$PEEDUCA, levels = c("Less than High School", "High school or GED", "Some college (no degree)",
                                                                 "Associate's degree", "Bachelor's degree", "Master's degree",
                                                                 "Professional degree", "Doctorate degree"))

#probit model

probit <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + GEDIV + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = model_vars)

probit_hc <- coeftest(probit, vcov. = vcovHC, type = "HC1")

screenreg(probit_hc, custom.coef.names = c("(Intercept)", "Age", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander",
                                        "Race Other", "Non-Hispanic", "Middle Atlantic", "East North Central", "West North Central",
                                        "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific", "Highschool or GED",
                                        "Some college", "Associate's", "Bachelor's", "Master's", "Professional", "Docterate", "Unemployed", "No Disability"),
          custom.model.names = "Probit Robust SE", digits = 4, stars = c(.01, .05, .1))

#TODO
#post-hoc testing?
#separate models for each region (could do interaction as well but that seems a bit messy)

