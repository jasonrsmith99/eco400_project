library(tidyverse)
library(texreg)
library(lmtest)
library(sandwich)

load(here::here("data", "cps_clean.RData"))

model_vars <- cps_supplement %>% 
  select(PEINHOME, PRTAGE, PTDTRACE, PEHSPNON, GEDIV, PEEDUCA,
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

#separate data by region
ne <- model_vars %>% 
  filter(GEDIV == "New England")

mid_alt <- model_vars %>% 
  filter(GEDIV == "Middle Atlantic")

enc <- model_vars %>% 
  filter(GEDIV == "East North Central")

wnc <- model_vars %>% 
  filter(GEDIV == "West North Central")

sa <- model_vars %>% 
  filter(GEDIV == "South Atlantic")

esc <- model_vars %>% 
  filter(GEDIV == "East South Central")

wsc <- model_vars %>% 
  filter(GEDIV == "West South Central")

mnt <- model_vars %>%
  filter(GEDIV == "Mountain")

pac <- model_vars %>% 
  filter(GEDIV == "Pacific")


#probit model

mod_ne <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = ne)

mod_mid_alt <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = mid_alt)

mod_enc <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = enc)

mod_wnc <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = wnc)

mod_sa <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = sa)

mod_esc <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = esc)

mod_wsc <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = wsc)

mod_mnt <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = mnt)

mod_pac <- glm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PREXPLF + PRDISFLG, family = binomial(link = "probit"),
              data = pac)

mod_ne <- coeftest(mod_ne, vcov. = vcovHC, type = "HC1")

mod_mid_alt <- coeftest(mod_mid_alt, vcov. = vcovHC, type = "HC1")

mod_enc <- coeftest(mod_enc, vcov. = vcovHC, type = "HC1")

mod_wnc <- coeftest(mod_wnc, vcov. = vcovHC, type = "HC1")

mod_sa <- coeftest(mod_sa, vcov. = vcovHC, type = "HC1")

mod_esc <- coeftest(mod_esc, vcov. = vcovHC, type = "HC1")

mod_wsc <- coeftest(mod_wsc, vcov. = vcovHC, type = "HC1")

mod_mnt <- coeftest(mod_mnt, vcov. = vcovHC, type = "HC1")

mod_pac <- coeftest(mod_pac, vcov. = vcovHC, type = "HC1")

screenreg(list(mod_ne, mod_mid_alt, mod_enc,
               mod_wnc, mod_sa, mod_esc, 
               mod_wsc, mod_mnt, mod_pac), custom.coef.names = c("(Intercept)", "Age", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander",
                                        "Race Other", "Non-Hispanic", "Highschool or GED", "Some college", "Associate's", "Bachelor's", "Master's", "Professional", "Docterate", "Unemployed", "No Disability"),
          custom.model.names = c("New England", "Mid Altlantic", "East North Central", 
                                 "West North Central", "South Atlantic", "East South Central", 
                                 "West South Central", "Mountain", "Pacific"),
          custom.header = list("My Model" = 1:9),
          digits = 4, stars = c(.01, .05, .1))

texreg(list(mod_ne, mod_mid_alt, mod_enc,
               mod_wnc, mod_sa, mod_esc, 
               mod_wsc, mod_mnt, mod_pac), custom.coef.names = c("(Intercept)", "Age", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander",
                                                                 "Race Other", "Non-Hispanic", "Highschool or GED", "Some college", "Associate's", "Bachelor's", "Master's", "Professional", "Docterate", "Unemployed", "No Disability"),
          custom.model.names = c("New England", "Mid Altlantic", "East North Central", 
                                 "West North Central", "South Atlantic", "East South Central", 
                                 "West South Central", "Mountain", "Pacific"),
          caption = "Regression Model by Region", caption.above = TRUE,
          digits = 4, stars = c(.01, .05, .1))


car::Anova(c(mod_enc, mod_esc, mod_mid_alt, mod_mnt, mod_ne, mod_pac, mod_sa, mod_wnc, mod_wsc))

#TODO
#post-hoc testing?
#Figure out how to add weights so glm algorithm converges

