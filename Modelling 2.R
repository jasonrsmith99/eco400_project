library(tidyverse)
library(texreg)
library(xtable)

nov19pub <- read_csv("data/nov19pub.csv")

nov19pub <- nov19pub %>% 
  mutate(id = 1:nrow(nov19pub)) %>% 
  select(id, PEMLR)

nov19pub[nov19pub == -1] <- NA

nov19pub$PEMLR <- factor(nov19pub$PEMLR, labels = c("employed1", "employed2", "unemployed1", "unemployed2", "NILF_ret", "NILF_dis", "NILF_oth"))
nov19pub$PEMLR <- fct_other(nov19pub$PEMLR, drop = c("employed1", "employed2"), other_level = "Employed")
nov19pub$PEMLR <- fct_other(nov19pub$PEMLR, drop = c("unemployed1", "unemployed2"), other_level = "Unemployed")
nov19pub$PEMLR <- fct_other(nov19pub$PEMLR, drop = c("NILF_ret", "NILF_dis", "NILF_oth"), other_level = "Not in Labor Force")


model_vars2 <- model_vars %>% right_join(nov19pub, by = "id")


lpm <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + GEDIV + PEEDUCA + PEMLR + PRDISFLG, data = model_vars2, weights = PWSSWGT)



texreg(lpm, custom.model.names = "Base Model",
          custom.coef.names = c("(Intercept)", "Age", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander", "Other",
                                "Non-hispanic", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic",
                                "East South Central", "West South Central", "Mountain", "Pacific", "Highschool/GED", "Some College",
                                "Associate's", "Bachelor's", "Master's", "Professional", "Doctorate", "Unemployed", "Not in Labor Force", "No disability"),
          digits = 4, stars = c(.01, .05, .1),
          include.fstatistic = TRUE,
          custom.note = "%stars. \nF-statistic: 109.1 on 24 and 58429 DF, p-value: < 2.22e-16",
          file = "output/base_model.tex")




#interaction model
lpm_interact <- lm(PEINHOME ~ PRTAGE*GEDIV + PTDTRACE*GEDIV + PEHSPNON*GEDIV + PEEDUCA*GEDIV + PEMLR*GEDIV + PRDISFLG*GEDIV, data = model_vars2, weights = PWSSWGT)
lpm_race <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG + GEDIV*PTDTRACE, data = model_vars2, weights = PWSSWGT)
lpm_dis <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG*GEDIV, data = model_vars2, weights = PWSSWGT)
lpm_emp <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + GEDIV*PEMLR + PRDISFLG, data = model_vars2, weights = PWSSWGT)
lpm_edu <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + GEDIV*PEEDUCA + PEMLR + PRDISFLG, data = model_vars2, weights = PWSSWGT)
lpm_his <- lm(PEINHOME ~ PRTAGE + PTDTRACE + GEDIV*PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = model_vars2, weights = PWSSWGT)
lpm_age <- lm(PEINHOME ~ GEDIV*PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = model_vars2, weights = PWSSWGT)

print(
  xtable(
    anova(lpm, lpm_interact, lpm_age, lpm_race, lpm_his, lpm_edu, lpm_emp, lpm_dis)
    ),
  file = "output/anova.tex")

ne <- model_vars2 %>% filter(GEDIV == "New England")
ma <- model_vars2 %>% filter(GEDIV == "Middle Atlantic")
enc <- model_vars2 %>% filter(GEDIV == "East North Central")
wnc <- model_vars2 %>% filter(GEDIV == "West North Central")
sa <- model_vars2 %>% filter(GEDIV == "South Atlantic")
esc <- model_vars2 %>% filter(GEDIV == "East South Central")
wsc <- model_vars2 %>% filter(GEDIV == "West South Central")
mnt <- model_vars2 %>% filter(GEDIV == "Mountain")
pac <- model_vars2 %>% filter(GEDIV == "Pacific")

lm_ne <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = ne, weights = PWSSWGT)
lm_ma <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = ma, weights = PWSSWGT)
lm_enc <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = enc, weights = PWSSWGT)
lm_wnc <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = wnc, weights = PWSSWGT)
lm_sa <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = sa, weights = PWSSWGT)
lm_esc <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = esc, weights = PWSSWGT)
lm_wsc <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = wsc, weights = PWSSWGT)
lm_mnt <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = mnt, weights = PWSSWGT)
lm_pac <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + PEEDUCA + PEMLR + PRDISFLG, data = pac, weights = PWSSWGT)

texreg(list(lm_ne, lm_ma, lm_enc, lm_wnc, lm_sa, lm_esc, lm_wsc, lm_mnt, lm_pac),
          custom.model.names = c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central",
                                 "West South Central", "Mountain", "Pacific"),
          custom.coef.names = c("(Intercept)", "Age", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander", "Other",
                                "Non-hispanic", "Highschool/GED", "Some College",
                                "Associate's", "Bachelor's", "Master's", "Professional", "Doctorate", "Unemployed", "Not in Labor Force", "No disability"),
          digits = 4, stars = c(.01, .05, .1),
       file = "output/interact_model.tex")
