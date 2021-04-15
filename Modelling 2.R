library(tidyverse)
library(texreg)
library(xtable)
library(rstatix)

load(here::here("data", "cps_clean.RData"))

model_vars <- cps_supplement %>% 
  dplyr::select(id, PEINHOME, PRTAGE, PTDTRACE, PEHSPNON, GEDIV, PEEDUCA,
                PREXPLF, PRDISFLG, PWSSWGT, HETENURE)

#releveling education variable
model_vars$PEEDUCA <- fct_other(model_vars$PEEDUCA, drop = c("Less than 1st grade", "1st - 3rd grade", "5th - 6th grade",
                                                             "7th - 8th grade", "9th grade", "10th grade", "11th grade",
                                                             "12th grade no diploma"), other_level = "Less than High School")
model_vars$PEEDUCA <- fct_other(model_vars$PEEDUCA, drop = c("Associate degree (academic)", "Associate degree (vocational)"),
                                other_level = "Associate's degree")

model_vars$PEEDUCA <- fct_relevel(model_vars$PEEDUCA, levels = c("Less than High School", "High school or GED", "Some college (no degree)",
                                                                 "Associate's degree", "Bachelor's degree", "Master's degree",
                                                                 "Professional degree", "Doctorate degree"))

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


white <- model_vars2 %>% 
  filter(PTDTRACE == "White")

black <- model_vars2 %>% 
  filter(PTDTRACE == "Black")

asian <- model_vars2 %>% 
  filter(PTDTRACE == "Asian")

native <- model_vars2 %>% 
  filter(PTDTRACE == "American Indian/Alaskan Native")

hispanic <- model_vars2 %>% 
  filter(PEHSPNON == "Hispanic")

lm_white <- lm(PEINHOME ~ PRTAGE + GEDIV + PEEDUCA + PEMLR + PRDISFLG, data = white, weights = PWSSWGT)
lm_black <- lm(PEINHOME ~ PRTAGE + GEDIV + PEEDUCA + PEMLR + PRDISFLG, data = black, weights = PWSSWGT)
lm_asian <- lm(PEINHOME ~ PRTAGE + GEDIV + PEEDUCA + PEMLR + PRDISFLG, data = asian, weights = PWSSWGT)
lm_native <- lm(PEINHOME ~ PRTAGE + GEDIV + PEEDUCA + PEMLR + PRDISFLG, data = native, weights = PWSSWGT)
lm_hispanic <- lm(PEINHOME ~ PRTAGE + GEDIV + PEEDUCA + PEMLR + PRDISFLG, data = hispanic, weights = PWSSWGT) 

texreg(list(lm_white, lm_black, lm_asian, lm_native, lm_hispanic),
          custom.model.names = c("White", "Black", "Asian", "American Indian/Alaskan Native", "Hispanic"),
          custom.coef.names = c("(Intercept)", "Age", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic",
                                "East South Central", "West South Central", "Mountain", "Pacific", "Highschool/GED", "Some College",
                                "Associate's", "Bachelor's", "Master's", "Professional", "Doctorate", "Unemployed", "Not in Labor Force", "No disability"),
       digits = 4, 
       stars = c(.01, .05, .1),
       caption = "Table X: Racial Breakdown",
       caption.above = TRUE,
       file = "output/race_mods.tex")


#full race interaction
race_inter <- lm(PEINHOME ~ PRTAGE*PTDTRACE + PTDTRACE*PTDTRACE + GEDIV*PTDTRACE + PEEDUCA*PTDTRACE + PEMLR*PTDTRACE + PRDISFLG*PTDTRACE, data = model_vars2, weights = PWSSWGT)

#anova
summary(aov(race_inter))

#significant difference with education tukey to find differences
diff <- TukeyHSD(aov(race_inter), "PTDTRACE:PEEDUCA")

diff <- broom::tidy(diff)
diff <- diff %>% 
  separate(contrast, into = c("group1", "group2"), sep = "-")
diff <- diff %>% 
  filter(str_detect(group1, "Black") & str_detect(group2, "White"))
diff <- diff %>% 
  filter(row_number() %in% c(1,9,16,22,27,31,34,36))
diff <- diff %>% 
  select(group1, group2, estimate, adj.p.value)
diff <- diff %>% 
  mutate(group1 = str_replace(group1, ":", " "),
         group2 = str_replace(group2, ":", " "))
diff <- diff %>% 
  unite("comparision", c(group1, group2), sep = " - ")
diff <- diff %>% 
  mutate(significance = case_when(adj.p.value <= .01 ~ "***",
                                  adj.p.value <=.05 ~ "**",
                                  adj.p.value <= .1 ~ "*",
                                  adj.p.value > .1 ~ "ns"),
         adj.p.value = round(adj.p.value, 4),
         estimate = round(estimate, 4)) 
print(
  xtable(diff, digits = 4),
  file = "output/raceinter_tukey.tex")
