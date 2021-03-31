library(tidyverse)
library(texreg)

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

model_vars2 <- model_vars2 %>% 
  filter(PRTAGE >= 18)

lpm2 <- lm(PEINHOME ~ PRTAGE + PTDTRACE + PEHSPNON + GEDIV + PEEDUCA + PEMLR + PRDISFLG + GEDIV*PTDTRACE, data = model_vars2, weights = PWSSWGT)

summary(lpm2) #
