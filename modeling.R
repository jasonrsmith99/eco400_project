library(tidyverse)

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
