library(tidyverse)
library(tidyselect)

load(here::here("data", "cps_clean.RData"))

#calculate proportion of households with home internet by region

#isolate household data (one entry per household)
household <- cps_supplement %>% 
  select(starts_with("H") | starts_with("G")) %>% 
  distinct(HRHHID, .keep_all = TRUE)

#remove housholds with missing response
int_region <- household %>% 
  select(GEDIV, HEINHOME) %>% 
  filter(!is.na(HEINHOME))

#calculate relative frequencies
int_region %>% 
  group_by(GEDIV) %>%
  summarise(internet = sum(HEINHOME), n = n(), `Has Home Internet` = (internet / n) * 100) %>% 
  select(GEDIV, `Has Home Internet`) %>% 
  rename("Region" = GEDIV) %>%
  mutate(`Has Home Internet` = round(`Has Home Internet`, 2),
         `Has Home Internet` = paste(`Has Home Internet`, "%", sep = ""))


#calculate proportion of households with home internet by home tenure (household ownership?)

int_tenure <- household %>% 
  select(HETENURE, HEINHOME) %>% 
  filter(!is.na(HEINHOME))

int_tenure %>% 
  group_by(HETENURE) %>%
  summarise(internet = sum(HEINHOME), n = n(), `Has Home Internet` = (internet / n) * 100) %>% 
  select(HETENURE, `Has Home Internet`) %>% 
  rename("Household Tenure" = HETENURE) %>%
  mutate(`Has Home Internet` = round(`Has Home Internet`, 2),
         `Has Home Internet` = paste(`Has Home Internet`, "%", sep = ""))
