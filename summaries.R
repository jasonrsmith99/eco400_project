#Do we somehow have to incorporate weights?

library(tidyverse)
library(tidyselect)
library(xtable)

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
region_table <- int_region %>% 
  group_by(GEDIV) %>%
  summarise(internet = sum(HEINHOME), n = n(), `Has Home Internet` = (internet / n) * 100) %>% 
  select(GEDIV, `Has Home Internet`) %>% 
  rename("Region" = GEDIV) %>%
  mutate(`Has Home Internet` = round(`Has Home Internet`, 2),
         `Has Home Internet` = paste(`Has Home Internet`, "%", sep = "")) %>% 
  xtable()

#make tex output to compile to pdf
print(region_table, file = "output/region_table.tex")

#calculate proportion of households with home internet by home tenure (household ownership?)

int_tenure <- household %>% 
  select(HETENURE, HEINHOME) %>% 
  filter(!is.na(HEINHOME))

tenure_table <- int_tenure %>% 
  group_by(HETENURE) %>%
  summarise(internet = sum(HEINHOME), n = n(), `Has Home Internet` = (internet / n) * 100) %>% 
  select(HETENURE, `Has Home Internet`) %>% 
  rename("Household Tenure" = HETENURE) %>%
  mutate(`Has Home Internet` = round(`Has Home Internet`, 2),
         `Has Home Internet` = paste(`Has Home Internet`, "%", sep = "")) %>% 
  xtable()

#make tex output to compile to PDF
print(tenure_table, file = "output/tenure_table.tex")



#TODO
#add how many have ever had internet at home to each table
#home internet by race
#reason for not having home internet (if they ever had it)
#some of these could be graphs I guess
