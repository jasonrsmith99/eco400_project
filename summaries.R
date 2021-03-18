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
  select(GEDIV, `Has Home Internet`, n) %>% 
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
  select(HETENURE, `Has Home Internet`, n) %>% 
  rename("Household Tenure" = HETENURE) %>%
  mutate(`Has Home Internet` = round(`Has Home Internet`, 2),
         `Has Home Internet` = paste(`Has Home Internet`, "%", sep = "")) %>% 
  xtable()

#make tex output to compile to PDF
print(tenure_table, file = "output/tenure_table.tex")



#add how many have ever had internet at home

#by region, among people who currently don't have home internet
ever_int_region <- household %>% 
  select(GEDIV, HEINHOME, HEEVRHOM) %>% 
  filter(!is.na(HEINHOME)) %>% 
  filter(HEINHOME == 0)

ever_int_region_table <- ever_int_region %>% 
  group_by(GEDIV) %>% 
  summarise(internet = sum(HEEVRHOM), n = n(), `Ever Had Home Internet` = (internet / n) * 100) %>% 
  select(GEDIV, `Ever Had Home Internet`, n) %>% 
  rename("Region" = GEDIV) %>% 
  mutate(`Ever Had Home Internet` = round(`Ever Had Home Internet`, 2),
         `Ever Had Home Internet` = paste(`Ever Had Home Internet`, "%", sep = "")) %>% 
  xtable()

print(ever_int_region_table, file = "output/ever_int_region.tex")

#by home tenure, among people who currently don't have home internet
ever_int_tenure <- household %>% 
  select(HETENURE, HEINHOME, HEEVRHOM) %>% 
  filter(!is.na(HEINHOME)) %>% 
  filter(HEINHOME == 0)

ever_int_tenure_table <- ever_int_tenure %>% 
  group_by(HETENURE) %>% 
  summarise(internet = sum(HEEVRHOM), n = n(), `Ever Had Home Internet` = (internet / n) * 100) %>% 
  select(HETENURE, `Ever Had Home Internet`, n) %>% 
  rename("Household Tenure" = HETENURE) %>% 
  mutate(`Ever Had Home Internet` = round(`Ever Had Home Internet`, 2),
         `Ever Had Home Internet` = paste(`Ever Had Home Internet`, "%", sep = "")) %>% 
  xtable()

print(ever_int_tenure_table, file = "output/ever_int_tenure.tex")

#home internet by race (percent of people in households with home internet)
int_race <- cps_supplement %>%
  select(PTDTRACE, HEINHOME, HEEVRHOM) %>% 
  filter(!is.na(HEINHOME))

int_race_table <- int_race %>% 
  group_by(PTDTRACE) %>% 
  summarise(internet = sum(HEINHOME), n = n(), `Has Home Internet` = (internet / n) * 100) %>%
  select(PTDTRACE, `Has Home Internet`, n) %>% 
  rename("Race" = PTDTRACE) %>% 
  mutate(`Has Home Internet` = round(`Has Home Internet`, 2),
         `Has Home Internet` = paste(`Has Home Internet`, "%", sep = "")) %>% 
  xtable()

print(int_race_table, file = "output/int_race_table.tex")

#ever had home internet by race (percent of people in households with home internet)
ever_int_race <- int_race %>% 
  filter(HEINHOME == 0)

ever_int_race_table <- ever_int_race %>%
  group_by(PTDTRACE) %>% 
  summarise(internet = sum(HEEVRHOM), n = n(), `Has Home Internet` = (internet / n) * 100) %>%
  select(PTDTRACE, `Has Home Internet`, n) %>% 
  rename("Race" = PTDTRACE) %>% 
  mutate(`Has Home Internet` = round(`Has Home Internet`, 2),
         `Has Home Internet` = paste(`Has Home Internet`, "%", sep = "")) %>% 
  xtable()

print(ever_int_race_table, file = "output/ever_int_race_table.tex")

#reason for not having home internet (if they ever had it)
no_int_reason <- household %>% 
  select(HEINHOME, HEPRINOH) %>% 
  filter(!is.na(HEINHOME)) %>% 
  filter(HEINHOME == 0) %>% 
  group_by(HEPRINOH) %>% 
  summarise(n = n(), `%` = (n / 8524) * 100) %>% 
  mutate(`%` = round(`%`, 2)) %>% 
  rename("Reason for not Having Home Internet" = HEPRINOH) %>% 
  xtable()

print(no_int_reason, file = "output/no_int_reason.tex")
