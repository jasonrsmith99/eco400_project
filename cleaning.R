library(tidyverse)
library(tidyselect)

internet <- read_csv(here::here("data", "nov19pub.csv"))

#general CPS data to include
demo_vars <- read_csv(here::here("data", "included_vars.csv"))

industry_codes <- read_csv(here::here("data", "industry_codes.csv"))

occupation_codes <- read_csv(here::here("data", "occupation_codes.csv"))

#make vectors for includes variables, industry codes, and occupation codes
demo_vars <- pull(demo_vars, vars)

industry_codes <- pull(industry_codes, industry)

occupation_codes <- pull(occupation_codes, occupation)

#select completed surveys
survey_complete <- internet %>%
  filter(HUFINAL == 1 | HUFINAL == 201)

#add unique id
survey_complete <- survey_complete %>% 
  mutate(id = 1:nrow(survey_complete)) %>% 
  relocate(id)

#break into general and supplement questions (to clean supplement portion)
cps_questions <- survey_complete %>% 
  select(id, demo_vars)

supplement_questions <- survey_complete %>% 
  select(id, HEINHOME:PEINOTHR, HEMOBDAT, HEHOMSU, HEHOMTE1:HEHOMTE4, HEEVRHOM, HEPRINOH, HEPSENSI)

#clean supplement questions
#remove NAS
for (i in 1:ncol(supplement_questions)) {
  if (colnames(supplement_questions[i]) == "id") {
    next
  } 
for (j in 1:nrow(supplement_questions)) {
    if (supplement_questions[j, i] < 0) {
      supplement_questions[j, i] = NA
    }
  }
}

#Make yes/no questions 0 and 1
for (i in 1:ncol(supplement_questions)) {
  for (j in 1:nrow(supplement_questions)) {
    if (colnames(supplement_questions[i]) == "HEHOMSU" | colnames(supplement_questions[i]) == "id" | colnames(supplement_questions[i] == "HEPRINOH")) {
      next
    }
    if (is.na(supplement_questions[j, i]) == TRUE) {
      next
    } 
    if (supplement_questions[j,i] == 2) {
      supplement_questions[j, i] = 0
    }
  }
  basicPlotteR::progress(i, ncol(supplement_questions))
}

#clean demographic information
#Make Negatives NA
for (i in 1:ncol(cps_questions)) {
  for (j in 1:nrow(cps_questions)) {
    if (demo_questions[j, i] < 0) {
      demo_questions[j, i] = NA
    }
  }
  
}

#Make yes/no 0 and 1
for (i in 1:ncol(cps_questions)) {
  for (j in 1:nrow(cps_questions)) {
    if (colnames(cps_questions[i]) != "PRDISFLG" | colnames(cps_questions[i]) != "id") {
      next
    }
    if (is.na(cps_questions[j, i]) == TRUE) {
      next
    } 
    if (cps_questions[j,i] == 2) {
      cps_questions[j, i] = 0
    }
  }
basicPlotteR::progress(i, ncol(cps_questions))
}

#join datasets
cps_supplement <- cps_questions %>% 
  inner_join(supplement_questions, by = "id")


#adding factors to necessary variables
cps_supplement$HETENURE <- factor(cps_supplement$HETENURE, labels = c("Owned by household memeber", "Rented for cash", "Occupied without payment of cash rent"))
cps_supplement$GEREG <- factor(cps_supplement$GEREG, labels = c("Northeast", "Midwest", "South", "West"))
cps_supplement$GEDIV <- factor(cps_supplement$GEDIV, labels = c("New England", "Middle Atlantic", "East North Central", "West North Central",
                                                                "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"))
cps_supplement$GTCBSASZ <- factor(cps_supplement$GTCBSASZ, labels = c("Not Metropolitian", "100,000 - 249,999", "250,000 - 499,999", "500,000 - 999,999",
                                                                      "1,000,000 - 2,499,999", "2,500,000 - 4,999,999", "5,000,000+"))
cps_supplement$PEMARITL <- factor(cps_supplement$PEMARITL, labels = c("Married - spouse present", "Married - spouse absent", "Widowed", "Divorced", "Separated", "Never married"))
cps_supplement$PESEX <- factor(cps_supplement$PESEX, labels = c("Male", "Female"))
cps_supplement$PEEDUCA <- factor(cps_supplement$PEEDUCA, labels = c("Less than 1st grade", "1st - 3rd grade", "5th - 6th grade", "7th - 8th grade", "9th grade", "10th grade", "11th grade",
                                                                    "12th grade no diploma", "High school or GED", "Some college (no degree)", "Associate degree (vocational)", "Associate degree (academic)",
                                                                    "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree"))
cps_supplement$PTDTRACE <- factor(cps_supplement$PTDTRACE, labels = c("White", "Black", "American Indian/Alaskan Native", "Asian", "Hawaiian/Pacific Islander", "White-Black", "White-AI",
                                                                      "White-Asian", "White-HP", "Black-AI", "Black-Asian", "Black-HP", "AI-Asian", "AI-HP", "Asian-HP", "W-B-AI", "W-B-A", "W-AI-A", "W-AI-HP",
                                                                      "W-A-HP", "W-B-AI-A", "Other 4 and 5 race combination"))
cps_supplement$PTDTRACE <- fct_other(cps_supplement$PTDTRACE, keep = c("White", "Black", "Asian", "American Indian/Alaskan Native", "Hawaiian/Pacific Islander"))
cps_supplement$PEHSPNON <- factor(cps_supplement$PEHSPNON, labels = c("Hispanic", "Nonhispanic"))
cps_supplement$PREMPNOT <- factor(cps_supplement$PREMPNOT, labels = c("Employed", "Unemployed", "Discouraged", "Other"))
cps_supplement$PRFTLF <- factor(cps_supplement$PRFTLF, labels = c("Full-time", "Part-time"))
cps_supplement$PRCOWPG <- factor(cps_supplement$PRCOWPG, labels = c("Private", "Government"))
cps_supplement$PRDTIND1 <- factor(cps_supplement$PRDTIND1, labels = industry_codes)
cps_supplement$PRDTOCC1 <- factor(cps_supplement$PRDTOCC1, labels = occupation_codes)
cps_supplement$PRNLFSCH <- factor(cps_supplement$PRNLFSCH, labels = c("In school", "Not in school"))
cps_supplement$PRDISFLG <- factor(cps_supplement$PRDISFLG, labels = c("Disability", "No disability"))
cps_supplement$HEHOMSU <- factor(cps_supplement$HEHOMSU, labels = c("ISP", "Nonprofit/public agency/cooperative", "Included in housing costs", "Publically available", "Other"))
cps_supplement$HEPRINOH <- factanal(cps_supplement$HEPRINOH, labels = c("Don't need or not interested", "Can't afford it", "Not worth the cost", "Can use it elsewhere", "Not available in area",
                                                                        "No computing device, or device inadequate or broken", "Online privacy or cybersecurity concern", "Personal safety concern",
                                                                        "Household moved or is in the process of moving", "Other"))

#save clean data
save(cps_supplement, file = "data/cps_clean.RData")