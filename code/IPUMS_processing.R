library(dplyr)
library(readr)
library(Hmisc)

ipums_data <- read_csv("E:\\Downloads\\usa_00003.csv\\usa_00003.csv")

adults_edu <- ipums_data %>%
  filter(AGE >= 25)

adults_edu <- adults_edu %>% mutate(
  Bachelor = ifelse(EDUCD == 101, 1, 0),
  HighSchool = ifelse(EDUCD == 62, 1, 0),
  Associates = ifelse(EDUCD == 81, 1, 0),
  Masters = ifelse(EDUCD == 114, 1, 0),
  Doctors = ifelse(EDUCD == 116, 1, 0)
)

state_edu <- adults_edu %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    total_pop = sum(PERWT, na.rm=TRUE),
    bach_pop = sum(PERWT * Bachelor, na.rm=TRUE),
    hs_pop = sum(PERWT * HighSchool, na.rm = TRUE),
    assoc_pop = sum(PERWT * Associates, na.rm = TRUE),
    masters_pop  = sum(PERWT * Masters, na.rm = TRUE),
    drs_pop  = sum(PERWT * Doctors, na.rm = TRUE)
  ) %>%
  ungroup()

write.csv(state_edu, "State_Education_by_Degree.csv", row.names = FALSE)

age <- ipums_data %>%
  select(AGE, STATEFIP, YEAR, PERWT) %>%
  mutate(
    voter = ifelse(AGE >= 18, 1, 0)
  )

state_age <- age %>% group_by(STATEFIP, YEAR) %>%
  summarise(
    med_age = wtd.quantile(AGE, weights = PERWT, probs = 0.5, na.rm=TRUE),
    voting_pop = sum(PERWT * voter, na.rm = TRUE)
  ) %>% ungroup()

glimpse(state_age)

write.csv(state_age, "State_by_Age.csv")
