library(dplyr)
library(readr)
library(Hmisc)

ipums_data <- read_csv("E:\\Downloads\\usa_00003.csv\\usa_00003.csv")

adults_edu <- ipums_data %>%                        # Filter data to only include adults
  filter(AGE >= 25)

adults_edu <- adults_edu %>% mutate(
  Bachelor = ifelse(EDUCD == 101, 1, 0)            # Create bachelor category
)

state_edu <- adults_edu %>%                        # Create bachelor population and total state population by year and state
  group_by(STATEFIP, YEAR) %>%
  summarise(
    total_pop = sum(PERWT, na.rm=TRUE),
    bach_pop = sum(PERWT * Bachelor, na.rm=TRUE)
  ) %>%
  ungroup()

write.csv(state_edu, "State_Education_by_Degree.csv", row.names = FALSE)    # Store this data

age <- ipums_data %>%                                
  select(AGE, STATEFIP, YEAR, PERWT)

state_age <- age %>% group_by(STATEFIP, YEAR) %>%                            # Find median age for each year and state
  summarise(
    med_age = wtd.quantile(AGE, weights = PERWT, probs = 0.5, na.rm=TRUE)
  ) %>% ungroup()

glimpse(state_age)

write.csv(state_age, "State_by_Age.csv")

