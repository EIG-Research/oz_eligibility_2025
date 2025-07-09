# Project: Mapping new opportunity zones in the 2025 House Reconciliation Bill
# File Description: Summary statistics and comparison with OZ 1.0
# last update: 6/16/2025 by Jiaxin He

# remove dependencies
rm(list = ls())

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(scales)
library(stringr)
library(openxlsx)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/oz_eligibility_2025"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_output <- file.path(path_project, "output")

### Load master file and old OZ data ###
acs_combined_oz <- read_xlsx(file.path(path_output, "tracts_by_OZ_eligibility.xlsx"))
oz_old_summary <- read.xlsx(file.path(path_data, "Master Census Tract File_2012 to 2016.xlsx"), sheet = "Summary")
oz_old_summary <- oz_old_summary %>% rename(Designation = `X1`) %>%
  filter(Designation %in% c("All Tracts", "LIC Tracts", "OZ Tracts")) %>%
  mutate(`Share of US Population` = `Total.Population` / oz_old_summary$`Total.Population`[1],
         Designation = c("National (2016)", "OZ-Eligible Tracts, TCJA (2017)", "OZ Tracts, Currently Designated"),
         `Number of Tracts` = as.numeric(`Number.of.Tracts`),
         `Data Vintage` = "2012-2016 ACS",
         `Tract Vintage` = "2010 Decennial") %>%
  select(Designation, `Data Vintage`, `Tract Vintage`, `Number of Tracts`, `Share of US Population`,
         `Poverty.Rate`, `Average.Median.Family.Income`,
         `Percent.Non-White`, `Percent.without.HS.diploma`,
         `Percent.with.Bachelors.+`, `Percent.Prime.Age.Adults.Not.Working`, `Vacancy.Rate`) %>%
  rename(`Poverty Rate` = `Poverty.Rate`,
         `Average Median Family Income` = `Average.Median.Family.Income`,
         `Percent Non-White` = `Percent.Non-White`,
         `Percent without HS diploma` = `Percent.without.HS.diploma`,
         `Percent with Bachelors +` = `Percent.with.Bachelors.+`,
         `Percent Prime Age Adults Not Working` = `Percent.Prime.Age.Adults.Not.Working`,
         `Vacancy Rate` = `Vacancy.Rate`) %>%
  mutate(Designation = factor(Designation, levels = c("OZ-Eligible Tracts, TCJA (2017)", "OZ Tracts, Currently Designated", "National (2016)"))) %>%
  arrange(Designation)

### Generate summary statistics ###
oz_25_summary <- acs_combined_oz %>%
  filter(oz_eligible == "OZ eligible", GEOID_st != "72") %>% # Exclude PR from summaries
  summarise(
    Designation = "OZ-Eligible Tracts, BBB (2025)",
    `Number of Tracts` = n(),
    `Share of US Population` = sum(population) / sum(acs_combined_oz$population),
    `Poverty Rate` = mean(poverty_rte),
    `Average Median Family Income` = mean(mfi, na.rm = TRUE),
    `Percent Non-White` = mean(share_nonwhite),
    `Percent without HS diploma` = mean(share_no_hs_adult),
    `Percent with Bachelors +` = mean(share_ba_above_adult),
    `Percent Prime Age Adults Not Working` = mean(prime_age_not_working, na.rm = TRUE),
    `Vacancy Rate` = mean(vacancy_rate, na.rm = TRUE)
  ) %>% rbind(
    acs_combined_oz %>% filter(GEOID_st != "72") %>%
      summarise(
        Designation = "National (2023)",
        `Number of Tracts` = 84414,
        `Share of US Population` = 1,
        `Poverty Rate` = sum(pop_poverty) / sum(poverty_univ),
        `Average Median Family Income` = mean(mfi, na.rm = TRUE),
        `Percent Non-White` = (sum(race_univ) - sum(race_white)) / sum(race_univ),
        `Percent without HS diploma` = sum(edu_no_hs) / sum(pop_adult),
        `Percent with Bachelors +` = sum(edu_ba_above) / sum(pop_adult),
        `Percent Prime Age Adults Not Working` = (sum(prime_age_unemployed) + sum(prime_age_nilf)) / sum(prime_age_population),
        `Vacancy Rate` = (sum(housing_vacant) - sum(housing_vacant_seasonal)) / sum(housing_total)
      )
  ) %>%
  mutate(`Data Vintage` = "2019-2023 ACS",
         `Tract Vintage` = "2020 Decennial") %>%
  relocate(Designation, `Data Vintage`, `Tract Vintage`)

oz_proportion_summary <- bind_rows(
  bind_cols(oz_old_summary[1,1:3] %>% mutate(Designation = "Eligible areas as % of National, TCJA"),
            oz_old_summary[1,-c(1:3)] / oz_old_summary[3,-c(1:3)]),
  bind_cols(oz_25_summary[1,1:3] %>% mutate(Designation = "Eligible areas as % of National, BBB"),
            oz_25_summary[1,-c(1:3)] / oz_25_summary[2,-c(1:3)])
)

oz_combined_summary <- bind_rows(oz_old_summary, oz_25_summary) %>%
  mutate(across(c(5,6,8:12), ~ .x*100)) %>%
  bind_rows(., oz_proportion_summary %>% mutate(across(c(4:12), ~ .x*100)))

write.csv(oz_combined_summary, file = file.path(path_output, "OZ Comparative Summary Table.csv"))
