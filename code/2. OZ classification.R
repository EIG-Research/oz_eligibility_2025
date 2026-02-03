
################################################################################
# Description: Eligible OZs, as in the 2025 Big Beautiful Bill
    # Bill text can be found here: https://www.congress.gov/bill/119th-congress/house-bill/1/text
    # see pages 390 - 420
    # definitions are on pages 392 - 393
# Author: Sarah Eckhardt (sarah@eig.org) and Jason He (jiaxin@eig.org)
# Last updated: Jan 29th, 2026
################################################################################

# remove dependencies
rm(list = ls())

# load required packages
library(dplyr)
library(tidyr)
library(tidycensus) # for ACS data
library(sf) # for shapefile construction
library(tigris) # for tract and msa shapefiles

# set user-specific project paths
project_directories <- list(
  "name" = "PATH TO DIRECTORY"
)

current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_output <- file.path(path_project, "output")
path_data <- file.path(path_project, "data")


# Criteria:
  # Low income communities:
      # median family income that does not exceed 70% of
        # statewide median family income if not in a MSA
        # metropolitan area median family income if in a MSA
    # OR:
        # poverty rate is at least 20% AND
        # median family income dies not exceed 125% of
          # statewide median family income if not in a MSA
          # MSA median family income if within a MSA

# contiguous census tracts: REPEALED

#########################
# pull in ACS variables #
#########################

### Determine ACS variables ###
acs_vars <- load_variables(2024, "acs5", cache = TRUE)

# variable list:
tract_vars = c(
  # poverty
  "poverty_univ" = "B17001_001", # population universe
  "pop_poverty" = "B17001_002", # population under poverty line
  
  # mfi
  "mfi" = "B19113_001", # median family income
  
  # population
  "population" = "B01003_001",
  "pop_adult_male" = "B05003_008", # Adult Male
  "pop_adult_female" = "B05003_019",  # Adult Female
  
  # unemployment
  "labor_force" = "B23025_003",
  "unemployed" = "B23025_005",
  
  # education attainment
  "edu_no_hs" = "B06009_002",    # Without high school diploma
  "edu_bachelor" = "B06009_005", # Bachelor's degree
  "edu_graduate" = "B06009_006", # Graduate degree
  
  # race
  "race_univ" = "B02001_001",
  "race_white" = "B01001H_001",
  
  # prime age population
  "pop_male_25_29" = "B23001_024",
  "pop_male_30_34" = "B23001_031",
  "pop_male_35_44" = "B23001_038",
  "pop_male_45_54" = "B23001_045",
  "pop_fem_25_29" = "B23001_110",
  "pop_fem_30_34" = "B23001_117",
  "pop_fem_35_44" = "B23001_124",
  "pop_fem_45_54" = "B23001_131",
  
  # prime age unemployment
  "unemp_male_25_29" = "B23001_029",
  "unemp_male_30_34" = "B23001_036",
  "unemp_male_35_44" = "B23001_043",
  "unemp_male_45_54" = "B23001_050",
  "unemp_fem_25_29" = "B23001_115",
  "unemp_fem_30_34" = "B23001_122",
  "unemp_fem_35_44" = "B23001_129",
  "unemp_fem_45_54" = "B23001_136",
  
  # prime age not in labor force
  "nilf_male_25_29" = "B23001_030",
  "nilf_male_30_34" = "B23001_037",
  "nilf_male_35_44" = "B23001_044",
  "nilf_male_45_54" = "B23001_051",
  "nilf_fem_25_29" = "B23001_116",
  "nilf_fem_30_34" = "B23001_123",
  "nilf_fem_35_44" = "B23001_130",
  "nilf_fem_45_54" = "B23001_137",
  
  # housing vacancy
  "housing_total" = "B25002_001",
  "housing_vacant" = "B25002_003",
  "housing_vacant_seasonal" = "B25004_006"
)

state_vars = c(
  "st_mfi" = "B19113_001"
)

msa_vars = c(
  "msa_mfi" = "B19113_001"
)  

# pull in tract data
tracts_list = list()
for(st in c(state.abb, "DC", "PR")){
tract_pull = get_acs(
  geography = "tract",
  variables = tract_vars,
  year = 2024,
  survey = "acs5",
  state = st,
  geometry = FALSE)
  tracts_list[[st]] <- tract_pull
}

census_tracts <- bind_rows(tracts_list) %>%
  select(-moe) %>% rename(tract = NAME) %>%
  mutate(variable_aggregate = case_when(
    variable %in% c("pop_adult_male", "pop_adult_female") ~ "pop_adult",
    variable %in% c("edu_bachelor", "edu_graduate") ~ "edu_ba_above",
    variable %in% c("pop_male_25_29", "pop_male_30_34",
                    "pop_male_35_44", "pop_male_45_54",
                    "pop_fem_25_29", "pop_fem_30_34",
                    "pop_fem_35_44", "pop_fem_45_54") ~ "prime_age_population",
    variable %in% c("unemp_male_25_29", "unemp_male_30_34",
                    "unemp_male_35_44", "unemp_male_45_54",
                    "unemp_fem_25_29", "unemp_fem_30_34",
                    "unemp_fem_35_44", "unemp_fem_45_54") ~ "prime_age_unemployed",
    variable %in% c("nilf_male_25_29", "nilf_male_30_34",
                    "nilf_male_35_44", "nilf_male_45_54",
                    "nilf_fem_25_29", "nilf_fem_30_34",
                    "nilf_fem_35_44", "nilf_fem_45_54") ~ "prime_age_nilf",
    TRUE ~ variable
  )) %>%
  group_by(GEOID, tract, variable_aggregate) %>%
  summarise(estimate = sum(estimate)) %>% ungroup() %>%
  pivot_wider(names_from = variable_aggregate, values_from = estimate) %>%
  mutate(poverty_rte = pop_poverty/poverty_univ,
         unempl_rte = unemployed/labor_force,
         share_nonwhite = (race_univ - race_white)/race_univ,
         share_no_hs_adult = edu_no_hs/pop_adult,
         share_ba_above_adult = edu_ba_above/pop_adult,
         prime_age_not_working = (prime_age_unemployed + prime_age_nilf)/prime_age_population,
         vacancy_rate = (housing_vacant - housing_vacant_seasonal)/housing_total,
         GEOID_county = substr(GEOID, 1, 5),
         GEOID_st = substr(GEOID, 1,2)) %>%
  select(GEOID, GEOID_st, GEOID_county, tract, population, mfi, poverty_rte, unempl_rte,
         share_nonwhite, share_no_hs_adult, share_ba_above_adult, prime_age_not_working, vacancy_rate,
         pop_poverty, poverty_univ, unemployed, labor_force, race_white, race_univ,
         edu_no_hs, edu_ba_above, pop_adult, prime_age_unemployed, prime_age_nilf, prime_age_population,
         housing_vacant, housing_vacant_seasonal, housing_total)

cl90_z_score <- qnorm(0.95)
census_tract_moe <- bind_rows(tracts_list) %>%
  filter(variable == "mfi") %>%
  select(GEOID, estimate, moe) %>%
  mutate(mfi_90cl_low = estimate - moe,
         mfi_90cl_high = estimate + moe,
         mfi_standard_error = moe/cl90_z_score,
         mfi_moe_over_50pct = moe/estimate > 0.5) %>%
  select(-c(estimate, moe))

census_tracts <- census_tracts %>%
  left_join(census_tract_moe, by = "GEOID")

# clean up
rm(tracts_list, tract_pull, st)

# pull in MSA data
census_msa <- get_acs(
  geography = "cbsa",
  variables = msa_vars,
  year = 2024,
  survey = "acs5",
  geometry = FALSE
) %>%
  
  # filter out micro areas
  filter(stringr::str_detect(NAME, "Metro Area")) %>%
  select(-c(moe)) %>%
  rename(`msa` = NAME) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# pull in State data
census_st <- get_acs(
  geography = "state",
  variables = state_vars,
  year = 2024,
  survey = "acs5",
  geometry = FALSE
) %>%
  select(-c(moe)) %>% rename(state = NAME) %>%
  pivot_wider(names_from = variable, values_from = estimate)

######################################
# tract - to - msa crosswalk #
######################################

# pull in official MSA to county crosswalk
# https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk

county_msa_xwalk <- read.csv(file.path(path_data, "cbsa2fipsxw.csv")) %>%
  filter(metropolitanmicropolitanstatis == "Metropolitan Statistical Area") %>%
  mutate(GEOID_county = paste0(stringr::str_pad(fipsstatecode, pad = "0", side="left",width=2),
                              stringr::str_pad(fipscountycode, pad = "0", side="left",width=3)),
         cbsacode = as.character(cbsacode)) %>%
  select(GEOID_msa = cbsacode, GEOID_county)

#########################
# Generate Eligible OZs # 
#########################

eligible_ozs <- census_tracts %>%
  
  # add in crosswalks
  left_join(county_msa_xwalk, by = "GEOID_county") %>%

  # add in state MFI and MSA MFI (if available)
  left_join(census_msa, by = c("GEOID_msa" = "GEOID")) %>%
  left_join(census_st, by = c("GEOID_st" = "GEOID"))

eligible_ozs = eligible_ozs %>%
  # extract the correct geography to match MFI to
  mutate(mfi_relate = case_when(
    !is.na(msa_mfi) ~ msa_mfi,
    is.na(msa_mfi) ~ st_mfi
    ),
    mfi_ratio = mfi/mfi_relate
  ) %>%
        
  # implement OZ definition
  mutate(oz_eligible_mfi = case_when(
            mfi_ratio <= 0.7 ~ 1,
            mfi_ratio > 0.7 ~ 0,
            TRUE ~ NA
            ),
         oz_eligible_pov = case_when(
           poverty_rte < 0.2 ~ 0,
           poverty_rte >= 0.2 & is.na(mfi) ~ 1, # if no MFI available, eligible based on poverty alone
           poverty_rte >= 0.2 & mfi_ratio <= 1.25 ~ 1,
           poverty_rte >= 0.2 & mfi_ratio > 1.25 ~ 0,
           TRUE ~ NA
         ),
         
         oz_eligible = case_when(
           oz_eligible_mfi == 1 | oz_eligible_pov == 1 ~ "OZ eligible",
           oz_eligible_pov == 0 & oz_eligible_mfi == 0 ~ "OZ ineligible",
           
           oz_eligible_pov == 0 & is.na(oz_eligible_mfi) ~ "OZ ineligible",
           is.na(oz_eligible_pov) & oz_eligible_mfi == 0 ~ "OZ ineligible",
           
           
           TRUE ~ "insufficient information" # this includes tracts that have missing poverty or mfi data
         )) %>%
        
        select(GEOID_tract = GEOID, tract, GEOID_msa, msa, GEOID_st, state,
               population,
               mfi, mfi_relate,
               mfi_90cl_low, mfi_90cl_high, mfi_standard_error, mfi_moe_over_50pct,
               poverty_rte, mfi_ratio, oz_eligible,
               unempl_rte, share_nonwhite, share_no_hs_adult, share_ba_above_adult, prime_age_not_working, vacancy_rate,
               pop_poverty, poverty_univ, unemployed, labor_force, race_white, race_univ,
               edu_no_hs, edu_ba_above, pop_adult, prime_age_unemployed, prime_age_nilf, prime_age_population,
               housing_vacant, housing_vacant_seasonal, housing_total)

table(eligible_ozs$oz_eligible)

# Generate OZ eligibility for U.S. island territories excl. Puerto Rico
island_vars <- load_variables(2020, dataset = "dhcvi")
decennial_islands <- bind_rows(
  read.csv(file.path(path_data, "island census/DECENNIALDHCAS2020.P1-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCGU2020.P1-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCMP2020.P1-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCVI2020.P1-Data.csv"))
) %>%
  filter(substr(GEO_ID, 1, 9) == "1400000US") %>%
  rename(tract = NAME) %>%
  mutate(GEOID_tract = substr(GEO_ID, 10, 20),
         GEOID_st = substr(GEO_ID, 10, 11),
         population = as.numeric(P1_001N)) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  left_join(fips_codes %>% distinct(state, state_code),
            by = join_by("GEOID_st" == "state_code")) %>%
  select(GEOID_tract, tract, GEOID_st, state, population) %>%
  distinct()

decennial_islands_mfi <- bind_rows(
  read.csv(file.path(path_data, "island census/DECENNIALDHCAS2020.PBG63-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCGU2020.PBG63-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCMP2020.PBG63-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCVI2020.PBG63-Data.csv"))
) %>%
  filter(substr(GEO_ID, 1, 9) == "1400000US") %>%
  mutate(GEOID_tract = substr(GEO_ID, 10, 20),
         mfi = as.numeric(PBG63_001N)) %>%
  select(GEOID_tract, mfi) %>%
  distinct()

decennial_islands_mfi_state <- bind_rows(
  read.csv(file.path(path_data, "island census/DECENNIALDHCAS2020.PBG63-State.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCGU2020.PBG63-State.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCMP2020.PBG63-State.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCVI2020.PBG63-State.csv"))
) %>%
  filter(GEO_ID != "Geography") %>%
  mutate(GEOID_st = substr(GEO_ID, 10, 11),
         mfi_relate = as.numeric(PBG63_001N)) %>%
  select(GEOID_st, mfi_relate)

decennial_islands_pov <- bind_rows(
  read.csv(file.path(path_data, "island census/DECENNIALDHCAS2020.PBG73-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCGU2020.PBG73-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCMP2020.PBG73-Data.csv")),
  read.csv(file.path(path_data, "island census/DECENNIALDHCVI2020.PBG73-Data.csv"))
) %>%
  filter(substr(GEO_ID, 1, 9) == "1400000US") %>%
  rename(poverty_univ = PBG73_001N,
         pop_poverty = PBG73_002N) %>%
  mutate(GEOID_tract = substr(GEO_ID, 10, 20),
         poverty_univ = as.numeric(poverty_univ),
         pop_poverty = as.numeric(pop_poverty),
         poverty_rte = pop_poverty/poverty_univ) %>%
  select(GEOID_tract, poverty_rte, poverty_univ, pop_poverty) %>%
  distinct()

decennial_islands <- decennial_islands %>%
  left_join(decennial_islands_mfi, by = "GEOID_tract") %>%
  left_join(decennial_islands_mfi_state, by = "GEOID_st") %>%
  left_join(decennial_islands_pov, by = "GEOID_tract")

eligible_oz_islands <- decennial_islands %>%
  # implement OZ definition
  mutate(mfi_ratio = mfi/mfi_relate,
         oz_eligible_mfi = case_when(mfi_ratio <= 0.7 ~ 1,
                                     mfi_ratio > 0.7 ~ 0,
                                     TRUE ~ NA),
         oz_eligible_pov = case_when(
           poverty_rte < 0.2 ~ 0,
           poverty_rte >= 0.2 & (is.na(mfi) | is.nan(mfi) | mfi == 0) ~ 1, # if no MFI available, eligible based on poverty alone
           poverty_rte >= 0.2 & mfi_ratio <= 1.25 ~ 1,
           poverty_rte >= 0.2 & mfi_ratio > 1.25 ~ 0,
           TRUE ~ NA),
         oz_eligible = case_when(
           oz_eligible_mfi == 1 | oz_eligible_pov == 1 ~ "OZ eligible",
           oz_eligible_pov == 0 & oz_eligible_mfi == 0 ~ "OZ ineligible",
           oz_eligible_pov == 0 & is.na(oz_eligible_mfi) ~ "OZ ineligible",
           is.na(oz_eligible_pov) & oz_eligible_mfi == 0 ~ "OZ ineligible",
           TRUE ~ "insufficient information" # this includes tracts that have missing poverty or mfi data, or 0 population
           )) %>%
  select(GEOID_tract, tract, GEOID_st, state, population,
         mfi, mfi_relate, mfi_ratio, poverty_rte, oz_eligible)

# Add rural classification to OZ eligibility dataset
rural_classification <- read.csv(file.path(path_output,"tracts_rural_classification_24.csv")) %>% 
  mutate(GEOID_tract = stringr::str_pad(GEOID, side = "left", pad = "0", width = 11)) %>%
  select(-c(X, GEOID))

# save master oz eligibility file
setwd(path_output)
oz_24_eligibility_public <- bind_rows(eligible_ozs,
                                      eligible_oz_islands) %>%
  mutate(GEOID_msa = ifelse(is.na(GEOID_msa), "not in an MSA", GEOID_msa),
         msa = ifelse(is.na(msa), "not in an MSA", msa),
         population = case_when(population == 0 | is.na(population) ~ "not available",
                                population != 0 ~ paste0(round(population, 0))),
         mfi = case_when(mfi == 0 | is.na(mfi) ~ "not available",
                         mfi != 0 ~ paste0("$", round(mfi, 0)),
                         TRUE ~ NA),
         mfi_relate = case_when(mfi_relate == 0 | is.na(mfi_relate) ~ "not available",
                                mfi_relate != 0 ~ paste0("$", round(mfi_relate, 0)),
                                TRUE ~ NA),
         mfi_ratio = case_when(mfi_ratio == 0 | is.na(mfi_ratio) ~ "not available",
                               mfi_ratio != 0 ~ as.character(round(mfi_ratio, 4)),
                               TRUE ~ NA),
         poverty_rte = case_when(is.na(poverty_rte) ~ "not available",
                                 !is.na(poverty_rte) ~ paste0(round(poverty_rte * 100, 2), "%"))) %>%
  left_join(rural_classification, by = "GEOID_tract") %>%
  mutate(r_stat = ifelse(is.na(r_stat), "Neither", r_stat)) %>%
  mutate(r_stat_simp = ifelse(r_stat == "Rural", "Rural", "Non-Rural")) %>%
  mutate(state = case_when(state == "AS" ~ "American Samoa",
                           state == "GU" ~ "Guam",
                           state == "MP" ~ "Northern Mariana Islands",
                           state == "VI" ~ "U.S. Virgin Islands",
                           TRUE ~ state)) %>%
  select(GEOID_tract, tract,
         GEOID_msa, msa,
         GEOID_st, state,
         population,
         `Median Family Income` = mfi,
         `Comparable MSA/State MFI` = mfi_relate,
         `Tract to MSA/State MFI Ratio` = mfi_ratio,
         `Poverty Rate` = poverty_rte,
         `OZ Eligbility` = oz_eligible,
         `Rural Classification` = r_stat_simp) %>%
  arrange(GEOID_tract)

writexl::write_xlsx(oz_24_eligibility_public,
                    "Tracts by OZ eligibility 2020-24 ACS.xlsx")
writexl::write_xlsx(eligible_ozs,
                    "tracts_by_OZ_eligibility_24_master.xlsx")

eligible_ozs_by_state <- oz_24_eligibility_public %>%
  group_by(GEOID_st, state) %>%
  summarise(total_tracts = n(),
            num_eligible = sum(as.numeric(`OZ Eligbility` == "OZ eligible"))) %>%
  ungroup() %>%
  mutate(num_expected_designated = case_when(
    ceiling(num_eligible/4) < 25 & num_eligible >= 25 ~ 25,
    num_eligible < 25 ~ num_eligible,
    TRUE ~ ceiling(num_eligible/4)
  ))
sum(eligible_ozs_by_state$num_expected_designated)
sum(eligible_ozs_by_state$num_expected_designated[1:51])

write.csv(eligible_ozs_by_state,
          file.path(path_output, "Number of Eligible and Expected Designated OZs by State.csv"),
          row.names = FALSE)

# Margin of error summary stats
mfi_moe_sum_tables <- bind_rows(
  # What % of all census tracts have MOEs > 50% of the MFI estimate (and what % of the total population lives in such tracts)
  eligible_ozs %>%
    summarise(tract_count = sum(mfi_moe_over_50pct, na.rm = TRUE),
              share_tract_count = tract_count/n(),
              pop_count = sum(as.numeric(mfi_moe_over_50pct) * population, na.rm = TRUE),
              pop_share = pop_count/sum(population, na.rm = TRUE)) %>%
    mutate(statistic = "Tracts >50% MOE in MFI") %>%
    relocate(statistic),
  
  # What % of eligible OZ tracts have MOEs > 50% of the estimate (and what % of the total population lives in such tracts)
  eligible_ozs %>%
    summarise(tract_count = sum(as.numeric(mfi_moe_over_50pct) *
                                  as.numeric(oz_eligible == "OZ eligible"),
                                na.rm = TRUE),
              share_tract_count = tract_count / sum(as.numeric(oz_eligible == "OZ eligible")),
              pop_count = sum(as.numeric(mfi_moe_over_50pct) *
                                    as.numeric(oz_eligible == "OZ eligible") *
                                    population,
                                  na.rm = TRUE),
              pop_share = pop_count / sum(population * as.numeric(oz_eligible == "OZ eligible"), na.rm = TRUE)) %>%
    mutate(statistic = "OZ Eligible Tracts >50% MOE in MFI") %>%
    relocate(statistic),
  
  # How many census tracts that do not meet the eligibility criteria are nevertheless within the margin of error (and how many people live in such tracts)? Meaning, how many tracts with an MFI estimate >70% might be unfairly excluded, since the true value could be within the eligible range?
  eligible_ozs %>%
    mutate(mfi_ratio_90cl_low = mfi_90cl_low/mfi_relate,
           oz_eligible_mfi_90cl_low = case_when(
             mfi_ratio_90cl_low <= 0.7 ~ 1,
             mfi_ratio_90cl_low > 0.7 ~ 0,
             TRUE ~ NA),
           oz_eligible_pov_90cl_low = case_when(
             poverty_rte < 0.2 ~ 0,
             poverty_rte >= 0.2 & is.na(mfi) ~ 1,
             poverty_rte >= 0.2 & mfi_ratio_90cl_low <= 1.25 ~ 1,
             poverty_rte >= 0.2 & mfi_ratio_90cl_low > 1.25 ~ 0,
             TRUE ~ NA
           ),
           oz_eligible_90cl_low = case_when(
             oz_eligible_mfi_90cl_low == 1 | oz_eligible_pov_90cl_low == 1 ~ "OZ eligible",
             oz_eligible_pov_90cl_low == 0 & oz_eligible_mfi_90cl_low == 0 ~ "OZ ineligible",
             
             oz_eligible_pov_90cl_low == 0 & is.na(oz_eligible_mfi_90cl_low) ~ "OZ ineligible",
             is.na(oz_eligible_pov_90cl_low) & oz_eligible_mfi_90cl_low == 0 ~ "OZ ineligible",

             TRUE ~ "insufficient information" # this includes tracts that have missing poverty or mfi data
           )) %>%
    summarise(tract_count = sum(as.numeric(oz_eligible_90cl_low == "OZ eligible") *
                                  as.numeric(oz_eligible == "OZ ineligible"),
                                na.rm = TRUE),
              share_tract_count = tract_count / sum(as.numeric(oz_eligible == "OZ ineligible")),
              pop_count = sum(as.numeric(oz_eligible_90cl_low == "OZ eligible") *
                                    as.numeric(oz_eligible == "OZ ineligible") *
                                    population,
                                  na.rm = TRUE),
              pop_share = pop_count / sum(population * as.numeric(oz_eligible == "OZ ineligible"), na.rm = TRUE)) %>%
    mutate(statistic = "Ineligible Tracts Eligible with Low 90% CI") %>%
    relocate(statistic),
  
  # Then a similar analysis at the top end: How many tracts that qualified on poverty but were disqualified by their high MFI (exceeding 125% of the benchmark) are at risk of unfair disqualification? 
  #   Stated differently, what share of all "disqualified" tracts are at risk of unfair disqualification, if we looked at full MOEs?
  eligible_ozs %>%
    mutate(mfi_ratio_90cl_high = mfi_90cl_high/mfi_relate,
           oz_eligible_mfi_90cl_high = case_when(
             mfi_ratio_90cl_high <= 0.7 ~ 1,
             mfi_ratio_90cl_high > 0.7 ~ 0,
             TRUE ~ NA),
           oz_eligible_pov_90cl_high = case_when(
             poverty_rte < 0.2 ~ 0,
             poverty_rte >= 0.2 & is.na(mfi) ~ 1,
             poverty_rte >= 0.2 & mfi_ratio_90cl_high <= 1.25 ~ 1,
             poverty_rte >= 0.2 & mfi_ratio_90cl_high > 1.25 ~ 0,
             TRUE ~ NA
           ),
           oz_eligible_90cl_high = case_when(
             oz_eligible_mfi_90cl_high == 1 | oz_eligible_pov_90cl_high == 1 ~ "OZ eligible",
             oz_eligible_pov_90cl_high == 0 & oz_eligible_mfi_90cl_high == 0 ~ "OZ ineligible",
             
             oz_eligible_pov_90cl_high == 0 & is.na(oz_eligible_mfi_90cl_high) ~ "OZ ineligible",
             is.na(oz_eligible_pov_90cl_high) & oz_eligible_mfi_90cl_high == 0 ~ "OZ ineligible",
             
             TRUE ~ "insufficient information" # this includes tracts that have missing poverty or mfi data
           )) %>%
    summarise(tract_count = sum(as.numeric(oz_eligible_90cl_high == "OZ ineligible") *
                                  as.numeric(oz_eligible == "OZ eligible"),
                                na.rm = TRUE),
              share_tract_count = tract_count / sum(as.numeric(oz_eligible == "OZ eligible")),
              pop_count = sum(as.numeric(oz_eligible_90cl_high == "OZ ineligible") *
                                    as.numeric(oz_eligible == "OZ eligible") *
                                    population,
                                  na.rm = TRUE),
              pop_share = pop_count / sum(population * as.numeric(oz_eligible == "OZ eligible"), na.rm = TRUE)) %>%
    mutate(statistic = "Eligible Tracts Ineligible with High 90% CI") %>%
    relocate(statistic)
)

write.csv(mfi_moe_sum_tables,
          file = file.path(path_output, "Tract MFI MOE Summary Table.csv"),
          row.names = FALSE)

# add in shape information
tracts = tracts(cb = TRUE, year = 2024)

ozs_sf = tracts %>%
  left_join(eligible_ozs, by = c("GEOID" = "GEOID_tract"))

# fix the dataset format changed by Jason
ozs_sf_wrangled = ozs_sf %>%
  mutate(mfi = case_when(
    mfi == 0 ~ "not available",
    is.na(mfi) ~ "not available",
    mfi != 0 ~ paste0("$", round(mfi, 0)),
    TRUE ~ NA
  ),
  mfi_ratio = case_when(
    mfi_ratio == 0 ~ "not available",
    is.na(mfi_ratio) ~ "not available",
    mfi_ratio !=0 ~ as.character(round(mfi_ratio, 2)),
    TRUE ~ NA
  ),
  poverty_rte = case_when(
    is.na(poverty_rte) ~ "not available",
    !is.na(poverty_rte) ~ paste0(round(poverty_rte*100, 2), "%")
  ),
  unempl_rte = case_when(
    is.na(unempl_rte) ~ "not available",
    !is.na(unempl_rte) ~ paste0(round(unempl_rte*100, 2), "%")
  ),
  msa = ifelse(is.na(msa), "not in an MSA", msa),
  population = case_when(
    population == 0 ~ "not available",
    is.na(population) ~ "not available",
    population != 0 ~ paste0(round(population, 0))
  )) %>%
  select(STATEFP, COUNTYFP, TRACTCE, GEOID,
         NAME, NAMELSAD, STUSPS, NAMELSADCO, STATE_NAME, LSAD,
         ALAND, AWATER, tract, GEOID_msa, msa, GEOID_st, state,
         population, mfi, poverty_rte, mfi_ratio, oz_eligible,
         unempl_rte, r_stat, r_stat_simp, geometry)

ozs_sf_write <- st_make_valid(ozs_sf_wrangled)  # ensure validity
ozs_sf_write <- st_transform(ozs_sf_write, 4326)

setwd(path_output)
dir.create("ozs_shape_24")
setwd(file.path(path_output, "ozs_shape_24"))
st_write(ozs_sf_write, "ozs_shape_24.shp")

setwd(path_output)
dir.create("oz_shape_2024_kml")
setwd(file.path(path_output, "ozs_shape_24"))
st_write(ozs_sf_write, file.path("oz_shape_2024_kml","ozs_shape_24.kml"), driver = "KML")

# important factors to list -- that Treasury must report
# page 417 - 418
# unemployment rate
# persons working in the population census tract
# individual, family, household poverty rates
# median family income
# demographic info: age, income, education, race, employment
# average % of income of population census tract spent on rent annually
# # of residences in population census tract
# home ownership rate
# average value of residentail property
# number of affordable housing units
# number of new busines starts in ceneus tract
# dist of employees by NAICS code
