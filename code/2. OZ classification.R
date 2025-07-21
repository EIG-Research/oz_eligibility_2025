
################################################################################
# Description: Eligible OZs, as in the 2025 Big Beautiful Bill
    # Bill text can be found here: https://www.congress.gov/bill/119th-congress/house-bill/1/text
    # see pages 390 - 420
    # definitions are on pages 392 - 393
# Author: Sarah Eckhardt (sarah@eig.org)
# Last updated: July 2nd, 2025
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
  "name" = "PATH TO DIRECTORY",
  "sarah" = "/Users/sarah/Documents/GitHub/oz_eligibility_2025",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/projects/oz_eligibility_2025"
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
acs_vars <- load_variables(2023, "acs5", cache = TRUE)

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

census_api_key("f0a4d766bde27f14627892a18bdc610ab945336d", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")

# pull in tract data
tracts_list = list()
for(st in c(state.abb, "DC", "PR")){
tract_pull = get_acs(
  geography = "tract",
  variables = tract_vars,
  year = 2023,
  survey = "acs5",
  state = st,
  geometry = FALSE
)

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

# clean up
rm(tracts_list, tract_pull, st)

# pull in MSA data
census_msa <- get_acs(
  geography = "cbsa",
  variables = msa_vars,
  year = 2023,
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
  year = 2023,
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
               population, mfi, poverty_rte, mfi_ratio, oz_eligible, unempl_rte,
               share_nonwhite, share_no_hs_adult, share_ba_above_adult, prime_age_not_working, vacancy_rate,
               pop_poverty, poverty_univ, unemployed, labor_force, race_white, race_univ,
               edu_no_hs, edu_ba_above, pop_adult, prime_age_unemployed, prime_age_nilf, prime_age_population,
               housing_vacant, housing_vacant_seasonal, housing_total)

table(eligible_ozs$oz_eligible)

# add in rural classification

ct_tract_xwalk <- read.csv(file.path(path_data, "2022tractcrosswalk.csv")) %>%
  select(tract_fips_2020, tract_fips_2022 = Tract_fips_2022) %>%
  mutate(tract_fips_2020 = stringr::str_pad(tract_fips_2020, side = "left", pad = "0", width = 11),
         tract_fips_2022 = stringr::str_pad(tract_fips_2022, side = "left", pad = "0", width = 11))

rural_classification <- read.csv(file.path(path_output,"tracts_rural_classification.csv")) %>% 
  mutate(GEOID_tract = stringr::str_pad(GEOID, side = "left", pad = "0", width = 11)) %>%
  select(-c(X, GEOID)) %>%
  left_join(ct_tract_xwalk, by = c("GEOID_tract" = "tract_fips_2020")) %>%
  mutate(GEOID_tract = ifelse(!is.na(tract_fips_2022), tract_fips_2022, GEOID_tract)) %>% select(-tract_fips_2022)
  

eligible_ozs <- eligible_ozs %>%
  left_join(rural_classification, by = "GEOID_tract") %>%
  mutate(r_stat = ifelse(is.na(r_stat), "Neither", r_stat)) %>%
  mutate(r_stat_simp = ifelse(r_stat == "Rural", "Rural", "Non-Rural"))

# save master oz eligibility file
setwd(path_output)
writexl::write_xlsx(eligible_ozs, "tracts_by_OZ_eligibility.xlsx")

# add in shape information
tracts = tracts(cb = TRUE, year = 2020)

ozs_sf = tracts %>%
  left_join(ct_tract_xwalk, by = c("GEOID" = "tract_fips_2020")) %>%
  mutate(GEOID = ifelse(!is.na(tract_fips_2022), tract_fips_2022, GEOID)) %>% select(-tract_fips_2022) %>%
  left_join(eligible_ozs, by = c("GEOID" = "GEOID_tract"))


# fix the dataset format changed by Jason
ozs_sf_wrangled = ozs_sf %>%
  mutate(mfi = case_when(
    mfi == 0 ~ "not available",
    is.na(mfi) ~ "not available",
    mfi !=0 ~ paste0("$", round(mfi,0)),
    TRUE ~ NA
  ),
  mfi_ratio = case_when(
    mfi_ratio == 0 ~ "not available",
    is.na(mfi_ratio) ~ "not available",
    mfi_ratio !=0 ~ as.character(round(100*mfi_ratio,2)),
    TRUE ~ NA
  ),
  poverty_rte = round(poverty_rte*100,2),
  unempl_rte = round(unempl_rte*100,2),
  msa = ifelse(is.na(msa), "not in an MSA", msa)
  ) %>% select(STATEFP, COUNTYFP, TRACTCE, AFFGEOID, GEOID,
    NAME, NAMELSAD, STUSPS, NAMELSADCO, STATE_NAME, LSAD,
    ALAND, AWATER, tract, GEOID_msa, msa, GEOID_st, state,
    population, mfi, poverty_rte, mfi_ratio, oz_eligible,
    unempl_rte, r_stat, r_stat_simp, geometry)


ozs_sf_write <- st_make_valid(ozs_sf_wrangled)  # ensure validity
ozs_sf_write <- st_transform(ozs_sf_write, 4326)

setwd(path_output)
dir.create("ozs_shape")
setwd(file.path(path_output, "ozs_shape"))
st_write(ozs_sf_write, "ozs_shape.shp")

# important factors to list -- that secetarty must report
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
