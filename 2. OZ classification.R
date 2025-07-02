
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
  "sarah" = "/Users/sarah/Documents/GitHub/oz_eligibility_2025"
)

current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_output <- file.path(path_project, "output")

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

# variable list:
tract_vars = c(
  # poverty
  "poverty_univ" = "B17001B_001", # population universe
  "pop_poverty" = "B17001B_002", # population under poverty line
  
  # mfi
  "mfi" = "B19113_001", # median family income
  
  # population
  "population" = "B01003_001",
  
  # unemployment
  "labor_force" = "B23025_003",
  "unemployed" = "B23025_005"
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

tracts_list[[st]] = tract_pull
}


census_tracts = bind_rows(tracts_list) %>%
  select(-moe) %>% rename(tract = NAME) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(poverty_rte = 100*pop_poverty/poverty_univ,
         unempl_rte = 100*unemployed/labor_force) %>%
  select(-c(pop_poverty, poverty_univ, unemployed, labor_force))


# clean up
rm(tracts_list, tract_pull, st)


# pull in MSA data
census_msa = get_acs(
  geography = "cbsa",
  variables = msa_vars,
  year = 2023,
  survey = "acs5",
  geometry = FALSE
) %>%
  
  # filter out micro areas
  filter(str_detect(NAME, "Metro Area")) %>%
  select(-c(moe)) %>%
  rename(`msa` = NAME) %>%
  pivot_wider(names_from = variable, values_from = estimate)
  

# pull in State data
census_st = get_acs(
  geography = "state",
  variables = state_vars,
  year = 2023,
  survey = "acs5",
  geometry = FALSE
) %>%
  select(-c(moe)) %>% rename(state = NAME) %>%
  pivot_wider(names_from = variable, values_from = estimate)


######################################
# generate tract - to - msa crossalk #
######################################

options(tigris_use_cache = TRUE)

# Download CBSA (MSA + Micropolitan areas)
cbsa <- core_based_statistical_areas(cb = TRUE, year = 2020)

# Filter for just Metropolitan Statistical Areas (MSA)
msa <- cbsa %>%
  filter(LSAD == "M1")

tracts = tracts(cb = TRUE, year = 2020)

tracts_with_msa = st_join(tracts, msa, join = st_within) # fully within a MSA

tracts_msa_xwalk = tracts_with_msa %>%
  st_drop_geometry() %>%
  select(GEOID_tract = GEOID.x,
         GEOID_msa = GEOID.y
         ) %>% na.omit()

# Check results -- should have about 83% of tracts in MSAs
      total_tracts <- nrow(tracts)
      tracts_in_msa <- sum(!is.na(tracts_with_msa$GEOID.y))
      tracts_not_in_msa <- total_tracts - tracts_in_msa
      
      # Output
      cat("Total tracts:", total_tracts, "\n")
      cat("Tracts in MSAs:", tracts_in_msa, "\n")
      cat("Tracts NOT in MSAs:", tracts_not_in_msa, "\n")
      cat("Share of tracts in MSAs:", round(100*tracts_in_msa/total_tracts,2), "\n")
      

#########################
# Generate Eligible OZs # 
#########################

eligible_ozs = census_tracts %>%
  
  # add in crosswalks
  left_join(tracts_msa_xwalk, by = c("GEOID" = "GEOID_tract")) %>%
  mutate(GEOID_st = substr(GEOID, 1, 2)) %>%
  
  # add in state MFI and MSA MFI (if available)
  left_join(census_msa, by = c("GEOID_msa" = "GEOID")) %>%
  left_join(census_st, by = c("GEOID_st" = "GEOID")) %>%
  
  # extract the correct geography to match MFI to
  mutate(mfi_relate = case_when(
    !is.na(msa_mfi) ~ msa_mfi,
    is.na(msa_mfi) ~ st_mfi
    ),
    mfi_ratio = 100*mfi/mfi_relate
  ) %>%
        
  # implement OZ definition
  mutate(oz_eligible_mfi = case_when(
            mfi_ratio <= 70 ~ 1,
            mfi_ratio > 70 ~ 0,
            TRUE ~ NA
            ),
      
         oz_eligible_pov = case_when(
           poverty_rte < 20 ~ 0,
           poverty_rte >= 20 & is.na(mfi) ~ 1, # if no MFI available, eligible based on poverty alone
           poverty_rte >= 20 & mfi_ratio <=125 ~ 1,
           poverty_rte >= 20 & mfi_ratio >125 ~ 0,
           TRUE ~ NA
         ),
         
         oz_eligible = case_when(
           oz_eligible_mfi == 1 | oz_eligible_pov == 1 ~ "OZ eligible",
           oz_eligible_pov == 0 & oz_eligible_mfi == 0 ~ "OZ ineligible",
           TRUE ~ "insufficient information" # this includes tracts that have missing poverty or mfi data
         )) %>%
        
        select(GEOID_tract = GEOID, tract, GEOID_msa, msa, GEOID_st, state,
               population, mfi, unempl_rte, poverty_rte, mfi_ratio, 
               oz_eligible)
      
      
table(eligible_ozs$oz_eligible)
    
# add in rural classification

rural_classification = read.csv(file.path(path_output,"tracts_rural_classification.csv")) %>% 
  mutate(GEOID_tract = str_pad(GEOID, side = "left", pad = "0", width = 11)) %>%
  select(-c(X, GEOID))

eligible_ozs = eligible_ozs %>%
  left_join(rural_classification, by = "GEOID_tract")


# save master oz eligibility file
setwd(path_output)
writexl::write_xlsx(eligible_ozs, "tracts_by_OZ_eligibility.xlsx")

# add in shape information
ozs_sf = tracts %>%
  left_join(eligible_ozs, by = c("GEOID" = "GEOID_tract")) %>%
  select(-c(tract, GEOID_msa, msa, GEOID_st, state))

ozs_sf <- st_make_valid(ozs_sf)  # ensure validity
ozs_sf <- st_transform(ozs_sf, 4326)


setwd(path_output)
dir.create("ozs_shapefiles")
setwd(file.path(path_output, "ozs_shapefiles"))
st_write(ozs_sf, "ozs_shape.shp")



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



