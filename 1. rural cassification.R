

################################################################################
# Description: Eligible OZs, as in the 2025 Big Beautiful Bill
# Bill text can be found here: https://www.congress.gov/bill/119th-congress/house-bill/1/text
# OZ bill text are on pages 390 - 420
# Rural definitions are on page 398
# Author: Sarah Eckhardt (sarah@eig.org)
# Last updated: July 2nd, 2025
################################################################################

# Rural:
# any area other than:
# city or town that has a population of >50,000
# AND
# any urbanized area contiguous and adjacent to a city or town (population > 50,000)


# remove dependencies
rm(list = ls())

# load required packages
library(dplyr)
library(tidyr)
library(purrr)
library(tidycensus) # for ACS data
library(sf) # for shapefile construction
library(tigris) # for tract shapefiles

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
path_data <- file.path(path_project, "data")


##############################
# 1. census tracts file

# Get all state FIPS codes (excluding territories)
state_fips <- unique(fips_codes$state)[1:51]
state_fips <- c(state_fips, "PR")

# Download and bind all tracts for each state
tracts_all <- map_dfr(state_fips, ~ tracts(state = .x, year = 2020))

# filter out water tracts
tracts_all = tracts_all %>%
  filter(ALAND > 0)


########################  
# 2. census places 

# Download place data for all states and combine
places_all <- map_dfr(state_fips, ~ places(state = .x, year = 2020, class = "sf"))

# pull in place population
census_api_key("f0a4d766bde27f14627892a18bdc610ab945336d", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")

places_pop <- get_decennial(geography = "place", 
                            variables = "P1_001N", 
                            year = 2020, 
                            output = "wide") %>%
  rename(POPULATION = P1_001N) %>%
  select(GEOID, POPULATION)

places_all = places_all %>%
  left_join(places_pop, by = "GEOID")

# identify places with a population of < 50,000
urban_threshold = 50000

places_above_50k = places_all %>%
  filter(POPULATION > urban_threshold)

rm(places_all, places_pop)



########################## 
# 3. grab urbanized areas

# from CENSUS tigris files directly
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php  

urban_areas = st_read(file.path(path_data, "tl_2024_us_uac20/tl_2024_us_uac20.shp"))



#######################################################################################    
# 4. combine urban_areas and places_above_50k to construct geometry of all urban areas

# this identifies urban areas that intersect places with >50k population
# Note that this is agnostic as to whether there are more than one place with >50k 
# that intersect the same urban area.

# ensure that the CRS coding matches
st_crs(urban_areas) == st_crs(places_above_50k)

# find UAs that intersect cities >50k
# ""**T***T**"": restricts geometries to have overlapping interiors (true area intersection)
# excludes tracts that only share a border

uas_near_cities <- st_filter(urban_areas, places_above_50k,
                             .predicate = st_intersects)


###################################################################################
# 5. identify which tracts have any intersection with urban areas, and tracts that
# have no intersection with urban areas


# (A) Identify fully rural tracts

    # no city intersections
    tracts_non_cities <- tracts_all %>%
      filter(lengths(st_intersects(., places_above_50k)) == 0)
    
    # no UA intersections
    tracts_non_uas <- tracts_all %>%
      filter(lengths(st_intersects(., uas_near_cities)) == 0)
  
  
    tracts_rural <- inner_join(
      tracts_non_cities,
      st_drop_geometry(tracts_non_uas),
      by = names(tracts_non_cities)[1:length(names(tracts_non_cities))-1]
    )


# (B) Identify fully urban tracts        

    # within a city with >50k persons
    tracts_in_cities <- st_filter(tracts_all, places_above_50k,
                                  .predicate = st_within)
    
    # within a contiguous urban area
    tracts_in_uas <- st_filter(tracts_all, uas_near_cities,
                               .predicate = st_within)
    
    tracts_urban <- dplyr::bind_rows(tracts_in_cities, tracts_in_uas) %>%
      dplyr::distinct(GEOID, .keep_all = TRUE)


    # check that these are non-intersecting sets
    nrow(error <- inner_join(
      tracts_rural,
      st_drop_geometry(tracts_urban),
      by = "GEOID"
    ))

# (C) The remainder are semi-rural

tracts_neither <- tracts_all %>%
  anti_join(st_drop_geometry(tracts_rural), by = names(tracts_non_cities)[1:length(names(tracts_non_cities))-1]) %>%
  anti_join(st_drop_geometry(tracts_urban), by = names(tracts_non_cities)[1:length(names(tracts_non_cities))-1])


    # check that this classification completely covers all tracts
    nrow(tracts_neither) + nrow(tracts_urban) + nrow(tracts_rural) == nrow(tracts_all)
    nrow(tracts_neither) 
    nrow(tracts_urban) 
    nrow(tracts_rural)


# (D) add identifiers    
    
# to comply with the strictness of the rural definition, the overlapping tracts 
# are categorized as neither
tracts_neither = tracts_neither %>%
  mutate(r_stat = "Neither")

tracts_urban = tracts_urban %>%
  mutate(r_stat = "Urban")

tracts_rural = tracts_rural %>%
  mutate(r_stat = "Rural")

# combine all tracts
tracts_catagorized = bind_rows(tracts_neither, tracts_urban, tracts_rural)

###################################################################################
# 6. save output

tracts_catagorized <- tracts_catagorized %>% relocate(geometry, .after = last_col())
tracts_catagorized <- st_make_valid(tracts_catagorized)  # ensure validity
tracts_catagorized <- st_cast(tracts_catagorized, "MULTIPOLYGON", warn = FALSE)  # enforce uniform type


# combine and save tracts
setwd(path_output)
dir.create("tracts_rural_cssification")
setwd(file.path(path_output, "tracts_rural_cssification"))
st_write(tracts_catagorized, "tracts_rural_classif.shp")


# save df version
tracts_catagorized_df = tracts_catagorized %>%
  st_drop_geometry() %>%
  select(GEOID, r_stat)

setwd(path_output)
write.csv(tracts_catagorized_df, "tracts_rural_classification.csv")

