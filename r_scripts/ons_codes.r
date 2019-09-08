library(readr)
library(dplyr)

# ONS codes for Local Authorities and higher level administrative areas
lad_codes <- function(data_loc) {
    fn.ons <- paste(
        data_loc, 'ons_codes',
        'Ward_to_Local_Authority_District_to_County_to_Region_to_Country_December_2017_Lookup_in_United_Kingdom_version_2.csv',
        sep='/'
    )
    
    # Filter out unwanted Ward information
    lad_codes <- read_csv(fn.ons) %>%
        select(matches('^[^WF]')) %>%
        distinct() %>%
        arrange(GOR10NM, CTY17NM, LAD17NM)
    
    return(lad_codes)
}

# Get best fit matching ONS codes linking MSOA11CD to LAD17CD
msoa_to_lad <- function(data_loc) {
    fn.ons <- paste(
        data_loc, 'ons_codes',
        'Middle_Layer_Super_Output_Area_2011_to_Ward_2017_Lookup_in_England_and_Wales.csv',
        sep='/'
    )
    # Filter out unwanted Ward information and keep only codes
    msoa_codes <- read_csv(fn.ons) %>%
        select(matches('^[^WF]+CD$'))
    
    return(msoa_codes)
}

