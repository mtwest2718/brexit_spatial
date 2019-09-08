library(readr)
library(dplyr)

lad_codes <- function() {
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
