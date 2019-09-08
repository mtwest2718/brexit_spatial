library(readxl)
library(dplyr)

referendum_votes <- function(data_loc) {
    fn.votes <- paste(
        data_loc, 'election',
        'EU-referendum-results-and-characteristics-data.xlsx', sep='/'
    )
    
    # Only want vote counts from English local authorities
    votes <- read_excel(fn.votes, sheet='Results') %>%
        select(Area_Code, Area, Electorate, Remain, Leave) %>%
        rename(lad16cd = Area_Code, lad16nm = Area)
    
    return(votes)
}
