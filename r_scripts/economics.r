library(readxl)
library(purrr)

source('ons_codes.r')
source('demographics.r')

deprivation_avgs <- function(data_loc) {
    ## Stats underlying many deprivation scores
    fn.stats <- paste(
        data_loc, 'economic', 'File_8_ID_2015_Underlying_indicators.xlsx', sep='/'
    )
    edu <- read_excel(fn.stats, sheet='ID 2015 Education Domain', n_max=32850) %>%
        select(matches('code|^Staying'))
    health <- read_excel(fn.stats, sheet='ID 2015 Health Domain', n_max=32850) %>%
        select(-matches('name'))
    barriers <- read_excel(
        fn.stats, sheet='ID 2015 Barriers Domain', n_max=32850
    ) %>%
        select(-matches('name|post office|primary school'))
    living_env <- read_excel(
        fn.stats, sheet='ID 2015 Living Env Domain', n_max=32850
    ) %>%
        select(matches('code|poor condition'))

    ## Crime score
    fn.crime <- paste(
        data_loc, 'economic',
        'File_5_ID_2015_Scores_for_the_Indices_of_Deprivation.xlsx', sep='/'
    )
    crime <- read_excel(fn.crime, sheet='ID2015 Scores', n_max=32850) %>%
        select(matches('code|Crime'))

    ## Population values at the LSOA level
    fn.pop <- paste(
        data_loc, 'economic', 'File_6_ID_2015_Population_denominators.xlsx', sep='/'
    )
    pop <- read_excel(fn.pop, sheet='Population Denominators', n_max=32850) %>%
        select(matches('code|^Total|^Dependent'))
    sum_pop <- pop %>%
        rename(lad13cd = `Local Authority District code (2013)`) %>%
        group_by(lad13cd) %>%
        summarise(
            sum_total = sum(`Total population: mid 2012 (excluding prisoners)`),
            sum_child = sum(`Dependent Children aged 0-15: mid 2012 (excluding prisoners)`)
        )

    ## Putting everything together
    depriv <- list(edu, health, barriers, living_env, crime, pop) %>%
        purrr::reduce(
            inner_join, by=c('LSOA code (2011)', 'Local Authority District code (2013)')
        )
    # renaming columns with clearer labels
    names(depriv) <- c(
        'lsoa11cd', 'lad13cd', 'leave_before_gcse', 'life_lost_indicator',
        'disability', 'acute_morbidity', 'mental_health', 'dist_to_market',
        'dist_to_gp', 'overcrowding', 'homelessness', 'housing_affordability',
        'poor_housing', 'crime_score', 'total_pop', 'children'
    )
    depriv <- depriv %>% inner_join(sum_pop, by='lad13cd')

    # summary stats for each Local Authority
    depriv.lad <- depriv %>%
        mutate_at(vars(c('leave_before_gcse')), ~(. * children/sum_child)) %>%
        mutate_at(vars(names(depriv)[4:14]), ~(. * total_pop/sum_total)) %>%
        select(-c(total_pop, children, sum_total, sum_child, lsoa11cd)) %>%
        group_by(lad13cd) %>%
        summarise_if(is.numeric, sum)

    return(depriv.lad)
}

welfare_cuts <- function(data_loc) {
    ## Just keeping the "Total Estimated Annual Loss" due to Tory welfare
    #   "reforms" through 2016
    fn.welfare <- paste(data_loc, 'economic',
        'Welfare_Reform_Beatty-Fothergill_2016.xlsx', sep='/'
    )
    cuts <- read_excel(
        fn.welfare, sheet='DATABASE PRE 2015 REFORMS', n_max=400, skip=1
    ) %>%
        select(matches('^X__1|^Financial .+__10$'))
    # rename columns
    colnames(cuts) <- c('lad15cd', 'welfare_loss_2016_APP')

    return(loss)
}

## Percentage of population that owns their home
housing_tenure <- function(data_loc) {
    fn.home <- paste(
        data_loc, 'economic', 'r21ewrttableks402ewladv2_tcm77-290717.xls', sep='/'
    )
    # get both columns of home ownership (with and without mortage)
    tenure <- read_excel(
        fn.home, sheet='KS402EW_Percentages', range='A11:G465'
    ) %>%
        select(matches('Area code|^(Owned|Social|Private)')) %>%
        type_convert()

    colnames(tenure) <- c(
        'lad11cd', 'owned_outright', 'owned_mortage', 'social_rent_la',
        'social_rent_other', 'private_rent', 'private_rent_other'
    )
    ## Want % of home ownership, irrespective of debt status
    home <- tenure %>%
        transmute(
            lad11cd = lad11cd,
            home_owner_pct = (owned_outright + owned_mortage)/100,
            social_renter_pct = (social_rent_la + social_rent_other)/100,
            private_renter_pct = (private_rent + private_rent_other)/100
        )
    return(home)
}
