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

council_cuts <- function(data_loc) {
    ## Adjust some of the local authority names so they match up properly
    lad_codes <- lad_codes(data_loc) %>%
        mutate(LAD17NM = str_remove_all(LAD17NM, '[,.]|\\s?C(ity|ounty)( of)?\\s?'))

    fn.budget <- paste(
        data_loc, "economic", 'Sheet_1_LASpendCuts-1.xlsx', sep='/'
    )
    # Getting real budget expenditure numbers from IFS report
    cuts <- read_excel(
        fn.budget, sheet='England - cuts by area', range='B4:F156'
    ) %>%
        rename(LANM = `Local authority`) %>%
        select(matches('^(LANM|Total|Grant)')) %>%
        mutate(LANM = str_replace(LANM, '\\&', 'and')) %>%
        mutate(LANM = str_remove_all(
            LANM, '\\s?UA$|[,.]|\\s?(City|County)( of)?\\s?')
        ) %>%
        mutate(LANM = str_replace(LANM, 'The (Medway) Towns', '\\1')) %>%
        mutate(LANM = str_replace(
            LANM, '(Telford and) the( Wrekin)', '\\1\\2')
        ) %>%
        mutate(LANM = str_replace(LANM, '(Middlesb)o(rough)', '\\1\\2'))
    ## NOTE: Really wish researchers would just be consistent and use ONS codes
    colnames(cuts)[-1] <- c('Expenditure_2016', 'Expenditure_2009',
                            'Grant_Dependence')

    # Some of the budget numbers are for aggregate shire counties
    cty <- lad_codes %>%
        inner_join(cuts, by=c('CTY17NM' = 'LANM'))
    lad <- lad_codes %>%
        inner_join(cuts, by=c('LAD17NM' = 'LANM'))

    ## Append total population numbers for 2009 and 2016 for authorities
    # Unpack population files from gzipped tarball
    dir.temp <- paste(data_loc, 'temp', sep='/')
    fn.pop_tar <- paste(
        data_loc, 'demographics',
        'pop-count_birth-nationality_ew.tar.gz',sep='/'
    )
    untar(fn.full, exdir=dir.temp)
    # Select only two years from the list
    years <- c('2009', '2016')
    for (y in years) {
        fn.pop <- paste(
            data_loc, 'temp',
            paste0('pop-count_birth-nationality_year', y, '_EW.xls'),
            sep='/'
        )
        pop <- get_yearly_img(fn.pop) %>%
            rename(CD = LAD17CD) %>%
            select(matches('^(CD|All_est)'))
        colnames(pop)[2] <- paste('total_pop', y, sep='_')
        # match population estimates to local authority codes
        lad <- lad %>% inner_join(pop, by=c('LAD17CD'='CD'))
        cty <- cty %>% inner_join(pop, by=c('CTY17CD'='CD'))
    }
    # remove files from temp folder
    file.remove(
        list.files(dir.temp, full.names=TRUE, pattern='^pop-count_')
    )

    # EPP: Expenditure Per Person (in that year)
    cuts.per_capita <- bind_rows(lad, cty) %>%
        select(matches('^(LAD|Expenditure|total_pop|Grant)')) %>%
        mutate(
            Authority_EPP_2009 = Expenditure_2009/total_pop_2009,
            Authority_EPP_2016 = Expenditure_2016/total_pop_2016
        ) %>%
        mutate(
            Authority_EPP_Change = Authority_EPP_2016/Authority_EPP_2009 - 1
        ) %>%
        mutate_if(is.numeric, round, digits=3) %>%
        select(
            LAD17CD, LAD17NM,
            Authority_EPP_2016, Authority_EPP_Change, Grant_Dependence
        )

    return(cuts.per_capita)
}

unemployment_lm <- function(data_loc) {
    fn.lad <- paste(
        data_loc, 'economic', 'model_unemployment_10-16.csv', sep='/'
    )
    # Districts with no data
    rates <- read_csv(fn.lad, skip=7) %>%
        select(-c(Jul09_Jun10, Conf_0910)) %>%
        rename(lad15cd = LA_Code, lad15nm = LA_Name) %>%
        filter(Jul10_Jun11 != '-') %>%
        mutate_all(., ~replace(., .=='-', NA)) %>%
        type_convert()

    # NA values in CI's come from council districts, whose estimates are listed
    #   as exact.
    for (i in seq(4, dim(rates)[2], 2)) {
        r <- rates[, i]
        # Need uncertainty estimate so am using the smallest value across all
        #   districts for that year.
        min_ci <- min(r, na.rm=TRUE)
        rates[, i] <- replace(r, is.na(r), min_ci)
    }

    # Rename data columns so they are usable in <time_series_lm>
    colnames(rates) <- str_replace(
        names(rates), '^Jul\\d+_Jun(\\d+)$', 'rate_est_20\\1'
    )
    colnames(rates) <- str_replace(
        names(rates), '^Conf_\\d{2}(\\d{2})', 'rate_ci_20\\1'
    )

    ## Run normal linear regressoin for each unemployment time series
    results.lm <- time_series_lm(rates, rs=1e-1)
    # the values that will be used as covariates in logit regression later
    results.covar <- results.lm %>%
        transmute(
            lad15cd = lad15cd,
            Unemployment_last = c_last,
            Unemployment_Change = m
        ) %>%
        mutate_if(is.numeric, round, digits=4)

    # match first/last with appropriate year in column names
    years <- unique(
        str_replace(names(rates)[-c(1:2)], '^r\\w+_(\\d+)$', '\\1')
    )
    colnames(results.covar) <- str_replace_all(
        names(results.covar), c('first' = years[1], 'last'=tail(years, n=1))
    )

    return(results.covar)
}
