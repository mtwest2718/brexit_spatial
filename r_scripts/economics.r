library(readxl)
library(purrr)
library(dplyr)
library(stringr)

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
    fn.scores <- paste(
        data_loc, 'economic',
        'File_5_ID_2015_Scores_for_the_Indices_of_Deprivation.xlsx', sep='/'
    )
    scores <- read_excel(fn.scores, sheet='ID2015 Scores', n_max=32850) %>%
        select(matches('code|^(Crime|Income) Score'))

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
    depriv <- list(edu, health, barriers, living_env, scores, pop) %>%
        purrr::reduce(
            inner_join, by=c('LSOA code (2011)', 'Local Authority District code (2013)')
        )
    # renaming columns with clearer labels
    names(depriv) <- c(
        'lsoa11cd', 'lad13cd', 'leave_before_gcse', 'life_lost_indicator',
        'disability', 'acute_morbidity', 'mental_health', 'dist_to_market',
        'dist_to_gp', 'overcrowding', 'homelessness', 'housing_affordability',
        'poor_housing', 'crime_score', 'income_score', 'total_pop', 'children'
    )

    ## What portion of a district lives in an LSOA in the worst quartile
    quartile <- depriv %>% select(-matches('^lad|total|child|homeless'))
    for (i in 2:dim(quartile)[2]) {
        qrt1 <- sort(as.matrix(quartile[,i]), decreasing=TRUE)[dim(depriv)[1]/4]
        quartile[,i] <- ifelse(quartile[,i] >= qrt1, 1, 0)
    }
    # append quartile-1 to column name
    colnames(quartile)[-1] <- paste(
        names(quartile)[-1], rep('qrt1', dim(quartile)[2]-1), sep='_'
    )

    # summary stats for each Local Authority
    depriv.lad <- depriv %>%
        inner_join(quartile, by='lsoa11cd') %>%
        inner_join(sum_pop, by='lad13cd') %>%
        mutate_at(
            vars(matches('^leave_before_gcse')), ~(. * children/sum_child)
        ) %>%
        mutate_at(
            vars(-matches('cd$|total|child|gcse')),
            ~(. * total_pop/sum_total)
        ) %>%
        select(-c(total_pop, children, sum_total, sum_child, lsoa11cd)) %>%
        group_by(lad13cd) %>%
        summarise_if(is.numeric, sum)

    return(depriv.lad)
}

welfare_cuts <- function(data_loc) {
    ## Just keeping the "Total Estimated Annual Loss" due to Tory welfare
    #   "reforms" through 2016
    fn.welfare <- paste(
        data_loc, 'economic',
        'Welfare_Reform_Beatty-Fothergill_2016.xlsx', sep='/'
    )
    cuts <- read_excel(
        fn.welfare, sheet='DATABASE PRE 2015 REFORMS', n_max=400, skip=1
    ) %>%
        select(matches('^\\.{3}1|^Financial .+35$'))
    # rename columns
    colnames(cuts) <- c('lad15cd', 'welfare_loss_2016_APP')

    return(cuts)
}

## Percentage of population that owns their home
housing_tenure <- function(data_loc) {
    fn.home <- paste(
        data_loc, 'economic', 'r21ewrttableks402ewladv2_tcm77-290717.xls', sep='/'
    )
    # get both columns of home ownership (with and without mortage)
    tenure <- read_excel(
        fn.home, sheet='KS402EW_Percentages', range='A11:L465'
    ) %>%
        select(matches('Area code|^(Owned|Social|Private)')) %>%
        filter(!is.na(`Area code`)) %>%
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
    cty <- lad_codes %>% inner_join(cuts, by=c('CTY17NM' = 'LANM'))
    lad <- lad_codes %>% inner_join(cuts, by=c('LAD17NM' = 'LANM'))

    ## Append total population numbers for 2009 and 2016 for authorities
    # Unpack population files from gzipped tarball
    dir.temp <- paste(data_loc, 'temp', sep='/')
    fn.pop_tar <- paste(
        data_loc, 'demographics',
        'pop-count_birth-nationality_ew.tar.gz',sep='/'
    )
    untar(fn.pop_tar, exdir=dir.temp)
    # Select only two years from the list
    years <- c('2009', '2016')
    for (y in years) {
        fn.pop <- paste(
            data_loc, 'temp',
            paste0('pop-count_birth-nationality_year', y, '_EW.xls'),
            sep='/'
        )
        pop <- get_yearly_img(fn.pop) %>%
            rename(CD = ladcd) %>%
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
        rename(ladcd = LA_Code, ladnm = LA_Name) %>%
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
            ladcd = ladcd,
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

####----------------------------------------------------------------------####
#                           Changes in Median Income                         #
####----------------------------------------------------------------------####

## Yearly median income & tax data
process_yearly_inc <- function(fn, codes, cpih, costs=FALSE) {
    yearly <- read_csv(fn) %>%
        select(matches('^(area|median)_?')) %>%
        filter(!is.na(`area_name`) & str_detect(`median`, '\\d')) %>%
        type_convert()

    # Compute the half-width of the disposable income confidence intervals
    disposable <- yearly %>%
        mutate(
            income_est = median - median_tax,
            income_ci = sqrt(
                (median_high_ci-median_low_ci)^2/4 +
                (median_tax_high_ci-median_tax_low_ci)^2/4
            ),
            lc_name = tolower(area_name)
        ) %>%
        dplyr::select(matches('_name$|^(income|tax)(_ci)?'))

    ## Compute the regional discretionary income if Costs-of-Living are given
    if (any(costs != FALSE)) {
        income <- codes %>%
            mutate(lc_name = tolower(GOR10NM)) %>%
            full_join(disposable, by='lc_name') %>%
            inner_join(costs, by='GOR10CD') %>%
            mutate(income_est = income_est/52 - weekly_col) %>%
            mutate_if(is.numeric, round, digits=2) %>%
            select(matches('^LAD|income_est|weekly_col'))
    } else {
        # otherwise just stick with LAD level disposable income
        disposable <- disposable %>%
            mutate(lc_name = str_remove_all(
                lc_name, '\\.| ua$|( metropolitan)? county| towns')
            ) %>%
            mutate(lc_name = str_replace_all(lc_name, '-', ' ')) %>%
            mutate(
                lc_name = str_replace(lc_name, '(south buck)inghamshire', '\\1s')
            )
        # NOTE: more manipulation of the district names so they match up with they
        #   official ones from the ONS. South Buckinghamshire is officialy called
        #   'South Bucks'.

        ## Match median income and tax estimates with correct ONS LAD codes
        income <- codes %>%
            inner_join(disposable, by=c("lc_name"))
    }

    ## Apply CPIH scaling
    income <- income %>% mutate_if(is.numeric, funs(. / cpih))

    return(income)
}

indexed_april_cpih <- function(data_loc) {
    ## load consumer price index (sans housing) CPIH
    fn.cpih <- paste(
        data_loc, 'economic', 'cpih-cumm-series_20190609_MM23_L522.csv', sep='/'
    )
    cpih <- read_csv(fn.cpih, skip=6) %>%
        filter(str_detect(month, '^201[1-6] APR$'))
    # set baseline value to be April 2016
    cpih$cpih_value <- cpih$cpih_value/tail(cpih$cpih_value, n=1)

    return(cpih)
}

disposable_income_lm <- function(data_loc) {
    # NOTE: Changing the names to lower case, as well as the manipulation of
    #   individual characters and district prefixes is because I have to match
    #   districts by name and not code. The names have just enough differences
    #   so that exact matching will miss a number of districts.
    lad_codes <- lad_codes(data_loc) %>%
        select(LAD17CD, LAD17NM) %>%
        mutate(lc_name = tolower(LAD17NM)) %>%
        mutate(lc_name = str_remove_all(
            lc_name, ", (city|county) of|\\'|\\.")
        ) %>%
        mutate(lc_name = str_replace_all(lc_name, '-', ' '))

    ## load consumer price index (with housing) CPIH
    cpih <- indexed_april_cpih(data_loc)

    # storage structure for all years
    income.all_yrs <- lad_codes
    ## Looping from 2010-2011 through 2015-2016 tax years
    # NOTE: End of tax year is in April in the UK.
    years <- seq(11,16)
    for (i in 1:length(years)) {
        fn.inc <- paste(
            data_loc, 'economic',
            paste0('NS_Table_3_14a_', years[i]-1, years[i], '_total_inc.csv'),
            sep='/'
        )
        income.yrly <- process_yearly_inc(
            fn.inc, lad_codes, cpih$cpih_value[i]
        )

        # mark income columns with tax year
        colnames(income.yrly)[5:6] <- paste(
            names(income.yrly)[5:6], rep(years[i], 2), sep='_'
        )

        # append to existing storage
        income.all_yrs <- income.all_yrs %>%
            inner_join(income.yrly, by=c('LAD17CD', 'LAD17NM'))  %>%
            select(matches('^(LAD|income)'))
    }

    #------------------------------------------------------------------------#
    ## Linear Regression for each LAD's income time series
    results.lm <- time_series_lm(
        income.all_yrs %>% rename(ladcd = LAD17CD),
        rs=1e-4
    )
    # the values that will be used as covariates in logit regression later
    results.covar <- results.lm %>%
        transmute(
            ladcd = ladcd,
            Disposable_Income_last = c_last,
            Disposable_Income_RelChange_first = m / c_first
        ) %>%
        mutate_if(is.numeric, round, digits=4)

    # match first/last with appropriate year in column names
    colnames(results.covar) <- str_replace_all(
        names(results.covar),
        c(
            'first' = paste0('20', years[1]),
            'last' = paste0('20', tail(years, n=1))
        )
    )

    return(results.covar)
}

fulltime_income_lm <- function(lad_codes) {
    fn.fulltime <- paste(
        data_loc, 'economic',
        'uk_fulltime_median_gross_income_1116.csv', sep='/'
    )
    fulltime <- read_csv(fn.fulltime, skip=9, n_max=326) %>%
        filter(str_detect(mnemonic, '^E')) %>%
        mutate_all(., ~str_replace(., '^[#!-]$', 'NA')) %>%
        type_convert() %>%
        filter(!is.na(`2011`))

    # rename columns
    years <- names(fulltime)[seq(3, dim(fulltime)[2], 2)]
    colnames(fulltime) <- c(
        'ladnm', 'ladcd',
        paste(
            rep(c('income_est', 'income_ci'), length(years)),
            rep(years, each=2), sep='_'
        )
    )
    # NOTE: some of the districts are counties rather than local authorities.
    # Just easier to keep the naming convention and make sure its fixed later

    cpih <- indexed_april_cpih()
    for (j in 1:dim(cpih)[1]) {
        # Convert confidence interval from % to pounds
        fulltime[,2+2*j] <- (fulltime[,2+2*j]/100)*fulltime[,1+2*j]
        # Adjust the income values for CPIH
        fulltime[,1:2+2*j] <- fulltime[,1:2+2*j] / cpih$cpih_value[j]
    }

    ## Run simple linear time series regression for each LAD
    results.lm <- time_series_lm(fulltime, rs=1e-2)
    results.covar <- results.lm %>%
        transmute(
            CD = ladcd,
            Fulltime_Income_RelChange_first = m / c_first
        ) %>%
        mutate_if(is.numeric, round, digits=4)

    # match first/last with appropriate year in column names
    colnames(results.covar) <- str_replace_all(
        names(results.covar),
        c('first'=years[1], 'last'=tail(years, n=1))
    )

    # Connect regression results to the appropriate LAD or CTY code
    results.lad <- lad_codes %>%
        inner_join(results.covar, by=c('LAD17CD'='CD'))
    results.cty <- lad_codes %>%
        inner_join(results.covar, by=c('CTY17CD'='CD'))
    # combine values back together
    results <- bind_rows(results.lad, results.cty) %>%
        select(matches('LAD17CD|Fulltime_Income')) %>%
        rename(ladcd = LAD17CD)

    return(results)
}

recession_job_loss <- function(data_loc) {
    # Get local authority and county codes to match data with proper locations
    lad_codes <- lad_codes(data_loc) %>%
        select(LAD17CD, CTY17CD)
    
    # names of different industry collections based on SIC 2007 standard
    sic <- c(
        'agriculture_fish', 'energy_water', 'manufacturing', 'construction', 
        'distr_hotel_restaurant', 'transport_comms', 'banking_insurance', 
        'admin_edu_health', 'other_services'
    )
    
    fn.ind <- paste(
        data_loc, 'economic',
        'nomis_2007-11_industry_percentage.xlsx', sep='/'
    )
    # getting region codes from first sheet
    jobs <- read_excel(fn.ind, skip=7, sheet='Sheet 0') %>%
        rename(NM = `...1`, CD = `...2`) %>%
        select(CD, NM)
    
    ## Looping over industries
    for (s in 1:length(sic)) {
        sheet <- paste0('Sheet ', s-1)
        # Load % of workforce from 2007 & 2011 for particular industry
        industry <- read_excel(fn.ind, skip=7, sheet=sheet) %>% 
            rename(NM = `...1`, CD = `...2`) %>%
            select(matches('^(CD|NM|percent)')) %>%
            filter(str_detect(CD, '^E'))
        colnames(industry)[3:4] <- c('pct_2007', 'pct_2011')
        
        # Replace missing entries with base sample level of 0.2%
        industry <- industry %>% 
            mutate_all(., ~str_replace(., '^[#!~-]$', '0.2')) %>%
            type.convert() %>%
            mutate(pct_change = pct_2011 - pct_2007) %>%
            select(CD, NM, pct_2007, pct_change) %>%
            mutate_if(is.factor, as.character)
        colnames(industry)[3:4] <- str_replace(names(industry)[3:4], '^pct', sic[s])
            
        jobs <- jobs %>% inner_join(industry, by=c('CD', 'NM'))
    }
    
    # Connect job percentages to LAD/CTY codes
    lad <- lad_codes %>% inner_join(jobs, by=c('LAD17CD'='CD'))
    cty <- lad_codes %>% inner_join(jobs, by=c('CTY17CD'='CD'))
    # combine tables back together
    job.pct <- bind_rows(lad, cty) %>%
        filter(!str_detect(LAD17CD, 'E0(9000001|6000053)')) %>%
        select(-c(CTY17CD, NM)) %>% 
        rename(ladcd = LAD17CD)
    # NOTE: dropping 'City of London' and 'Isle of Scilly' here
        
    return(job.pct)
}
