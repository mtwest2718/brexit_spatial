library(readr)
library(readxl)
library(stringr)
library(dplyr)

quals_proportions <- function(data_loc){
    # specify sub-directory of desired data
    sub_dir <-

    fn.quals <- paste(
        data_loc, 'demographics', 'gb_2015gcse_counts.csv', sep='/'
    )
    # Only want the computed percentages in each category
    quals <- read_csv(fn.quals, skip=7, n_max=381) %>%
        select(matches('^[lm\\%]')) %>%
        mutate_all(., ~str_replace(., '^[!-]', 'NA')) %>%
        type_convert() %>%
        mutate_if(is.numeric, funs(. / 100))
    # Again, only want the English districts

    # Rename the columns according to their equivalent RQF numbers (need cite)
    colnames(quals) <- c(
        'lad15nm', 'lad15cd',
        'RQF_5Plus', 'RQF_4', 'RQF_3', 'RQF_2', 'RQF_other', 'RQF_entry'
    )

    # Drop the 'City of London' and 'Isle of Scilly' rows as they are
    #   mostly & completely empty (respectively)
    quals <- quals %>%
        filter(!str_detect(lad15nm, 'London$|Scilly$'))

    # Filling in the NA's with approximate values based on other entries
    quals$missing <- pmax(0, 1 - apply(quals[,-c(1:2)], 1, sum, na.rm=TRUE))
    quals <- quals %>%
        mutate_all( funs(ifelse(is.na(.), missing, .)) ) %>%
        select(-c(missing))
    # NOTE: I know there is only one NA per row where one occurs. Might be
    #   worth including some random noise in estimates.

    return(quals)
}

#----------------------------------------------------------------------------#
## Load counts of people in each year bracket within each LAD
age_cohorts <- function(data_loc){
    fn.age <- paste(
        data_loc, 'demographics', 'ukmidyearestimates2016.xls', sep='/'
    )
    # Again only want values for English LADs (dropping N.I., Wales & Scotland)
    age <- read_excel(fn.age, sheet='MYE2 - All', skip=4) %>%
        filter(str_detect(Code, '^E0')) %>%
        rename(lad16cd = Code, lad16nm = Name, Total = `All ages`)

    # Change names of age columns so they can be processed with tibble function
    colnames(age) <- str_replace(names(age), '^(\\d+)$', 'a\\1')

    age.sum <- age %>% dplyr::select(lad16cd, lad16nm, Total)
    ## Break into cohorts
    age.sum$a18_29 <- apply(age %>% select(num_range('a',18:29)), 1, sum)
    age.sum$a30_44 <- apply(age %>% select(num_range('a',30:44)), 1, sum)
    age.sum$a45_64 <- apply(age %>% select(num_range('a',45:64)), 1, sum)
    age.sum$a65_90p <- apply(age %>% select(num_range('a',65:90)), 1, sum)

    # Standardize cohorts so they are % of total LAD population
    age.pct <- age.sum %>%
        mutate_at(vars(matches('^a\\d+_\\d+')), ~(./Total)) %>%
        select(-c(Total)) %>%
        mutate_if(is.numeric, signif, digits=4)

    return(age.pct)
}

#----------------------------------------------------------------------------#
## National identity: more British or English?
english_identity <- function(data_loc){
    ## NOTE: the question in the APS/LFS surveys is multiple select
    fn.nat <- paste(
        data_loc, 'demographics', 'nomis_national_identity_june2016.csv', sep='/'
    )
    # Only want the computed percentages in each category
    nation <- read_csv(fn.nat, skip=7, n_max=381) %>%
        rename(lad15cd = mnemonic, Total = Denominator) %>%
        filter(str_detect(lad15cd, '^E0')) %>%
        select(matches('^(lad|Total|%)')) %>%
        mutate_all(., ~str_replace(., '^[!-]', 'NA')) %>%
        type_convert()
    # Rename columns according to the country/area
    colnames(nation) <- str_replace(
        names(nation), '^\\% [\\w ]+ (\\w+)$', '\\1'
    )
    # Focusing on the dichotomy of British and English identification
    England <- nation %>%
        select(lad15cd, Total, British, English) %>%
        filter(!is.na(Total)) %>%
        select(-c(Total)) %>%
        mutate_if(is.numeric, funs(. / 100))

    ## Need to rescale these percentages by the number of UK nationals/citizens
    fn.pop <- paste(
        data_loc, 'demographics', 'nomis_ethnicity_nationality_jun2016.csv', sep='/'
    )
    # Only want the computed percentages in each category of UK nationals
    pop.uk <- read_csv(fn.pop, skip=7, n_max=381) %>%
        rename(lad15cd = mnemonic, Total = Denominator) %>%
        filter(str_detect(lad15cd, '^E0')) %>%
        mutate_all(., ~str_replace(., '^[!#~-]', 'NA')) %>%
        type_convert() %>%
        select(
            matches('^lad15cd|(white|ethnic minority) UK national$')
        )
    # simplify column names
    colnames(pop.uk) <- str_replace(
        names(pop.uk), '^P[\\w ]+ are ([\\w ]+) UK national$', '\\1'
    )
    # percentage that is a UK national
    pop.uk <- pop.uk %>%
        filter(lad15cd != 'E09000001' & lad15cd != 'E06000053') %>%
        rename(bame = `ethnic minority`) %>%
        mutate(bame = if_else(is.na(bame), 0.2, bame)) %>%
        mutate(uk_nat = (white + bame)/100)
    ## NOTE: removing City of London & Isle of Scilly for lack of clear data.

    ## Rescale the identity measures by the proportion that is a UK national
    englishness <- England %>%
        inner_join(pop.uk, by='lad15cd') %>%
        mutate_at(vars(c(British, English)), ~(. / uk_nat)) %>%
        select(lad15cd, British, English) %>%
        mutate_if(is.numeric, round, digits=4)

    return(englishness)
}

#----------------------------------------------------------------------------#
## Run linear regression using R's LM on time series for each LAD
time_series_lm <- function(df, rs=1e-3) {
    results <- df %>% select(LAD17CD) %>%
        mutate(c_first = NA, c_last = NA, m = NA)

    # split count estimates from uncertainty
    est <- df %>% select(matches('_est_'))
    ci <- df %>% select(matches('_ci_'))

    n_lads <- dim(est)[1]
    Last <- dim(est)[2]

    for (k in 1:n_lads) {
        # year IDs starting from 2005
        lad_data <- data.frame(
            counts = t(est[k,]) * rs,
            se = t(ci[k,]/qnorm(0.975)) * rs,
            time = 0:(Last-1)
        )
        ## fit linear model
        model <- lm(data=lad_data, formula = counts ~ time, weights=1/se^2)
        # record rate of change
        results[k,4] <- model$coefficients[2] / rs
        # also record the first and last data points
        results[k, 2:3] <- c(lad_data$counts[1], lad_data$counts[Last]) / rs
    }
    return(results)
}


####----------------------------------------------------------------------####
#                  Change in English Immigration Demography                  #
####----------------------------------------------------------------------####
get_yearly_img <- function(fn) {
    # Only keeping English LADs
    people <- read_excel(fn, sheet='1.1', range="A9:W383") %>%
        rename(LAD17CD = `Area Code10`, LAD17NM = `Area Name`) %>%
        mutate(LAD17NM = str_remove(LAD17NM, '\\d+$')) %>%
        select(-c(X__1, X__2, X__3)) %>%
        filter(!is.na(LAD17NM))

    # Rename columns
    loc = c(
        'All', 'UK', 'nonUK', 'EU', 'EU14', 'EU8', 'EU2', 'EUother', 'nonEU'
    )
    colnames(people)[-c(1:2)] <- paste(
        rep(loc, each=2), rep(c('est', 'ci'), times=length(loc)), sep='_'
    )

    ## Remove the UK & EU subset columns
    # NOTE ':' occur in the City of London and Isle of Scilly rows
    people <- people %>%
        select(matches('^(LAD17CD|All_|(non)?EU_)')) %>%
        filter(All_ci != ':') %>%
        mutate_if(is.character, funs(str_replace(., '^[\\.c]', 'NA'))) %>%
        type_convert()

    ## Fill in empty cells with a low estimate (< 1) based on total population
    total <- people$All_est
    for (k in seq(2, dim(people)[2], 2)) {
        # which rows in k'th column have NA
        empty.row <- is.na(people[,k])

        if (any(empty.row)) {
            # ballparking empty cell for estimate as 0.2% of total population
            people[empty.row, k] <- 0.002 * total[empty.row]
            # making the confidence interval the same size as the estimate
            people[empty.row, k+1] <- people[empty.row, k]
        }
    }
    return(people)
}

immigrant_demography_lm <- function(data_loc) {
    # Starting with ONS codes for LADs
    lad_codes <- lad_codes(data_loc) %>% select(LAD17CD, LAD17NM)

    # storage for all years
    pop <- lad_codes
    years <- as.character(2005:2015)
    # Unpack population files from gzipped tarball
    dir.temp <- paste(data_loc, 'temp', sep='/')
    fn.pop_tar <- paste(
        data_loc, 'demographics',
        'pop-count_birth-nationality_ew.tar.gz',sep='/'
    )
    untar(fn.full, exdir=dir.temp)

    ## Loop over yearly immigration count files
    for (i in 1:length(years)) {
        fn.people <- paste(
            data_loc, 'temp',
            paste0('pop-count_birth-nationality_year', years[i], '_EW.xls'),
            sep='/'
        )
        pop.year <- get_yearly_img(fn.people)
        # include year on column names
        colnames(pop.year)[-1] <- paste(
            names(pop.year)[-1], rep(i, times=(dim(pop.year)[2]-1)),
            sep='_'
        )
        # append year's columns to existing storage tibble
        pop <- pop %>% inner_join(pop.year, by='LAD17CD')
    }
    # remove files from temp folder
    file.remove(
        list.files(dir.temp, full.names=TRUE, pattern='^pop-count_')
    )

    #------------------------------------------------------------------------#
    reg.results <- lad_codes

    groups <- c('^All_', '^EU_', '^nonEU_')
    ## Separate counts into immigrant groups and run individual regressions
    for (g in groups) {
        # which columns one needs to keep to run the regressions for a group
        col_regex <- paste('^LAD17CD', g, sep='|')
        pop.group <- pop %>% select(matches(col_regex))
        # regression results per LAD
        reg.group <- time_series_lm(pop.group, rs=1e-3)

        ## Add group id tag to results columns
        tag <- str_remove_all(g, '[_^]')
        colnames(reg.group)[-1] <- paste(names(reg.group)[-1], tag, sep='_')

        reg.results <- reg.results %>% inner_join(reg.group, by='LAD17CD')
    }

    ## Computing all the summary statistics to be used as covariates
    results.covar <- reg.results %>%
        transmute(
            LAD17CD = LAD17CD, LAD17NM = LAD17NM,
            PopPct_EU_first = c_first_EU/c_first_All,
            PopPct_nonEU_first = c_first_nonEU/c_first_All,
            PopPct_EU_last = c_last_EU/c_last_All,
            PopPct_nonEU_last = c_last_nonEU/c_last_All,
            Pop_EU_Change_first = m_EU/c_first_EU,
            Pop_nonEU_Change_first = m_nonEU/c_first_nonEU,
            Pop_Change_first = m_All/c_first_All
        ) %>%
        mutate(
            PopPct_EU_Change = PopPct_EU_first *
                (Pop_EU_Change_first - Pop_Change_first),
            PopPct_nonEU_Change = PopPct_nonEU_first *
                (Pop_nonEU_Change_first - Pop_Change_first)
        ) %>%
        select(-c(PopPct_EU_first, PopPct_nonEU_first, Pop_Change_first)) %>%
        mutate_if(is.numeric, round, digits=5)

    # match first/last with appropriate year in column names
    colnames(results.covar) <- str_replace_all(
        names(results.covar),
        c('first' = years[1], 'last' = tail(years, n=1))
    )

    return(results.covar)
}
