library(readr)
library(readxl)
library(stringr)
library(dplyr)

source('ons_codes.r')

quals_proportions <- function(data_loc){
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
    results <- df %>% select(ladcd) %>%
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
        rename(ladcd = `Area Code10`, ladnm = `Area Name`) %>%
        mutate(ladnm = str_remove(ladnm, '\\d+$')) %>%
        select(-matches('^\\.{3}\\d+')) %>%
        filter(!is.na(ladnm))

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
        select(matches('^(ladcd|All_|(non)?EU_)')) %>%
        filter(All_ci != ':') %>%
        mutate_if(is.character, ~str_replace(., '^[\\.c]', 'NA')) %>%
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
    lad_codes <- lad_codes(data_loc) %>%
        select(LAD17CD, LAD17NM) %>%
        rename(ladcd = LAD17CD, ladnm = LAD17NM)

    # storage for all years
    pop <- lad_codes
    # Unpack population files from gzipped tarball
    dir.temp <- paste(data_loc, 'temp', sep='/')
    fn.pop_tar <- paste(
        data_loc, 'demographics',
        'pop-count_birth-nationality_ew.tar.gz',sep='/'
    )
    untar(fn.pop_tar, exdir=dir.temp)

    years <- as.character(2005:2015)
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
        pop <- pop %>% inner_join(pop.year, by='ladcd')
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
        col_regex <- paste('^ladcd', g, sep='|')
        pop.group <- pop %>% select(matches(col_regex))
        # regression results per LAD
        reg.group <- time_series_lm(pop.group, rs=1e-3)

        ## Add group id tag to results columns
        tag <- str_remove_all(g, '[_^]')
        colnames(reg.group)[-1] <- paste(names(reg.group)[-1], tag, sep='_')

        reg.results <- reg.results %>% inner_join(reg.group, by='ladcd')
    }

    ## Computing all the summary statistics to be used as covariates
    results.covar <- reg.results %>%
        transmute(
            ladcd = ladcd, ladnm = ladnm,
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

####----------------------------------------------------------------------####
#                Change in domestic English Racial Demography                #
####----------------------------------------------------------------------####

## Load and clean a population subgroup timeseries file
process_subgroup <- function(fn) {
    # Load english districts except Isle of Scilly & City of London
    people <- read_csv(fn, skip=7, n_max=327) %>%
        rename(ladcd = mnemonic) %>%
        filter(
            str_detect(ladcd, '^E0') &
            !str_detect(ladcd, '^E0(9000001|6000053)')
        )

    years <- sort( unique(str_extract(names(people), '(\\d{4})')) )
    ## Fill all the character placeholders in table cells
    for (i in 1:length(years)) {
        col_idx <- (-1+4*i):(2+4*i)

        sngl.year <- people[ ,col_idx]
        colnames(sngl.year) <- c('count', 'total', 'prop', 'ci')
        # work through sequence of columns in a single year
        sngl.year <- sngl.year %>%
            mutate(
                prop = str_replace(prop, '^[!~*#-]', '0.2'),
                ci = str_replace(ci, '^[!~*#-]', prop)
            ) %>%
            mutate(
                ci = str_replace(ci, '100', '1')
            ) %>%
            mutate_all(., ~str_replace(., '^[!~*#-]', 'NA')) %>%
            type_convert() %>%
            mutate(ci = ci/100 * total)

        empty.count <- is.na(sngl.year$count)
        if (sum(empty.count) > 0) {
            ## fill in the gaps in counts based on rounded proportions
            sngl.year[empty.count, 1] <- sngl.year[empty.count, ] %>%
                transmute(count = total*prop/100)
        }
        ## return year values to full data.frame
        people[ ,col_idx] <- sngl.year
    }

    # Drop unnecessary columns
    people <- people %>%
        select(matches('^(lad|Numer|Conf)'))
    # rename population columns
    colnames(people)[-1] <- paste(
        rep(c('subgroup_est','subgroup_ci'), times=length(years)),
        rep(years, each=2),
        sep='_'
    )
    return(people)
}

ethnic_demography_lm <- function(data_loc) {
    ## Starting with ONS codes for LADs, dropping 'The City' and Scilly
    # Starting with ONS codes for LADs
    lad_codes <- lad_codes(data_loc) %>%
        select(LAD17CD, LAD17NM) %>%
        rename(ladcd = LAD17CD, ladnm = LAD17NM) %>%
        filter(!str_detect(ladcd, '^E0(9000001|6000053)'))

    ## Load in all ethnicity/uk-national data
    subgroup <- paste(
        rep(c('white', 'bame'), times=2),
        rep(c('uk', 'non_uk'), each=2), sep='_'
    )
    # temp storage for combined counts across categories
    all_grp <- data.frame()

    reg.results <- lad_codes
    # looping over files
    for (sg in subgroup) {
        fn.sub <- paste(
            data_loc, 'demographics',
            paste('nomis', sg,'0515.csv', sep='_'), sep='/'
        )
        sub_pop <- process_subgroup(fn.sub)

        # Only adding UK-born results immediately into table
        if (endsWith(sg, 'e_uk')) {
            if (sg == 'white_uk') {
                results <- time_series_lm(sub_pop, rs=1e-3)
                colnames(results)[-1] <- paste(
                    names(results)[-1], 'wUK', sep='_'
                )
            } else {
                results <- time_series_lm(sub_pop, rs=1e-2)
                colnames(results)[-1] <- paste(
                    names(results)[-1], 'bameUK', sep='_'
                )
            }
            reg.results <- reg.results %>% inner_join(results, by='ladcd')
        }

        # Combining counts for all residents in England
        if (dim(all_grp)[1] == 0) {
            all_grp <- sub_pop
        } else {
            # sum of estimates
            not_est <- seq(1,23,2)
            all_grp[,-c(not_est)] <- all_grp[,-c(not_est)] + sub_pop[,-c(not_est)]
            # quadrature some of CIs
            not_ci <- c(1, seq(2,23,2))
            all_grp[ ,-c(not_ci)] <- sqrt(
                all_grp[ ,-c(not_ci)]^2 + sub_pop[ ,-c(not_ci)]^2
            )
        }
    }

    ## Running regression for all residents
    results <- time_series_lm(all_grp, rs=1e-3)
    colnames(results)[-1] <- paste(names(results)[-1], 'all', sep='_')
    reg.results <- reg.results %>% inner_join(results, by='ladcd')

    ## Computing all the summary statistics to be used as covariates
    results.covar <- reg.results %>%
        transmute(
            ladcd = ladcd, ladnm = ladnm,
            PopPct_wUK_first = c_first_wUK/c_first_all,
            PopPct_bameUK_first = c_first_bameUK/c_first_all,
            PopPct_wUK_last = c_last_wUK/c_last_all,
            PopPct_bameUK_last = c_last_bameUK/c_last_all,
            Pop_wUK_Change_first = m_wUK/c_first_wUK,
            Pop_bameUK_Change_first = m_bameUK/c_first_bameUK,
            Pop_Change_first = m_all/c_first_all
        ) %>%
        mutate(
            PopPct_wUK_Change = PopPct_wUK_first *
                (Pop_wUK_Change_first - Pop_Change_first),
            PopPct_bameUK_Change = PopPct_bameUK_first *
                (Pop_bameUK_Change_first - Pop_Change_first)
        ) %>%
        select(-c(PopPct_wUK_first, PopPct_bameUK_first, Pop_Change_first)) %>%
        mutate_if(is.numeric, round, digits=5)

    years <- sort( unique(str_extract(names(all_grp), '(\\d{4})')) )
    # match first/last with appropriate year in column names
    colnames(results.covar) <- str_replace_all(
        names(results.covar),
        c('first' = years[1], 'last' = tail(years, n=1))
    )

    return(results.covar)
}
