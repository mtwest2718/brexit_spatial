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
data_loc <- '/home/mtwest2718/Documents/research/brexit_spatial/data_sets'
# self-perception as English &/or British
identity <- english_identity(data_loc)
#
age <- age_cohorts(data_loc)
#
qualifications <- quals_proportions(data_loc)
head(qualifications)
