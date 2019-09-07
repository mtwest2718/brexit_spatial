
library(readxl)
library(purrr)

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
names(depriv) <- c(
    'lsoa11cd', 'lad13cd', 'leave_before_gcse', 'life_lost_indicator', 
    'disability', 'acute_morbidity', 'mental_health', 'dist_to_market', 
    'dist_to_gp', 'overcrowding', 'homelessness', 'housing_affordability', 
    'poor_housing', 'crime_score', 'total_pop', 'children'
)
depriv <- depriv %>% 
    inner_join(sum_pop, by='lad13cd')

depriv.lad <- depriv %>% 
    mutate_at(vars(c('leave_before_gcse')), ~(. * children/sum_child)) %>%
    mutate_at(vars(names(depriv)[4:14]), ~(. * total_pop/sum_total)) %>%
    select(-c(total_pop, children, sum_total, sum_child, lsoa11cd)) %>%
    group_by(lad13cd) %>%
    summarise_if(is.numeric, sum)
