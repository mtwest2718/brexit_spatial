####----------------------------------------------------------------------####
#                                 Preamble                                   #
####----------------------------------------------------------------------####

## Load libraries
package_names <- c('purrr', 'dplyr', 'stringr')
for (pkg in package_names) {
    library(pkg, character.only=TRUE, quietly=TRUE, verbose=FALSE)
}

## Set working path for this script by its current location
rel_path <- getSrcDirectory(function(x) {x})
path <- ifelse(
    rel_path != '',
    paste(normalizePath(rel_path), 'r_scripts', sep='/'),
    dirname(rstudioapi::getActiveDocumentContext()$path)
)
setwd(path)

## Reference functions
source('ons_codes.r')
source('elections.r')
source('demographics.r')
source('economics.r')
source('spatial_neighbors.r')

# Where the data files are located
data_loc <- '/home/mtwest/Documents/brexit_spatial/data_sets'

####----------------------------------------------------------------------####
#                       Load covariate data from files                       #
####----------------------------------------------------------------------####

## English LAD Codes ------------------------------------------------------ ##
english_lads <- lad_codes(data_loc) %>%
    filter(str_detect(LAD17CD, '^E0')) %>%
    rename(ladcd = LAD17CD, ladnm = LAD17NM)

## Referendum Vote -------------------------------------------------------- ##
votes <- referendum_votes(data_loc)  %>%
    select(-c(lad16nm)) %>%
    rename(ladcd = lad16cd)

## Spatial Neighbors ------------------------------------------------------ ##
lad.counts <- lad_commuters_2011(data_loc)
# compute percentage flow rates and filter list
nb.pcts <- reduce_neighbors(lad.counts)
# percentages for commuters who work in the district they reside
local_worker_pct <- nb.pcts %>%
    filter(cd.work == cd.reside) %>%
    rename(ladcd = cd.work) %>%
    select(ladcd, r_pct, w_pct)

# construct spatial objects with binary weights via spdep
nb.obj <- define_neighbors(nb.pcts, symm=TRUE, w_type='pct', thresh=0.015)
# Get boundary polygons for english districts
english_bnds <- boundary_polygons(data_loc) %>%
    filter(str_detect(lad16cd, '^E0')) %>%
    select(-c(lad16nm)) %>%
    rename(ladcd = lad16cd)

## Economics at LAD Level ------------------------------------------------- ##
# Benefit cuts in pounds per year per person
benefit_cuts <- welfare_cuts(data_loc) %>%
    rename(ladcd = lad15cd)
# Cuts in local authority spending per year per person
spending_cuts <- council_cuts(data_loc) %>%
    select(-c(LAD17NM)) %>%
    rename(ladcd = LAD17CD)
# Change in disposable income at LAD level
disp_income <- disposable_income_lm(data_loc)

# LAD Avg Deprivation Stats
depriv_stats <- deprivation_avgs(data_loc) %>% rename(ladcd = lad13cd)

# % of pop that own's their home, privately rents or socially rents
housing <- housing_tenure(data_loc) %>% rename(ladcd = lad11cd)

# Change in the unemployment figures for each district
unemployment <- unemployment_lm(data_loc)
# Change in % of population in a particular industry during Great Recession
industries <- recession_job_loss(data_loc)

## Demographics at LAD level ---------------------------------------------- ##
# % of pop in voting-age brackets: 18-29, 30-44, 45-64, 65+
age <- age_cohorts(data_loc) %>%
    select(-c(lad16nm)) %>%
    rename(ladcd = lad16cd)

# % of the working-age pop with a given highest level of formal qualifications
qualifications <- quals_proportions(data_loc) %>%
    select(-c(lad15nm)) %>%
    rename(ladcd = lad15cd)

# % of British nationals that identify as English &/or British
englishness <- english_identity(data_loc) %>% rename(ladcd = lad15cd)

# % of population that are EU & non-EU immigrants and change in group size
immigrants <- immigrant_demography_lm(data_loc) %>% select(-c(ladnm))
# % of population that are British born and either white or bame, as well
#   as change in group size.
ethnicity <- ethnic_demography_lm(data_loc) %>% select(-c(ladnm))

####----------------------------------------------------------------------####
#                    Combine data into a single structure                    #
####----------------------------------------------------------------------####
data <- list(
    english_lads, votes, local_worker_pct, benefit_cuts, depriv_stats,
    spending_cuts, housing, unemployment, age, qualifications, englishness,
    immigrants, ethnicity, disp_income, industries,
    english_bnds
) %>%
    purrr::reduce(inner_join, by='ladcd') %>%
    mutate(
        voter_density = Electorate/(st_areasha/1e6),
        welfare_loss_vs_Income = welfare_loss_2016_APP/Disposable_Income_2016,
        LAD_EPP_vs_Income = Authority_EPP_2016/Disposable_Income_2016
    ) %>%
    select(-c(welfare_loss_2016_APP, Authority_EPP_2016))

####----------------------------------------------------------------------####
#                           Linear model components                          #
####----------------------------------------------------------------------####

# Response and Design matrix
data.XY <- data[ , c(9:89,99:101)]
# Centering and scaling the design matrix for easier variable comparison
data.xy <- data.XY
data.xy[ ,-c(1:3)] <- data.xy[ ,-c(1:3)] %>% scale(center=TRUE, scale=TRUE)

# simple logistic model for voter turnout
model.glm <- glm(
    data=data.xy, family='binomial',
    formula=cbind(Leave+Remain, Electorate-Leave-Remain) ~ .
)
leverage <- hatvalues(model.glm)
